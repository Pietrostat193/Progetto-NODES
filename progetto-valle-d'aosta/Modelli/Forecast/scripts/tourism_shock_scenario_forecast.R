suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(slider)
  library(lme4)
  library(lmerTest)
  library(mgcv)
  library(prophet)
  library(ggplot2)
})

set.seed(20260604)

script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE)),
  error = function(e) getwd()
)
if (!nzchar(script_dir) || is.na(script_dir)) script_dir <- getwd()

forecast_root <- NA_character_
candidate_root <- c(
  file.path(getwd(), "Modelli", "Forecast"),
  normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE),
  script_dir,
  getwd()
)

for (p in candidate_root) {
  if (dir.exists(p) && basename(p) == "Forecast" && dir.exists(file.path(p, "scripts"))) {
    forecast_root <- p
    break
  }
}
for (p in candidate_root) {
  if (is.na(forecast_root) && file.exists(file.path(p, "README.md")) && dir.exists(file.path(p, "scripts"))) {
    forecast_root <- p
    break
  }
}
if (is.na(forecast_root)) stop("Unable to resolve Forecast root directory")

results_intermediate <- file.path(forecast_root, "results", "intermediate")
results_scenario <- file.path(forecast_root, "results", "scenario_forecast")
bootstrap_out_dir <- file.path(results_scenario, "bootstrap_samples")
town_plot_dir <- file.path(results_scenario, "town_plots_95")

dir.create(results_scenario, recursive = TRUE, showWarnings = FALSE)
dir.create(bootstrap_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(town_plot_dir, recursive = TRUE, showWarnings = FALSE)

assignment_candidates <- c(
  file.path(results_intermediate, "model_selection", "town_model_assignment_mape_le20_2025.csv"),
  file.path(forecast_root, "archive", "municipality_plots_legacy_20260604", "town_model_assignment_mape_le20_2025.csv")
)
assignment_path <- assignment_candidates[file.exists(assignment_candidates)][1]
if (is.na(assignment_path)) stop("Missing town_model_assignment_mape_le20_2025.csv")

arrivals_quantile_candidates <- c(
  file.path(forecast_root, "report_assets", "tourism_arrivals_bootstrap_quantiles_municipal.csv"),
  file.path(forecast_root, "report_assets", "tourism_arrivals_bootstrap_quantiles_municipal_future.csv")
)
arrivals_quantile_path <- arrivals_quantile_candidates[file.exists(arrivals_quantile_candidates)][1]
if (is.na(arrivals_quantile_path)) stop("Missing tourism_arrivals_bootstrap_quantiles_municipal*.csv")

anomaly_candidates <- c(
  file.path(results_intermediate, "anomalies", "towns_anomalous_mape_gt20_2025.csv"),
  file.path(forecast_root, "archive", "municipality_plots_legacy_20260604", "towns_anomalous_mape_gt20_2025.csv")
)
anomaly_path <- anomaly_candidates[file.exists(anomaly_candidates)][1]

data_candidates <- c(
  file.path(forecast_root, "..", "data_full.csv"),
  file.path(forecast_root, "Modelli", "data_full.csv"),
  file.path(getwd(), "Modelli", "data_full.csv")
)
data_path <- data_candidates[file.exists(data_candidates)][1]
if (is.na(data_path)) stop("data_full.csv not found")

cat("1/7: Loading data, assignments, and q95 arrivals...\n")
assignment_tbl <- read_csv(assignment_path, show_col_types = FALSE) %>%
  transmute(
    comune_key = tolower(trimws(as.character(comune_key))),
    assigned_model = as.character(Best_Model)
  )

arrivals_q <- read_csv(arrivals_quantile_path, show_col_types = FALSE) %>%
  mutate(
    comune_key = tolower(trimws(as.character(comune_key))),
    date = as.Date(date),
    q95 = as.numeric(q95)
  ) %>%
  filter(year(date) == 2025) %>%
  select(comune_key, date, q95)

raw_data <- read_csv(data_path, show_col_types = FALSE) %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    dow = wday(date),
    comune_key = tolower(trimws(as.character(comune_key))),
    kwh = ifelse(kwh < 0, NA_real_, as.numeric(kwh)),
    log_kwh = log1p(kwh),
    temp = as.numeric(temperatura),
    log_arrivi = log1p(pmax(as.numeric(totale_arrivi), 0)),
    temp_sq = temp^2,
    HDD = pmax(18 - temp, 0),
    CDD = pmax(temp - 22, 0)
  )

cat("2/7: Feature engineering and scenario inputs...\n")
model_df <- raw_data %>%
  arrange(comune_key, date) %>%
  group_by(comune_key) %>%
  mutate(
    lag9 = lag(log_kwh, 9),
    lag12 = lag(log_kwh, 12),
    lag24 = lag(log_kwh, 24),
    roll_mean_12 = slide_dbl(lag9, mean, .before = 11, .complete = TRUE, na.rm = TRUE),
    roll_sd_12 = slide_dbl(lag9, sd, .before = 11, .complete = TRUE, na.rm = TRUE),
    sin1 = sin(2 * pi * month / 12),
    cos1 = cos(2 * pi * month / 12),
    sin2 = sin(2 * pi * 2 * month / 12),
    cos2 = cos(2 * pi * 2 * month / 12),
    sin3 = sin(2 * pi * 3 * month / 12),
    cos3 = cos(2 * pi * 3 * month / 12)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(log_kwh), !is.na(log_arrivi), !is.na(temp),
    !is.na(lag9), !is.na(lag12), !is.na(lag24),
    !is.na(roll_mean_12), !is.na(roll_sd_12)
  )

train_df <- model_df %>% filter(year >= 2021 & year < 2025)
test_df <- model_df %>% filter(year == 2025)

if (nrow(train_df) == 0) stop("No train rows found for 2021-2024")
if (nrow(test_df) == 0) stop("No test rows found for 2025")

train_df <- train_df %>% mutate(comune_key = factor(comune_key))
test_df <- test_df %>% mutate(comune_key = factor(comune_key, levels = levels(train_df$comune_key)))

# Keep only assigned municipalities.
test_assigned <- test_df %>%
  mutate(comune_key_chr = as.character(comune_key)) %>%
  inner_join(assignment_tbl, by = c("comune_key_chr" = "comune_key")) %>%
  rename(comune_key_str = comune_key_chr)

if (nrow(test_assigned) == 0) stop("No overlap between test data and assigned municipalities")

# Attach q95 arrivals to build shocked predictor.
test_assigned <- test_assigned %>%
  left_join(arrivals_q, by = c("comune_key_str" = "comune_key", "date" = "date")) %>%
  mutate(
    q95 = ifelse(!is.finite(q95) | is.na(q95), expm1(log_arrivi), q95),
    q95 = pmax(q95, 0),
    log_arrivi_q95 = log1p(q95)
  )

q95_used <- test_assigned %>%
  transmute(
    comune_key = comune_key_str,
    date,
    arrivals_actual = expm1(log_arrivi),
    arrivals_q95 = q95,
    arrivals_multiplier_q95 = arrivals_q95 / pmax(arrivals_actual, 1e-9)
  )
write_csv(q95_used, file.path(bootstrap_out_dir, "tourism_arrivals_q95_used_2025.csv"))

cat("3/7: Fitting LMER and GAMM on training data...\n")
mixed_formula <- log_kwh ~ temp + temp_sq + HDD + CDD +
  lag9 + lag12 + lag24 + roll_mean_12 + roll_sd_12 +
  sin1 + cos1 + sin2 + cos2 + sin3 + cos3 +
  (1 + log_arrivi + temp + HDD + lag12 || comune_key)

mixed_fit <- lmerTest::lmer(
  mixed_formula,
  data = train_df,
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 3e5),
    check.conv.singular = .makeCC(action = "ignore", tol = 1e-4)
  )
)

gamm_formula <- log_kwh ~
  s(month, bs = "cc", k = 12) +
  s(temp, k = 8) +
  s(log_arrivi, k = 8) +
  s(lag12, k = 6) +
  s(lag24, k = 6) +
  s(roll_mean_12, k = 6) +
  s(comune_key, bs = "re") +
  s(temp, comune_key, bs = "fs", k = 6, m = 1) +
  s(log_arrivi, comune_key, bs = "fs", k = 6, m = 1)

gamm_fit <- mgcv::bam(
  formula = gamm_formula,
  data = train_df,
  method = "fREML",
  discrete = TRUE,
  knots = list(month = c(0.5, 12.5))
)

cat("4/7: Building baseline and q95-shock predictions by model...\n")
new_base <- test_assigned
new_shock <- test_assigned %>% mutate(log_arrivi = log_arrivi_q95)

pred_base <- new_base %>%
  transmute(
    comune_key = comune_key_str,
    date,
    assigned_model,
    actual_kwh = expm1(log_kwh),
    pred_kwh_mixed_actual = expm1(predict(mixed_fit, newdata = new_base, allow.new.levels = TRUE)),
    pred_kwh_gamm_actual = expm1(predict(gamm_fit, newdata = new_base, type = "response"))
  )

pred_shock <- new_shock %>%
  transmute(
    comune_key = comune_key_str,
    date,
    pred_kwh_mixed_q95 = expm1(predict(mixed_fit, newdata = new_shock, allow.new.levels = TRUE)),
    pred_kwh_gamm_q95 = expm1(predict(gamm_fit, newdata = new_shock, type = "response"))
  )

prophet_towns <- assignment_tbl %>%
  filter(assigned_model == "Prophet") %>%
  pull(comune_key) %>%
  unique()
prophet_rows <- list()

for (muni in prophet_towns) {
  cat("   Prophet town:", muni, "\n")
  train_m <- train_df %>%
    filter(as.character(comune_key) == muni) %>%
    select(ds = date, y = log_kwh, log_arrivi)

  test_m <- test_assigned %>%
    filter(comune_key_str == muni) %>%
    arrange(date)

  if (nrow(train_m) < 12 || nrow(test_m) == 0) next

  m <- prophet(
    changepoint.prior.scale = 0.05,
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE
  )
  m <- add_regressor(m, "log_arrivi")
  m <- fit.prophet(m, train_m)

  future_base <- test_m %>% transmute(ds = date, log_arrivi = log_arrivi)
  future_q95 <- test_m %>% transmute(ds = date, log_arrivi = log_arrivi_q95)

  p_base <- predict(m, future_base)
  p_q95 <- predict(m, future_q95)

  prophet_rows[[muni]] <- tibble(
    comune_key = muni,
    date = as.Date(p_base$ds),
    pred_kwh_prophet_actual = expm1(p_base$yhat),
    pred_kwh_prophet_q95 = expm1(p_q95$yhat)
  )
}

pred_prophet <- bind_rows(prophet_rows)

pred_compare <- pred_base %>%
  left_join(pred_shock, by = c("comune_key", "date")) %>%
  left_join(pred_prophet, by = c("comune_key", "date")) %>%
  left_join(q95_used, by = c("comune_key", "date")) %>%
  mutate(
    load_actual_assigned = case_when(
      assigned_model == "Mixed" ~ pred_kwh_mixed_actual,
      assigned_model == "GAMM" ~ pred_kwh_gamm_actual,
      assigned_model == "Prophet" ~ pred_kwh_prophet_actual,
      TRUE ~ pred_kwh_mixed_actual
    ),
    load_q95_assigned = case_when(
      assigned_model == "Mixed" ~ pred_kwh_mixed_q95,
      assigned_model == "GAMM" ~ pred_kwh_gamm_q95,
      assigned_model == "Prophet" ~ pred_kwh_prophet_q95,
      TRUE ~ pred_kwh_mixed_q95
    ),
    delta_kwh_q95_minus_actual = load_q95_assigned - load_actual_assigned,
    delta_pct_q95_minus_actual = 100 * (load_q95_assigned / pmax(load_actual_assigned, 1e-9) - 1)
  ) %>%
  arrange(comune_key, date)

cat("5/7: Saving baseline-vs-shock outputs...\n")
write_csv(pred_compare, file.path(results_scenario, "tourism_shock_forecast_town_2025.csv"))

regional_compare <- pred_compare %>%
  group_by(date) %>%
  summarise(
    load_actual_assigned = sum(load_actual_assigned, na.rm = TRUE),
    load_q95_assigned = sum(load_q95_assigned, na.rm = TRUE),
    delta_kwh_q95_minus_actual = sum(delta_kwh_q95_minus_actual, na.rm = TRUE),
    delta_pct_q95_minus_actual = 100 * (load_q95_assigned / pmax(load_actual_assigned, 1e-9) - 1),
    .groups = "drop"
  ) %>%
  arrange(date)

write_csv(regional_compare, file.path(results_scenario, "tourism_shock_forecast_regional_2025.csv"))

if (!is.na(anomaly_path)) {
  anomaly_tbl <- read_csv(anomaly_path, show_col_types = FALSE)
  write_csv(anomaly_tbl, file.path(results_scenario, "tourism_shock_anomalous_excluded_towns.csv"))
}

cat("6/7: Plotting town and regional baseline vs q95-shock...\n")
plot_df <- pred_compare %>%
  mutate(comune_slug = gsub("[^a-z0-9]+", "_", tolower(comune_key)))

for (muni in unique(plot_df$comune_key)) {
  d <- plot_df %>% filter(comune_key == muni)
  if (nrow(d) == 0) next

  plot_long <- bind_rows(
    d %>% transmute(date, panel = "Assigned load: actual vs q95 shock", series = "actual", value = load_actual_assigned),
    d %>% transmute(date, panel = "Assigned load: actual vs q95 shock", series = "q95_shock", value = load_q95_assigned),
    d %>% transmute(date, panel = "Absolute shock impact", series = "delta", value = delta_kwh_q95_minus_actual)
  )

  town_plot <- ggplot() +
    geom_line(
      data = plot_long %>% filter(panel == "Assigned load: actual vs q95 shock"),
      aes(x = date, y = value, color = series),
      linewidth = 0.95
    ) +
    geom_hline(
      data = plot_long %>% filter(panel == "Absolute shock impact"),
      aes(yintercept = 0),
      color = "#888888",
      linewidth = 0.4
    ) +
    geom_line(
      data = plot_long %>% filter(panel == "Absolute shock impact"),
      aes(x = date, y = value),
      color = "#7F3C8D",
      linewidth = 0.85
    ) +
    facet_wrap(
      ~panel,
      ncol = 1,
      scales = "free_y",
      strip.position = "top",
      labeller = as_labeller(c(
        "Assigned load: actual vs q95 shock" = "Assigned load: actual vs q95 shock",
        "Absolute shock impact" = "Shock impact: q95 minus actual"
      ))
    ) +
    scale_color_manual(values = c(actual = "#2C3E50", q95_shock = "#D35400")) +
    labs(
      title = paste0("Town shock effect - ", muni),
      subtitle = "Top panel: baseline vs q95 shock load using assigned model | Bottom panel: absolute shock impact",
      x = "Date",
      y = NULL,
      color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold")
    )

  ggsave(
    filename = file.path(town_plot_dir, paste0("town_actual_vs_q95_", unique(d$comune_slug)[1], ".png")),
    plot = town_plot,
    width = 8.5,
    height = 7.4,
    dpi = 140
  )
}

p_reg <- ggplot(regional_compare, aes(x = date)) +
  geom_line(aes(y = load_actual_assigned, color = "actual_arrivals"), linewidth = 1.05) +
  geom_line(aes(y = load_q95_assigned, color = "q95_arrivals_shock"), linewidth = 1.05) +
  labs(
    title = "Regional Assigned Load: actual vs q95 tourism shock",
    x = "Date",
    y = "kWh",
    color = "Series"
  ) +
  scale_color_manual(values = c("actual_arrivals" = "#2C3E50", "q95_arrivals_shock" = "#D35400")) +
  theme_minimal(base_size = 11)

ggsave(
  filename = file.path(results_scenario, "tourism_shock_forecast_regional_95_plot.png"),
  plot = p_reg,
  width = 9,
  height = 5,
  dpi = 140
)

cat("7/7: Done.\n")
cat("\nOutputs written in results/scenario_forecast:\n")
cat(" - tourism_shock_forecast_town_2025.csv\n")
cat(" - tourism_shock_forecast_regional_2025.csv\n")
cat(" - tourism_shock_forecast_regional_95_plot.png\n")
cat(" - town_plots_95/town_actual_vs_q95_*.png\n")
cat("\nq95 inputs written in results/scenario_forecast/bootstrap_samples:\n")
cat(" - tourism_arrivals_q95_used_2025.csv\n")
if (!is.na(anomaly_path)) cat(" - tourism_shock_anomalous_excluded_towns.csv\n")
