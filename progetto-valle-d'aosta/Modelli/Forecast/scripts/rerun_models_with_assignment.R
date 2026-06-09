suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(readr)
  library(slider)
  library(lme4)
  library(lmerTest)
  library(mgcv)
  library(prophet)
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

candidate_data <- c(
  file.path(forecast_root, "..", "data_full.csv"),
  file.path(script_dir, "Modelli", "data_full.csv"),
  file.path(script_dir, "Forecast", "Modelli", "data_full.csv"),
  file.path(forecast_root, "Modelli", "data_full.csv"),
  file.path(getwd(), "Modelli", "data_full.csv")
)
data_path <- candidate_data[file.exists(candidate_data)][1]
if (is.na(data_path)) stop("data_full.csv not found")

results_intermediate_dir <- file.path(forecast_root, "results", "intermediate")
results_assignment_dir <- file.path(results_intermediate_dir, "assignment_eval")
if (!dir.exists(results_assignment_dir)) {
  dir.create(results_assignment_dir, recursive = TRUE, showWarnings = FALSE)
}

candidate_assignment <- c(
  file.path(results_intermediate_dir, "model_selection", "town_model_assignment_mape_le20_2025.csv"),
  file.path(forecast_root, "Municipality_Plots", "town_model_assignment_mape_le20_2025.csv"),
  file.path(forecast_root, "archive", "municipality_plots_legacy_20260604", "town_model_assignment_mape_le20_2025.csv")
)
assignment_path <- candidate_assignment[file.exists(candidate_assignment)][1]

if (is.na(assignment_path)) {
  stop("Missing assignment file in known locations")
}

anomaly_path <- c(
  file.path(results_intermediate_dir, "anomalies", "towns_anomalous_mape_gt20_2025.csv"),
  file.path(forecast_root, "Municipality_Plots", "towns_anomalous_mape_gt20_2025.csv"),
  file.path(forecast_root, "archive", "municipality_plots_legacy_20260604", "towns_anomalous_mape_gt20_2025.csv")
)
anomaly_path <- anomaly_path[file.exists(anomaly_path)][1]

if (!file.exists(assignment_path)) stop("Missing assignment file: ", assignment_path)
if (!file.exists(anomaly_path)) warning("Anomaly file not found: ", anomaly_path)

cat("1/8: Loading data and assignment...\n")
raw_data <- read_csv(data_path, show_col_types = FALSE)
assignment_tbl <- read_csv(assignment_path, show_col_types = FALSE) %>%
  transmute(
    comune_key = tolower(trimws(as.character(comune_key))),
    assigned_model = as.character(Best_Model)
  )

if (!"totale_arrivi" %in% names(raw_data)) {
  stop("Column totale_arrivi missing in data_full.csv")
}

cat("2/8: Feature engineering...\n")
model_df <- raw_data %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    dow = wday(date),
    comune_key = tolower(trimws(as.character(comune_key))),
    kwh = ifelse(kwh < 0, NA_real_, as.numeric(kwh)),
    log_kwh = log1p(kwh),
    temp = as.numeric(temperatura),
    log_arrivi = log1p(as.numeric(totale_arrivi)),
    temp_sq = temp^2,
    HDD = pmax(18 - temp, 0),
    CDD = pmax(temp - 22, 0)
  ) %>%
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

if (nrow(test_df) == 0) stop("No test rows found for year 2025")

train_df <- train_df %>% mutate(comune_key = factor(comune_key))
test_df <- test_df %>% mutate(comune_key = factor(comune_key, levels = levels(train_df$comune_key)))

assigned_towns <- assignment_tbl %>% distinct(comune_key)
test_assigned <- test_df %>% filter(as.character(comune_key) %in% assigned_towns$comune_key)
if (nrow(test_assigned) == 0) stop("No test rows overlap with assignment towns")

cat("3/8: Fitting Mixed model...\n")
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

cat("4/8: Fitting GAMM model...\n")
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

cat("5/8: Fitting Prophet per municipality...\n")
all_towns <- unique(as.character(train_df$comune_key))
prophet_base <- list()

for (muni in all_towns) {
  train_m <- train_df %>%
    filter(as.character(comune_key) == muni) %>%
    select(ds = date, y = log_kwh, log_arrivi)

  test_m <- test_df %>%
    filter(as.character(comune_key) == muni) %>%
    arrange(date) %>%
    transmute(ds = date, log_arrivi = log_arrivi)

  if (nrow(train_m) < 12 || nrow(test_m) == 0) next

  m <- prophet(
    changepoint.prior.scale = 0.05,
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE
  )
  m <- add_regressor(m, "log_arrivi")
  m <- fit.prophet(m, train_m)
  p <- predict(m, test_m)

  prophet_base[[muni]] <- tibble(
    date = as.Date(p$ds),
    comune_key = muni,
    pred_kwh_prophet = expm1(p$yhat)
  )
}

prophet_pred <- bind_rows(prophet_base)

cat("6/8: Building assigned strategy predictions...\n")
pred_df <- test_df %>%
  transmute(
    date,
    comune_key = as.character(comune_key),
    actual_kwh = expm1(log_kwh),
    pred_kwh_mixed = expm1(predict(mixed_fit, newdata = test_df, allow.new.levels = TRUE)),
    pred_kwh_gamm = expm1(predict(gamm_fit, newdata = test_df, type = "response"))
  ) %>%
  left_join(prophet_pred, by = c("date", "comune_key")) %>%
  left_join(assignment_tbl, by = "comune_key") %>%
  mutate(
    assigned_prediction = case_when(
      assigned_model == "Mixed" ~ pred_kwh_mixed,
      assigned_model == "GAMM" ~ pred_kwh_gamm,
      assigned_model == "Prophet" ~ pred_kwh_prophet,
      TRUE ~ NA_real_
    ),
    assigned_prediction = ifelse(is.na(assigned_prediction), pred_kwh_mixed, assigned_prediction),
    assigned_model_used = case_when(
      assigned_model %in% c("Mixed", "GAMM", "Prophet") & !is.na(assigned_prediction) ~ assigned_model,
      TRUE ~ "Mixed_fallback"
    )
  )

pred_assigned <- pred_df %>% filter(!is.na(assigned_model))

metric_tbl <- function(df, pred_col, model_name) {
  p <- df[[pred_col]]
  a <- df$actual_kwh
  tibble(
    model = model_name,
    n_obs = sum(!is.na(a) & !is.na(p)),
    MAE_kwh = mean(abs(a - p), na.rm = TRUE),
    RMSE_kwh = sqrt(mean((a - p)^2, na.rm = TRUE)),
    Bias_kwh = mean(p - a, na.rm = TRUE),
    MAPE_pct = 100 * mean(abs((a - p) / ifelse(a == 0, NA_real_, a)), na.rm = TRUE)
  )
}

cat("7/8: Computing new performance metrics...\n")
overall_comp <- bind_rows(
  metric_tbl(pred_assigned, "assigned_prediction", "Assigned_Strategy"),
  metric_tbl(pred_assigned, "pred_kwh_mixed", "Mixed_only"),
  metric_tbl(pred_assigned, "pred_kwh_gamm", "GAMM_only"),
  metric_tbl(pred_assigned, "pred_kwh_prophet", "Prophet_only")
) %>%
  arrange(MAPE_pct)

town_level_assigned <- pred_assigned %>%
  group_by(comune_key, assigned_model) %>%
  summarise(
    n_obs = n(),
    MAE_kwh = mean(abs(actual_kwh - assigned_prediction), na.rm = TRUE),
    RMSE_kwh = sqrt(mean((actual_kwh - assigned_prediction)^2, na.rm = TRUE)),
    Bias_kwh = mean(assigned_prediction - actual_kwh, na.rm = TRUE),
    MAPE_pct = 100 * mean(abs((actual_kwh - assigned_prediction) / ifelse(actual_kwh == 0, NA_real_, actual_kwh)), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(MAPE_pct))

assigned_by_model_counts <- pred_assigned %>%
  distinct(comune_key, assigned_model) %>%
  count(assigned_model, name = "n_towns") %>%
  arrange(desc(n_towns))

cat("8/8: Saving outputs...\n")
write_csv(overall_comp, file.path(results_assignment_dir, "assignment_strategy_vs_single_models_2025.csv"))
write_csv(town_level_assigned, file.path(results_assignment_dir, "assignment_strategy_town_metrics_2025.csv"))
write_csv(assigned_by_model_counts, file.path(results_assignment_dir, "assignment_strategy_model_counts_2025.csv"))
write_csv(pred_assigned, file.path(results_assignment_dir, "assignment_strategy_predictions_2025.csv"))

cat("\n=== DONE ===\n")
cat("Assigned towns evaluated:", n_distinct(pred_assigned$comune_key), "\n")
print(overall_comp)
cat("\nFiles written in results/intermediate/assignment_eval:\n")
cat(" - assignment_strategy_vs_single_models_2025.csv\n")
cat(" - assignment_strategy_town_metrics_2025.csv\n")
cat(" - assignment_strategy_model_counts_2025.csv\n")
cat(" - assignment_strategy_predictions_2025.csv\n")
