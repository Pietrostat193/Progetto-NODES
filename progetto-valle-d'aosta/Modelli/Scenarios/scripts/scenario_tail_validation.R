suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(tidyr)
})

get_script_dir <- function() {
  frame_files <- Filter(
    Negate(is.null),
    lapply(sys.frames(), function(frame) frame$ofile)
  )

  if (length(frame_files) > 0) {
    return(dirname(normalizePath(frame_files[[length(frame_files)]], winslash = "/", mustWork = FALSE)))
  }

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)))
  }

  getwd()
}

script_dir <- get_script_dir()
scenarios_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
modelli_root <- normalizePath(file.path(scenarios_root, ".."), winslash = "/", mustWork = FALSE)

results_dir <- file.path(scenarios_root, "results")
assets_dir <- file.path(scenarios_root, "report_assets")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(assets_dir, recursive = TRUE, showWarnings = FALSE)

scenario_month_path <- file.path(results_dir, "scenario_tail_impact_by_town_month.csv")
scenario_annual_path <- file.path(results_dir, "scenario_tail_impact_by_town_annual.csv")
forecast_town_path <- file.path(modelli_root, "Forecast", "results", "scenario_forecast", "tourism_shock_forecast_town_2025.csv")

if (!file.exists(scenario_month_path)) stop("Missing file: ", scenario_month_path)
if (!file.exists(scenario_annual_path)) stop("Missing file: ", scenario_annual_path)
if (!file.exists(forecast_town_path)) stop("Missing file: ", forecast_town_path)

cat("1/4: Loading validation inputs...\n")
scenario_month <- read_csv(scenario_month_path, show_col_types = FALSE) %>%
  mutate(
    date = as.Date(date),
    scenario_tail_impact_kwh = as.numeric(scenario_tail_impact_kwh),
    scenario_tail_impact_weighted = as.numeric(scenario_tail_impact_weighted)
  )

scenario_annual <- read_csv(scenario_annual_path, show_col_types = FALSE) %>%
  mutate(
    scenario_tail_impact_weighted_sum = as.numeric(scenario_tail_impact_weighted_sum),
    scenario_tail_impact_kwh_sum = as.numeric(scenario_tail_impact_kwh_sum)
  )

forecast_town <- read_csv(forecast_town_path, show_col_types = FALSE) %>%
  mutate(
    date = as.Date(date),
    actual_kwh = as.numeric(actual_kwh),
    load_actual_assigned = as.numeric(load_actual_assigned),
    realized_pos_stress_kwh = pmax(actual_kwh - load_actual_assigned, 0)
  )

cat("2/4: Computing validation metrics...\n")
valid_month <- scenario_month %>%
  left_join(
    forecast_town %>% select(comune_key, date, realized_pos_stress_kwh),
    by = c("comune_key", "date")
  ) %>%
  mutate(realized_pos_stress_kwh = ifelse(is.na(realized_pos_stress_kwh), 0, realized_pos_stress_kwh))

# Monthly hit rate on top-10 stressed municipalities.
monthly_topk <- valid_month %>%
  group_by(date) %>%
  mutate(
    pred_rank = dense_rank(desc(scenario_tail_impact_weighted)),
    real_rank = dense_rank(desc(realized_pos_stress_kwh)),
    pred_top10 = pred_rank <= 10,
    real_top10 = real_rank <= 10
  ) %>%
  summarise(
    overlap_top10 = sum(pred_top10 & real_top10, na.rm = TRUE),
    precision_top10 = overlap_top10 / 10,
    recall_top10 = overlap_top10 / 10,
    hit_rate_top10 = overlap_top10 / 10,
    .groups = "drop"
  )

# Annual town-level summary for ranking and correlation.
realized_annual <- forecast_town %>%
  group_by(comune_key) %>%
  summarise(
    realized_pos_stress_kwh_sum = sum(realized_pos_stress_kwh, na.rm = TRUE),
    realized_net_stress_kwh_sum = sum(actual_kwh - load_actual_assigned, na.rm = TRUE),
    .groups = "drop"
  )

annual_join <- scenario_annual %>%
  select(
    comune_key,
    comune_nome,
    scenario_tail_impact_weighted_sum,
    scenario_tail_impact_kwh_sum,
    assigned_model
  ) %>%
  left_join(realized_annual, by = "comune_key") %>%
  mutate(
    realized_pos_stress_kwh_sum = ifelse(is.na(realized_pos_stress_kwh_sum), 0, realized_pos_stress_kwh_sum),
    realized_net_stress_kwh_sum = ifelse(is.na(realized_net_stress_kwh_sum), 0, realized_net_stress_kwh_sum)
  )

spearman_weighted <- cor(
  annual_join$scenario_tail_impact_weighted_sum,
  annual_join$realized_pos_stress_kwh_sum,
  method = "spearman",
  use = "complete.obs"
)

spearman_unweighted <- cor(
  annual_join$scenario_tail_impact_kwh_sum,
  annual_join$realized_pos_stress_kwh_sum,
  method = "spearman",
  use = "complete.obs"
)

pearson_weighted <- cor(
  annual_join$scenario_tail_impact_weighted_sum,
  annual_join$realized_pos_stress_kwh_sum,
  method = "pearson",
  use = "complete.obs"
)

calc_topk <- function(df, k) {
  pred_top <- df %>% slice_max(order_by = scenario_tail_impact_weighted_sum, n = k, with_ties = FALSE) %>% pull(comune_key)
  real_top <- df %>% slice_max(order_by = realized_pos_stress_kwh_sum, n = k, with_ties = FALSE) %>% pull(comune_key)
  overlap <- length(intersect(pred_top, real_top))
  tibble(
    k = k,
    overlap = overlap,
    precision = overlap / k,
    recall = overlap / k
  )
}

topk_metrics <- bind_rows(
  calc_topk(annual_join, 5),
  calc_topk(annual_join, 10),
  calc_topk(annual_join, 15),
  calc_topk(annual_join, 20)
)

calibration_deciles <- annual_join %>%
  mutate(pred_decile = ntile(scenario_tail_impact_weighted_sum, 10)) %>%
  group_by(pred_decile) %>%
  summarise(
    n_towns = n(),
    mean_pred_weighted = mean(scenario_tail_impact_weighted_sum, na.rm = TRUE),
    mean_realized_pos = mean(realized_pos_stress_kwh_sum, na.rm = TRUE),
    median_realized_pos = median(realized_pos_stress_kwh_sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(pred_decile)

summary_metrics <- tibble(
  metric = c(
    "spearman_weighted_vs_realized_pos",
    "spearman_unweighted_vs_realized_pos",
    "pearson_weighted_vs_realized_pos",
    "avg_monthly_hit_rate_top10",
    "median_monthly_hit_rate_top10"
  ),
  value = c(
    spearman_weighted,
    spearman_unweighted,
    pearson_weighted,
    mean(monthly_topk$hit_rate_top10, na.rm = TRUE),
    median(monthly_topk$hit_rate_top10, na.rm = TRUE)
  )
)

write_csv(annual_join, file.path(results_dir, "scenario_tail_validation_join_annual.csv"))
write_csv(topk_metrics, file.path(results_dir, "scenario_tail_validation_topk_metrics.csv"))
write_csv(monthly_topk, file.path(results_dir, "scenario_tail_validation_monthly_hit_rate.csv"))
write_csv(calibration_deciles, file.path(results_dir, "scenario_tail_validation_calibration_deciles.csv"))
write_csv(summary_metrics, file.path(results_dir, "scenario_tail_validation_summary_metrics.csv"))

cat("3/4: Plotting validation diagnostics...\n")
p_topk <- ggplot(topk_metrics, aes(x = factor(k), y = precision)) +
  geom_col(fill = "#2A9D8F") +
  geom_text(aes(label = sprintf("%d/%d", overlap, k)), vjust = -0.4, size = 3.3) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Top-k precision of predicted stressed towns",
    subtitle = "Overlap between predicted and realized stressed-town rankings",
    x = "k",
    y = "Precision"
  ) +
  theme_minimal(base_size = 11)

ggsave(file.path(assets_dir, "scenario_tail_validation_topk_precision.png"), p_topk, width = 7.5, height = 4.8, dpi = 150)

p_hit <- ggplot(monthly_topk, aes(x = date, y = hit_rate_top10)) +
  geom_line(color = "#E76F51", linewidth = 0.9) +
  geom_point(color = "#E76F51", size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Monthly hit rate on top-10 stressed towns",
    subtitle = "Share of overlap between predicted and realized top-10 stress towns",
    x = "Date",
    y = "Hit rate"
  ) +
  theme_minimal(base_size = 11)

ggsave(file.path(assets_dir, "scenario_tail_validation_monthly_hit_rate.png"), p_hit, width = 8, height = 4.8, dpi = 150)

p_cal <- ggplot(calibration_deciles, aes(x = mean_pred_weighted, y = mean_realized_pos)) +
  geom_point(size = 2.3, color = "#264653") +
  geom_smooth(method = "lm", se = FALSE, color = "#F4A261", linewidth = 0.9) +
  geom_text(aes(label = pred_decile), nudge_y = max(calibration_deciles$mean_realized_pos, na.rm = TRUE) * 0.02, size = 3) +
  labs(
    title = "Calibration by predicted-risk decile",
    subtitle = "Higher predicted deciles should show higher realized stress",
    x = "Mean predicted weighted stress (decile)",
    y = "Mean realized positive stress (decile)"
  ) +
  theme_minimal(base_size = 11)

ggsave(file.path(assets_dir, "scenario_tail_validation_calibration.png"), p_cal, width = 7.5, height = 4.8, dpi = 150)

cat("4/4: Done. Validation assets written in Modelli/Scenarios.\n")
cat(sprintf("Spearman weighted vs realized positive stress: %.3f\n", spearman_weighted))
cat(sprintf("Average monthly hit rate (top10): %.3f\n", mean(monthly_topk$hit_rate_top10, na.rm = TRUE)))
