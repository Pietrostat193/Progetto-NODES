library(dplyr)
library(tidyr)
library(readr)

script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE)),
  error = function(e) getwd()
)
if (!nzchar(script_dir) || is.na(script_dir)) script_dir <- getwd()

forecast_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
if (!dir.exists(forecast_root)) forecast_root <- getwd()

candidate_input <- c(
  file.path(forecast_root, "results", "intermediate", "model_selection", "municipality_model_performance_2025.csv"),
  file.path(forecast_root, "archive", "municipality_plots_legacy_20260604", "municipality_model_performance_2025.csv")
)
INPUT_PATH <- candidate_input[file.exists(candidate_input)][1]
OUTPUT_PATH <- file.path(forecast_root, "results", "intermediate", "model_selection", "municipality_model_performance_2025_readable.csv")

if (!file.exists(INPUT_PATH)) {
  stop("Input file not found: ", INPUT_PATH)
}

raw_tbl <- read_csv(INPUT_PATH, show_col_types = FALSE)

metric_cols <- c(
  "n_obs",
  "MAE_kwh",
  "RMSE_kwh",
  "Bias_kwh",
  "MAPE_pct",
  "delta_from_best_RMSE",
  "underperform_flag"
)

raw_tbl <- raw_tbl %>%
  mutate(across(all_of(metric_cols), as.character))

readable_tbl <- raw_tbl %>%
  select(
    comune_key,
    model,
    n_obs,
    MAE_kwh,
    RMSE_kwh,
    Bias_kwh,
    MAPE_pct,
    delta_from_best_RMSE,
    underperform_flag
  ) %>%
  pivot_longer(
    cols = all_of(metric_cols),
    names_to = "performance_metric",
    values_to = "value"
  ) %>%
  mutate(
    performance_metric = factor(
      performance_metric,
      levels = c(
        "n_obs",
        "MAE_kwh",
        "RMSE_kwh",
        "Bias_kwh",
        "MAPE_pct",
        "delta_from_best_RMSE",
        "underperform_flag"
      )
    )
  ) %>%
  pivot_wider(
    names_from = model,
    values_from = value
  ) %>%
  arrange(comune_key, performance_metric)

write_csv(readable_tbl, OUTPUT_PATH)

cat("Readable table saved to:\n")
cat(" - ", OUTPUT_PATH, "\n", sep = "")
