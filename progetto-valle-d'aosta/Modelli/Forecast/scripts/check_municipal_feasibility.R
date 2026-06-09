suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE)),
  error = function(e) getwd()
)
if (!nzchar(script_dir) || is.na(script_dir)) script_dir <- getwd()

assets_dir <- file.path(script_dir, "report_assets")
data_path  <- file.path(script_dir, "..", "data_full.csv")

stopifnot(file.exists(assets_dir), file.exists(data_path))

boot_mun <- read.csv(file.path(assets_dir, "tourism_arrivals_bootstrap_quantiles_municipal.csv"),
                     stringsAsFactors = FALSE)
val_mun  <- read.csv(file.path(assets_dir, "tourism_arrivals_validation_quantiles.csv"),
                     stringsAsFactors = FALSE)
data_full <- read.csv(data_path, stringsAsFactors = FALSE)

boot_mun$date <- as.Date(substr(boot_mun$date, 1, 10))
val_mun$date  <- as.Date(substr(val_mun$date,  1, 10))
data_full$date <- as.Date(substr(data_full$date, 1, 10))

val_start <- min(boot_mun$date)
val_end   <- max(boot_mun$date)
cat("Validation window:", as.character(val_start), "->", as.character(val_end), "\n")

actuals <- data_full %>%
  filter(date >= val_start, date <= val_end) %>%
  mutate(totale_arrivi = pmax(as.numeric(totale_arrivi), 0)) %>%
  group_by(comune_key, comune_nome, date) %>%
  summarise(actual_arrivals = sum(totale_arrivi, na.rm = TRUE), .groups = "drop")

# Multilevel baseline scenario only (matches headline figures)
ml <- val_mun %>%
  filter(scenario == "baseline") %>%
  transmute(comune_key, comune_nome, date, q05, q50, q95, model = "multilevel")

bs <- boot_mun %>%
  transmute(comune_key, comune_nome, date, q05, q50, q95, model = "nnetar_share")

joined <- bind_rows(ml, bs) %>%
  left_join(actuals, by = c("comune_key", "comune_nome", "date"))

metrics <- joined %>%
  mutate(
    err     = q50 - actual_arrivals,
    abs_err = abs(err),
    sq_err  = err^2,
    ape     = ifelse(actual_arrivals > 0, abs_err / actual_arrivals, NA_real_),
    inside  = actual_arrivals >= q05 & actual_arrivals <= q95
  ) %>%
  group_by(model, comune_key, comune_nome) %>%
  summarise(
    n_obs        = sum(!is.na(actual_arrivals)),
    actual_total = sum(actual_arrivals, na.rm = TRUE),
    pred_total   = sum(q50, na.rm = TRUE),
    mae          = mean(abs_err, na.rm = TRUE),
    rmse         = sqrt(mean(sq_err, na.rm = TRUE)),
    bias         = mean(err, na.rm = TRUE),
    mape         = mean(ape, na.rm = TRUE),
    coverage_90  = mean(inside, na.rm = TRUE),
    rel_total    = ifelse(actual_total > 0,
                          (pred_total - actual_total) / actual_total,
                          NA_real_),
    .groups = "drop"
  )

# Aggregate summary
summary_tbl <- metrics %>%
  group_by(model) %>%
  summarise(
    n_comuni              = dplyr::n(),
    median_mape           = median(mape, na.rm = TRUE),
    mean_mape             = mean(mape, na.rm = TRUE),
    median_abs_rel_total  = median(abs(rel_total), na.rm = TRUE),
    mean_abs_rel_total    = mean(abs(rel_total),   na.rm = TRUE),
    mean_coverage_90      = mean(coverage_90, na.rm = TRUE),
    pct_rel_total_gt_50   = mean(abs(rel_total) > 0.5,  na.rm = TRUE),
    pct_rel_total_gt_100  = mean(abs(rel_total) > 1.0,  na.rm = TRUE),
    pct_mape_gt_75        = mean(mape > 0.75, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n===== SUMMARY (per municipality forecast quality, 2025 validation) =====\n")
print(as.data.frame(summary_tbl), row.names = FALSE, digits = 3)

# Worst comuni under the nnetar+share approach
worst_share <- metrics %>%
  filter(model == "nnetar_share") %>%
  arrange(desc(abs(rel_total))) %>%
  slice_head(n = 15) %>%
  select(comune_nome, actual_total, pred_total, rel_total, mape, coverage_90)

cat("\n===== TOP 15 worst comuni under nnetar+share (by |rel_total|) =====\n")
print(as.data.frame(worst_share), row.names = FALSE, digits = 3)

# Same comuni: how does multilevel do?
worst_keys <- metrics %>%
  filter(model == "nnetar_share") %>%
  arrange(desc(abs(rel_total))) %>%
  slice_head(n = 15) %>%
  pull(comune_nome)

side_by_side <- metrics %>%
  filter(comune_nome %in% worst_keys) %>%
  select(model, comune_nome, rel_total, mape, coverage_90) %>%
  pivot_wider(names_from = model, values_from = c(rel_total, mape, coverage_90)) %>%
  mutate(comune_nome = factor(comune_nome, levels = worst_keys)) %>%
  arrange(comune_nome)

cat("\n===== Same 15 comuni: nnetar_share vs multilevel =====\n")
print(as.data.frame(side_by_side), row.names = FALSE, digits = 3)

# Counts of implausible comuni
flag <- function(df) {
  df %>% mutate(implausible =
    (!is.na(rel_total)   & abs(rel_total)   > 0.5) |
    (!is.na(mape)        & mape             > 0.75) |
    (!is.na(coverage_90) & coverage_90      < 0.5)
  )
}

cat("\n===== Counts implausible (|rel|>0.5 OR mape>0.75 OR cov<0.5) =====\n")
print(
  metrics %>%
    flag() %>%
    group_by(model) %>%
    summarise(n = dplyr::n(), implausible = sum(implausible, na.rm = TRUE),
              share = mean(implausible, na.rm = TRUE), .groups = "drop") %>%
    as.data.frame(),
  row.names = FALSE, digits = 3
)

invisible(NULL)
