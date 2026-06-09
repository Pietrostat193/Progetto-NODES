# =============================================================================
# Phase C3 - Propagate forecast uncertainty onto scenario tail-impact metrics
# -----------------------------------------------------------------------------
# Reuses the existing per-replicate load-impact bootstrap draws (no new
# simulation) and combines them with the historical tail-risk weights to obtain
# predictive 95% intervals for the scenario load shock (Delta L), the tail-
# conditioned impact (I^tail) and the risk-weighted impact (I^w), at municipality
# and regional level.
#
# Reads:
#   ../../Forecast/results/scenario_forecast/bootstrap_samples/
#       tourism_load_impact_bootstrap_draws_2025.csv
#   ../../Tail Risk/report_assets/municipality_tail_risk_enriched.csv
#
# Produces (in ../results):
#   scenario_tail_impact_ci_by_town.csv
#   scenario_tail_impact_ci_regional.csv
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

normalize_key <- function(x) gsub("\\s+", " ", trimws(tolower(as.character(x))))

get_script_dir <- function() {
  frame_files <- Filter(Negate(is.null), lapply(sys.frames(), function(f) f$ofile))
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
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

draws_path <- file.path(modelli_root, "Forecast", "results", "scenario_forecast",
                        "bootstrap_samples", "tourism_load_impact_bootstrap_draws_2025.csv")
tail_risk_path <- file.path(modelli_root, "Tail Risk", "report_assets",
                            "municipality_tail_risk_enriched.csv")

if (!file.exists(draws_path)) stop("Missing bootstrap draws: ", draws_path)
if (!file.exists(tail_risk_path)) stop("Missing tail-risk file: ", tail_risk_path)

draws <- read_csv(draws_path, show_col_types = FALSE) %>%
  mutate(comune_key_norm = normalize_key(comune_key))

tail_risk <- read_csv(tail_risk_path, show_col_types = FALSE) %>%
  mutate(comune_key_norm = normalize_key(comune_key)) %>%
  transmute(
    comune_key_norm,
    comune_nome,
    risk_rank,
    p95 = ifelse(is.finite(p_energy_given_tourism_upper_95), p_energy_given_tourism_upper_95, 0),
    risk_score = ifelse(is.finite(risk_score), risk_score, 0)
  )

n_reps <- length(unique(draws$rep))
message(sprintf("C3: reusing %d existing bootstrap replicates across %d municipalities.",
                n_reps, length(unique(draws$comune_key_norm))))

# per replicate x municipality: annual aggregates of Delta L, I^tail, I^w
rep_town <- draws %>%
  left_join(tail_risk, by = "comune_key_norm") %>%
  mutate(
    p95 = ifelse(is.na(p95), 0, p95),
    risk_score = ifelse(is.na(risk_score), 0, risk_score),
    i_tail = delta_kwh_vs_baseline * p95,
    i_w    = delta_kwh_vs_baseline * p95 * risk_score
  ) %>%
  group_by(rep, comune_key, comune_key_norm) %>%
  summarise(
    delta_kwh_annual = sum(delta_kwh_vs_baseline, na.rm = TRUE),
    i_tail_annual = sum(i_tail, na.rm = TRUE),
    i_w_annual = sum(i_w, na.rm = TRUE),
    .groups = "drop"
  )

q025 <- function(x) stats::quantile(x, 0.025, na.rm = TRUE)
q975 <- function(x) stats::quantile(x, 0.975, na.rm = TRUE)

town_ci <- rep_town %>%
  group_by(comune_key, comune_key_norm) %>%
  summarise(
    delta_kwh_mean = mean(delta_kwh_annual), delta_kwh_lo = q025(delta_kwh_annual), delta_kwh_hi = q975(delta_kwh_annual),
    i_tail_mean = mean(i_tail_annual), i_tail_lo = q025(i_tail_annual), i_tail_hi = q975(i_tail_annual),
    i_w_mean = mean(i_w_annual), i_w_lo = q025(i_w_annual), i_w_hi = q975(i_w_annual),
    .groups = "drop"
  ) %>%
  left_join(tail_risk %>% select(comune_key_norm, comune_nome, risk_rank), by = "comune_key_norm") %>%
  arrange(desc(i_w_mean)) %>%
  select(comune_key, comune_nome, risk_rank,
         delta_kwh_mean, delta_kwh_lo, delta_kwh_hi,
         i_tail_mean, i_tail_lo, i_tail_hi,
         i_w_mean, i_w_lo, i_w_hi)

write.csv(town_ci, file = file.path(results_dir, "scenario_tail_impact_ci_by_town.csv"),
          row.names = FALSE)

# regional totals per replicate, then CI
rep_regional <- rep_town %>%
  group_by(rep) %>%
  summarise(
    delta_kwh_total = sum(delta_kwh_annual, na.rm = TRUE),
    i_tail_total = sum(i_tail_annual, na.rm = TRUE),
    i_w_total = sum(i_w_annual, na.rm = TRUE),
    .groups = "drop"
  )

regional_ci <- data.frame(
  metric = c("delta_kwh_total", "i_tail_total", "i_w_total"),
  mean_gwh = c(mean(rep_regional$delta_kwh_total), mean(rep_regional$i_tail_total), mean(rep_regional$i_w_total)) / 1e6,
  lo_gwh = c(q025(rep_regional$delta_kwh_total), q025(rep_regional$i_tail_total), q025(rep_regional$i_w_total)) / 1e6,
  hi_gwh = c(q975(rep_regional$delta_kwh_total), q975(rep_regional$i_tail_total), q975(rep_regional$i_w_total)) / 1e6,
  n_replicates = n_reps,
  stringsAsFactors = FALSE
)
write.csv(regional_ci, file = file.path(results_dir, "scenario_tail_impact_ci_regional.csv"),
          row.names = FALSE)

message("C3 regional predictive intervals (GWh):")
for (i in seq_len(nrow(regional_ci))) {
  message(sprintf("    %-16s %.3f [%.3f, %.3f]",
                  regional_ci$metric[i], regional_ci$mean_gwh[i],
                  regional_ci$lo_gwh[i], regional_ci$hi_gwh[i]))
}
message("Phase C3 complete.")
