# =============================================================================
# Phase C - Composite-score validation and benchmark comparison
# -----------------------------------------------------------------------------
# C1: weight-sensitivity of the tail-risk composite score.
# C2: benchmark comparison of 5 candidate rankings against realized 2025
#     positive electricity stress.
#
# Reads:
#   report_assets/municipality_tail_risk_enriched.csv
#   ../Forecast/results/scenario_forecast/tourism_shock_forecast_town_2025.csv
#
# Produces:
#   report_assets/risk_score_weight_sensitivity.csv
#   report_assets/benchmark_comparison.csv
# =============================================================================

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
setwd(script_dir)

suppressPackageStartupMessages(library(dplyr))
set.seed(20240517)

assets_dir <- "report_assets"
enriched <- read.csv(file.path(assets_dir, "municipality_tail_risk_enriched.csv"),
                     stringsAsFactors = FALSE)

# coalesce the three score components
cz <- function(x) ifelse(is.na(x), 0, x)
comp <- enriched %>% transmute(
  comune_key,
  comune_nome,
  p95   = cz(p_energy_given_tourism_upper_95),
  p90   = cz(p_energy_given_tourism_upper_90),
  sev95 = cz(mean_joint_severity_95),
  mean_arrivi = mean_arrivi,
  arrivi_per_residente = arrivi_per_residente,
  mean_kwh = mean_kwh,
  n_energy_upper_95 = n_energy_upper_95,
  n_obs = n_obs,
  risk_score_prod = risk_score
)

score_with <- function(w95, w90, wsev, df = comp) {
  df$p95 * w95 + df$p90 * w90 + df$sev95 * wsev
}

prod_score <- comp$risk_score_prod
prod_rank  <- rank(-prod_score, ties.method = "min")
prod_top10 <- comp$comune_key[order(prod_score, decreasing = TRUE)][1:10]

# -----------------------------------------------------------------------------
# C1. Weight sensitivity
# -----------------------------------------------------------------------------
weight_schemes <- list(
  production    = c(0.65, 0.25, 0.10),
  equal         = c(1/3, 1/3, 1/3),
  tail_only     = c(1, 0, 0),     # q95 conditional prob only
  q90_only      = c(0, 1, 0),
  severity_only = c(0, 0, 1)
)

eval_scheme <- function(name, w) {
  s <- score_with(w[1], w[2], w[3])
  r <- rank(-s, ties.method = "min")
  top10 <- comp$comune_key[order(s, decreasing = TRUE)][1:10]
  data.frame(
    scheme = name,
    w_p95 = w[1], w_p90 = w[2], w_sev = w[3],
    spearman_vs_production = suppressWarnings(cor(s, prod_score, method = "spearman")),
    rank_spearman_vs_production = suppressWarnings(cor(r, prod_rank, method = "spearman")),
    top10_overlap = length(intersect(top10, prod_top10)),
    stringsAsFactors = FALSE
  )
}

fixed_tbl <- bind_rows(lapply(names(weight_schemes),
                              function(n) eval_scheme(n, weight_schemes[[n]])))

# random simplex draws (Dirichlet(1,1,1))
n_random <- 500
rand_w <- matrix(rexp(n_random * 3), ncol = 3)
rand_w <- rand_w / rowSums(rand_w)
rand_stats <- t(apply(rand_w, 1, function(w) {
  s <- score_with(w[1], w[2], w[3])
  r <- rank(-s, ties.method = "min")
  top10 <- comp$comune_key[order(s, decreasing = TRUE)][1:10]
  c(spearman = suppressWarnings(cor(s, prod_score, method = "spearman")),
    rank_spearman = suppressWarnings(cor(r, prod_rank, method = "spearman")),
    top10_overlap = length(intersect(top10, prod_top10)))
}))

rand_summary <- data.frame(
  scheme = "random_simplex_mean", w_p95 = mean(rand_w[, 1]), w_p90 = mean(rand_w[, 2]),
  w_sev = mean(rand_w[, 3]),
  spearman_vs_production = mean(rand_stats[, "spearman"]),
  rank_spearman_vs_production = mean(rand_stats[, "rank_spearman"]),
  top10_overlap = mean(rand_stats[, "top10_overlap"]),
  stringsAsFactors = FALSE
)
rand_q <- data.frame(
  scheme = c("random_simplex_q05", "random_simplex_q95"),
  w_p95 = NA, w_p90 = NA, w_sev = NA,
  spearman_vs_production = quantile(rand_stats[, "spearman"], c(0.05, 0.95)),
  rank_spearman_vs_production = quantile(rand_stats[, "rank_spearman"], c(0.05, 0.95)),
  top10_overlap = quantile(rand_stats[, "top10_overlap"], c(0.05, 0.95)),
  stringsAsFactors = FALSE
)

weight_sensitivity_tbl <- bind_rows(fixed_tbl, rand_summary, rand_q)
write.csv(weight_sensitivity_tbl,
          file = file.path(assets_dir, "risk_score_weight_sensitivity.csv"),
          row.names = FALSE)
message(sprintf("C1: median random-simplex top-10 overlap = %.1f/10; rank Spearman vs production (random mean) = %.3f",
                median(rand_stats[, "top10_overlap"]), rand_summary$rank_spearman_vs_production))

# -----------------------------------------------------------------------------
# C2. Benchmark comparison against realized 2025 positive stress
# -----------------------------------------------------------------------------
forecast_town_path <- file.path("..", "Forecast", "results", "scenario_forecast",
                                "tourism_shock_forecast_town_2025.csv")
ft <- read.csv(forecast_town_path, stringsAsFactors = FALSE)
ft$realized_pos_stress_kwh <- pmax(ft$actual_kwh - ft$load_actual_assigned, 0)

realized <- ft %>%
  group_by(comune_key) %>%
  summarise(realized_pos_stress = sum(realized_pos_stress_kwh, na.rm = TRUE),
            .groups = "drop")

# normalize keys for a robust join
nk <- function(x) gsub("\\s+", " ", trimws(tolower(as.character(x))))
comp$key_norm <- nk(comp$comune_key)
realized$key_norm <- nk(realized$comune_key)

merged <- comp %>%
  inner_join(realized %>% select(key_norm, realized_pos_stress), by = "key_norm")
message(sprintf("C2: matched %d municipalities to realized 2025 stress.", nrow(merged)))

# candidate rankings (higher = more at-risk)
rankings <- list(
  tourism_intensity_arrivals = merged$mean_arrivi,
  arrivals_per_resident      = merged$arrivi_per_residente,
  electricity_only_extremes  = merged$n_energy_upper_95 / pmax(merged$n_obs, 1),
  historical_avg_demand      = merged$mean_kwh,
  proposed_R_i               = merged$risk_score_prod
)

realized_vec <- merged$realized_pos_stress
realized_top10 <- merged$comune_key[order(realized_vec, decreasing = TRUE)][1:10]

bench_tbl <- bind_rows(lapply(names(rankings), function(nm) {
  v <- rankings[[nm]]
  pred_top10 <- merged$comune_key[order(v, decreasing = TRUE)][1:10]
  data.frame(
    ranking = nm,
    spearman_vs_realized = suppressWarnings(cor(v, realized_vec, method = "spearman",
                                                use = "complete.obs")),
    top10_overlap_with_realized = length(intersect(pred_top10, realized_top10)),
    stringsAsFactors = FALSE
  )
})) %>% arrange(desc(spearman_vs_realized))

write.csv(bench_tbl, file = file.path(assets_dir, "benchmark_comparison.csv"),
          row.names = FALSE)
message("C2: benchmark comparison ->")
for (i in seq_len(nrow(bench_tbl))) {
  message(sprintf("    %-28s Spearman=%.3f  top10=%d/10",
                  bench_tbl$ranking[i], bench_tbl$spearman_vs_realized[i],
                  bench_tbl$top10_overlap_with_realized[i]))
}
message("Phase C complete.")
