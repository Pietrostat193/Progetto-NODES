# =============================================================================
# Phase B - Tail-risk uncertainty quantification and empirical-Bayes shrinkage
# -----------------------------------------------------------------------------
# Builds on the artefacts produced by run_copula_tail_risk.R (no heavy refit of
# the mixed model is required). Reads:
#   report_assets/copula_input_series.csv        (= tail_df, obs-level)
#   report_assets/municipality_tail_risk_enriched.csv (per-municipality table)
#   report_assets/copula_segment_summary.csv     (selected copula family/segment)
#
# Produces:
#   B1 -> municipality_tail_risk_shrunk.csv       (EB-shrunk p95 and R_i^EB)
#   B2 -> copula_tail_dependence_ci.csv           (bootstrap 95% CI for theta/lambda_U)
#   B3 -> copula_independence_tests.csv           (Kendall / exchangeability / permutation)
#   B4 -> municipality_tail_risk_ci.csv           (block-bootstrap 95% CI for R_i)
#         municipality_rank_probabilities.csv     (P(top-10) / P(top-20))
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

required_pkgs <- c("dplyr", "copula", "lme4")
to_install <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

suppressPackageStartupMessages({
  library(dplyr)
  library(copula)
  library(lme4)
})

set.seed(20240517)

assets_dir <- "report_assets"
in_series_path <- file.path(assets_dir, "copula_input_series.csv")
enriched_path  <- file.path(assets_dir, "municipality_tail_risk_enriched.csv")
segsum_path    <- file.path(assets_dir, "copula_segment_summary.csv")

if (!file.exists(in_series_path) || !file.exists(enriched_path)) {
  stop("Run run_copula_tail_risk.R first: required CSV artefacts are missing.", call. = FALSE)
}

tail_df  <- read.csv(in_series_path, stringsAsFactors = FALSE)
enriched <- read.csv(enriched_path, stringsAsFactors = FALSE)
seg_sum  <- if (file.exists(segsum_path)) read.csv(segsum_path, stringsAsFactors = FALSE) else NULL

# coerce booleans (read.csv may import as character/logical depending on locale)
to_logical <- function(x) {
  if (is.logical(x)) return(x)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes")
}
for (col in c("energy_upper_90", "tourism_upper_90", "joint_upper_90",
              "energy_upper_95", "tourism_upper_95", "joint_upper_95")) {
  tail_df[[col]] <- to_logical(tail_df[[col]])
}

# Production composite-score weights (must match run_copula_tail_risk.R)
W95 <- 0.65; W90 <- 0.25; WSEV <- 0.10

# -----------------------------------------------------------------------------
# B1. Empirical-Bayes shrinkage of p_i(0.95) via a binomial random-intercept GLMM
# -----------------------------------------------------------------------------
# Among tourism-upper-95 events in each municipality, model the probability that
# the event is jointly an energy-upper-95 event. Shrinks low-count municipalities
# toward the regional mean.
b1 <- enriched %>%
  transmute(
    comune_key,
    comune_nome,
    risk_rank,
    n_trials95 = as.integer(n_tourism_upper_95),
    n_succ95   = as.integer(n_joint_upper_95),
    p95_raw    = p_energy_given_tourism_upper_95,
    p90_raw    = p_energy_given_tourism_upper_90,
    sev95_raw  = mean_joint_severity_95,
    risk_score_raw = risk_score
  )

fit_df <- b1 %>% filter(n_trials95 > 0) %>%
  mutate(n_fail95 = n_trials95 - n_succ95)

glmm <- lme4::glmer(
  cbind(n_succ95, n_fail95) ~ 1 + (1 | comune_key),
  data = fit_df, family = binomial,
  control = lme4::glmerControl(optimizer = "bobyqa")
)

pop_logit <- as.numeric(lme4::fixef(glmm)[["(Intercept)"]])
re <- lme4::ranef(glmm)$comune_key
re_map <- setNames(re[, 1], rownames(re))

b1 <- b1 %>%
  mutate(
    p95_eb = ifelse(
      n_trials95 > 0 & comune_key %in% names(re_map),
      plogis(pop_logit + re_map[comune_key]),
      plogis(pop_logit)            # no q95 tourism events -> regional baseline
    ),
    # recompute the composite risk score with the shrunk q95 conditional prob
    risk_score_eb = dplyr::coalesce(p95_eb, 0) * W95 +
      dplyr::coalesce(p90_raw, 0) * W90 +
      dplyr::coalesce(sev95_raw, 0) * WSEV
  ) %>%
  arrange(desc(risk_score_eb)) %>%
  mutate(risk_rank_eb = row_number())

regional_baseline_p95 <- plogis(pop_logit)
message(sprintf("B1: regional baseline p95 = %.4f (logit = %.3f)", regional_baseline_p95, pop_logit))

write.csv(
  b1 %>% select(comune_key, comune_nome, n_trials95, n_succ95,
                p95_raw, p95_eb, p90_raw, sev95_raw,
                risk_score_raw, risk_score_eb, risk_rank, risk_rank_eb),
  file = file.path(assets_dir, "municipality_tail_risk_shrunk.csv"), row.names = FALSE
)

# document how the top-10 moved after shrinkage
top10_raw <- b1 %>% arrange(risk_rank) %>% slice_head(n = 10) %>% pull(comune_key)
top10_eb  <- b1 %>% arrange(risk_rank_eb) %>% slice_head(n = 10) %>% pull(comune_key)
message(sprintf("B1: top-10 overlap raw vs EB = %d/10; entered=%s; left=%s",
                length(intersect(top10_raw, top10_eb)),
                paste(setdiff(top10_eb, top10_raw), collapse = ", "),
                paste(setdiff(top10_raw, top10_eb), collapse = ", ")))

# -----------------------------------------------------------------------------
# Segment definitions (must mirror run_copula_tail_risk.R)
# -----------------------------------------------------------------------------
segment_keys <- list(
  all_comuni = unique(tail_df$comune_key)
)
if (all(c("risk_rank") %in% names(enriched))) {
  segment_keys$top10_tail_risk <- enriched %>% arrange(risk_rank) %>%
    slice_head(n = 10) %>% pull(comune_key)
}
if ("tourism_intensity_rank" %in% names(enriched)) {
  segment_keys$top10_tourism_intensity <- enriched %>%
    filter(!is.na(tourism_intensity_rank)) %>%
    arrange(tourism_intensity_rank) %>% slice_head(n = 10) %>% pull(comune_key)
}

selected_family_for <- function(seg) {
  if (!is.null(seg_sum) && seg %in% seg_sum$sample) {
    return(seg_sum$selected_family[match(seg, seg_sum$sample)])
  }
  "gumbel"
}

make_copula <- function(family_name) {
  switch(family_name,
    gaussian  = normalCopula(param = 0.2, dim = 2),
    student_t = tCopula(param = 0.2, dim = 2, dispstr = "un", df = 6, df.fixed = FALSE),
    clayton   = claytonCopula(param = 1.2, dim = 2),
    gumbel    = gumbelCopula(param = 1.2, dim = 2),
    frank     = frankCopula(param = 1.0, dim = 2),
    gumbelCopula(param = 1.2, dim = 2)
  )
}

# refit a copula on a (u_energy, u_tourism) matrix; return theta + upper lambda.
# method = "ml" for the one-time point estimate; method = "itau" (Kendall's-tau
# inversion, closed-form and near-instant) for the bootstrap resamples, which
# makes B = 1000 feasible on the pooled ~5900-row sample.
fit_lambda <- function(u_mat, family_name, method = "ml") {
  fit <- tryCatch(
    fitCopula(make_copula(family_name), data = u_mat, method = method,
              estimate.variance = FALSE),
    error = function(e) NULL)
  if (is.null(fit) || length(fit@estimate) == 0) {
    return(c(theta = NA_real_, lambda_upper = NA_real_, lambda_lower = NA_real_))
  }
  lam <- tryCatch(copula::lambda(fit@copula),
                  error = function(e) c(lower = NA_real_, upper = NA_real_))
  c(theta = as.numeric(fit@estimate[1]),
    lambda_upper = unname(lam["upper"]),
    lambda_lower = unname(lam["lower"]))
}

# pseudo-observations recomputed within the resampled subset
pseudo_obs <- function(df) {
  n <- nrow(df)
  cbind(
    u_energy  = rank(df$energy_residual, ties.method = "average") / (n + 1),
    u_tourism = rank(df$tourism_shock,  ties.method = "average") / (n + 1)
  )
}

# -----------------------------------------------------------------------------
# B2. Nonparametric pair bootstrap (B = 1000) for theta and lambda_U
# -----------------------------------------------------------------------------
B_COPULA <- 1000

n_cores <- 1L
cl <- NULL

boot_copula_ci <- function(seg_name) {
  keys <- segment_keys[[seg_name]]
  df <- tail_df %>% filter(comune_key %in% keys)
  fam <- selected_family_for(seg_name)
  n <- nrow(df)
  if (n < 50) return(NULL)

  point <- fit_lambda(pseudo_obs(df), fam, method = "ml")
  point_itau <- fit_lambda(pseudo_obs(df), fam, method = "itau")

  boot_mat <- matrix(NA_real_, nrow = B_COPULA, ncol = 3,
                     dimnames = list(NULL, c("theta", "lambda_upper", "lambda_lower")))
  for (b in seq_len(B_COPULA)) {
    idx <- sample.int(n, n, replace = TRUE)
    boot_mat[b, ] <- fit_lambda(pseudo_obs(df[idx, , drop = FALSE]), fam, method = "itau")
  }

  ci <- function(v) stats::quantile(v, c(0.025, 0.975), na.rm = TRUE)
  th_ci  <- ci(boot_mat[, "theta"])
  lu_ci  <- ci(boot_mat[, "lambda_upper"])
  data.frame(
    segment = seg_name,
    selected_family = fam,
    n_obs = n,
    theta_point_ml = unname(point["theta"]),
    theta_point_itau = unname(point_itau["theta"]),
    theta_lo = unname(th_ci[1]), theta_hi = unname(th_ci[2]),
    lambda_upper_point_ml = unname(point["lambda_upper"]),
    lambda_upper_point = unname(point_itau["lambda_upper"]),
    lambda_upper_lo = unname(lu_ci[1]), lambda_upper_hi = unname(lu_ci[2]),
    lambda_upper_ci_includes_zero = (lu_ci[1] <= 0),
    stringsAsFactors = FALSE
  )
}

message(sprintf("B2: bootstrapping copula tail dependence (B = %d) across %d segments...",
                B_COPULA, length(segment_keys)))
copula_ci_tbl <- bind_rows(lapply(names(segment_keys), boot_copula_ci))
write.csv(copula_ci_tbl, file = file.path(assets_dir, "copula_tail_dependence_ci.csv"),
          row.names = FALSE)
for (i in seq_len(nrow(copula_ci_tbl))) {
  message(sprintf("B2: %-22s lambda_U = %.3f [%.3f, %.3f]%s",
                  copula_ci_tbl$segment[i], copula_ci_tbl$lambda_upper_point[i],
                  copula_ci_tbl$lambda_upper_lo[i], copula_ci_tbl$lambda_upper_hi[i],
                  ifelse(copula_ci_tbl$lambda_upper_ci_includes_zero[i], "  <-- CI INCLUDES 0", "")))
}

# -----------------------------------------------------------------------------
# B3. Dependence / independence / exchangeability tests
# -----------------------------------------------------------------------------
B_PERM <- 2000

run_independence_tests <- function(seg_name) {
  keys <- segment_keys[[seg_name]]
  df <- tail_df %>% filter(comune_key %in% keys)
  if (nrow(df) < 50) return(NULL)
  u <- pseudo_obs(df)

  # Kendall tau test (asymptotic p-value; statistic is O(n^2) but computed once)
  kt <- suppressWarnings(cor.test(u[, 1], u[, 2], method = "kendall"))
  tau_obs <- unname(kt$estimate)

  # Permutation independence test. Kendall's tau is O(n^2) per permutation and
  # infeasible at the pooled sample size, so the permutation null uses Spearman's
  # rho (O(n log n)), a monotone-association statistic equivalent for testing
  # independence of the ranks.
  rho_obs <- suppressWarnings(cor(u[, 1], u[, 2], method = "spearman"))
  perm_rho <- replicate(B_PERM, {
    suppressWarnings(cor(u[, 1], sample(u[, 2]), method = "spearman"))
  })
  perm_p <- (1 + sum(abs(perm_rho) >= abs(rho_obs))) / (B_PERM + 1)

  # Exchangeability test (copula::exchTest)
  exch <- tryCatch(copula::exchTest(u, N = 300),
                   error = function(e) NULL)
  exch_stat <- if (is.null(exch)) NA_real_ else as.numeric(exch$statistic)
  exch_p    <- if (is.null(exch)) NA_real_ else as.numeric(exch$p.value)

  data.frame(
    segment = seg_name,
    n_obs = nrow(df),
    kendall_tau = unname(tau_obs),
    kendall_p_value = unname(kt$p.value),
    spearman_rho = unname(rho_obs),
    perm_indep_p_value = perm_p,
    exch_statistic = exch_stat,
    exch_p_value = exch_p,
    stringsAsFactors = FALSE
  )
}

message("B3: running Kendall / permutation / exchangeability tests...")
indep_tbl <- bind_rows(lapply(names(segment_keys), run_independence_tests))
write.csv(indep_tbl, file = file.path(assets_dir, "copula_independence_tests.csv"),
          row.names = FALSE)

# -----------------------------------------------------------------------------
# B4. Block (cluster) bootstrap for per-municipality R_i CI and rank probabilities
# -----------------------------------------------------------------------------
# Resample observations within each municipality (preserving its sample size),
# recompute the composite risk score from the precomputed exceedance indicators,
# re-rank all municipalities, and accumulate percentile CIs and rank frequencies.
B_RANK <- 1000

comunes <- sort(unique(tail_df$comune_key))
M <- length(comunes)
row_idx_by_comune <- split(seq_len(nrow(tail_df)), tail_df$comune_key)

ue <- tail_df$u_energy; ut <- tail_df$u_tourism
t90 <- tail_df$tourism_upper_90; j90 <- tail_df$joint_upper_90
t95 <- tail_df$tourism_upper_95; j95 <- tail_df$joint_upper_95

score_one <- function(idx) {
  n <- length(idx)
  nt95 <- sum(t95[idx]); nj95 <- sum(j95[idx])
  nt90 <- sum(t90[idx]); nj90 <- sum(j90[idx])
  p95 <- if (nt95 > 0) nj95 / nt95 else 0
  p90 <- if (nt90 > 0) nj90 / nt90 else 0
  jmask <- j95[idx]
  sev95 <- if (any(jmask)) mean((ue[idx][jmask] + ut[idx][jmask]) / 2) else 0
  p95 * W95 + p90 * W90 + sev95 * WSEV
}

risk_boot <- matrix(NA_real_, nrow = B_RANK, ncol = M, dimnames = list(NULL, comunes))
for (b in seq_len(B_RANK)) {
  scores <- numeric(M)
  for (m in seq_len(M)) {
    base_idx <- row_idx_by_comune[[comunes[m]]]
    res_idx <- base_idx[sample.int(length(base_idx), length(base_idx), replace = TRUE)]
    scores[m] <- score_one(res_idx)
  }
  risk_boot[b, ] <- scores
}

# rank within each replicate (1 = highest risk)
rank_boot <- t(apply(risk_boot, 1, function(s) rank(-s, ties.method = "min")))

ci_lo <- apply(risk_boot, 2, stats::quantile, probs = 0.025, na.rm = TRUE)
ci_hi <- apply(risk_boot, 2, stats::quantile, probs = 0.975, na.rm = TRUE)
ci_mean <- colMeans(risk_boot, na.rm = TRUE)

p_top10 <- colMeans(rank_boot <= 10)
p_top20 <- colMeans(rank_boot <= 20)

point_score <- setNames(enriched$risk_score, enriched$comune_key)[comunes]
point_rank  <- setNames(enriched$risk_rank,  enriched$comune_key)[comunes]
comune_nome <- setNames(enriched$comune_nome, enriched$comune_key)[comunes]

ci_tbl <- data.frame(
  comune_key = comunes,
  comune_nome = unname(comune_nome),
  risk_score_point = unname(point_score),
  risk_score_boot_mean = unname(ci_mean),
  risk_score_lo = unname(ci_lo),
  risk_score_hi = unname(ci_hi),
  risk_rank_point = unname(point_rank),
  stringsAsFactors = FALSE
) %>% arrange(risk_rank_point)
write.csv(ci_tbl, file = file.path(assets_dir, "municipality_tail_risk_ci.csv"),
          row.names = FALSE)

rank_prob_tbl <- data.frame(
  comune_key = comunes,
  comune_nome = unname(comune_nome),
  risk_rank_point = unname(point_rank),
  p_top10 = unname(p_top10),
  p_top20 = unname(p_top20),
  stringsAsFactors = FALSE
) %>% arrange(desc(p_top10), desc(p_top20))
write.csv(rank_prob_tbl, file = file.path(assets_dir, "municipality_rank_probabilities.csv"),
          row.names = FALSE)

message("Phase B complete. CSVs written to ", normalizePath(assets_dir))
message(sprintf("  - lambda_U CIs including 0: %d/%d segments",
                sum(copula_ci_tbl$lambda_upper_ci_includes_zero, na.rm = TRUE),
                nrow(copula_ci_tbl)))
