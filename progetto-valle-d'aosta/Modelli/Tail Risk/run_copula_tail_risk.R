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
setwd(script_dir)

source(file.path("..", "modelling_fixed.R"), local = TRUE)

required_pkgs <- c("dplyr", "copula")
to_install <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(dplyr)
library(copula)

dir.create("report_assets", showWarnings = FALSE)

normalize_key <- function(x) {
  out <- trimws(tolower(as.character(x)))
  gsub("\\s+", " ", out)
}

first_non_missing <- function(x) {
  idx <- which(!is.na(x) & trimws(as.character(x)) != "")
  if (length(idx) == 0) {
    return(NA)
  }
  x[idx[1]]
}

if (!all(c("resid_final", "arrivi_destag_weather_z", "comune_key") %in% names(analysis_df))) {
  stop("Le serie necessarie per il tail risk non sono disponibili in analysis_df.", call. = FALSE)
}

tail_df <- analysis_df %>%
  transmute(
    obs_id = dplyr::row_number(),
    comune_key = as.character(comune_key),
    energy_residual = as.numeric(resid_final),
    tourism_shock = as.numeric(arrivi_destag_weather_z)
  ) %>%
  filter(is.finite(energy_residual), is.finite(tourism_shock))

if (nrow(tail_df) < 50) {
  stop("Osservazioni insufficienti per stimare le copule in modo stabile.", call. = FALSE)
}

tail_df <- tail_df %>%
  mutate(
    u_energy = rank(energy_residual, ties.method = "average") / (n() + 1),
    u_tourism = rank(tourism_shock, ties.method = "average") / (n() + 1)
  )

upper_q90 <- 0.90
upper_q95 <- 0.95

tail_df <- tail_df %>%
  mutate(
    energy_upper_90 = u_energy >= upper_q90,
    tourism_upper_90 = u_tourism >= upper_q90,
    joint_upper_90 = energy_upper_90 & tourism_upper_90,
    energy_upper_95 = u_energy >= upper_q95,
    tourism_upper_95 = u_tourism >= upper_q95,
    joint_upper_95 = energy_upper_95 & tourism_upper_95
  )

u_data <- as.matrix(tail_df[, c("u_energy", "u_tourism")])

fit_specifications <- list(
  gaussian = normalCopula(param = 0.2, dim = 2),
  student_t = tCopula(param = 0.2, dim = 2, dispstr = "un", df = 6, df.fixed = FALSE),
  clayton = claytonCopula(param = 1.2, dim = 2),
  gumbel = gumbelCopula(param = 1.2, dim = 2),
  frank = frankCopula(param = 1.0, dim = 2)
)

fit_one_copula <- function(family_name, copula_model, u_matrix) {
  fit <- tryCatch(
    fitCopula(copula_model, data = u_matrix, method = "ml"),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(NULL)
  }

  if (length(fit@estimate) == 0) {
    return(NULL)
  }

  fitted_copula <- fit@copula
  theta_names <- names(fit@estimate)
  if (is.null(theta_names) || any(theta_names == "")) {
    theta_names <- paste0("param_", seq_along(fit@estimate))
  }

  var_est <- fit@var.est %||% rep(NA_real_, length(fit@estimate))
  if (length(var_est) == 0) {
    var_est <- rep(NA_real_, length(fit@estimate))
  }

  lambda_vals <- tryCatch(copula::lambda(fitted_copula), error = function(e) c(lower = NA_real_, upper = NA_real_))

  list(
    family = family_name,
    fit = fit,
    copula = fitted_copula,
    metrics = data.frame(
      family = family_name,
      n_obs = nrow(u_matrix),
      logLik = as.numeric(logLik(fit)),
      AIC = AIC(fit),
      BIC = BIC(fit),
      n_parameters = attr(logLik(fit), "df"),
      lower_tail = unname(lambda_vals[1]),
      upper_tail = unname(lambda_vals[2]),
      stringsAsFactors = FALSE
    ),
    parameters = data.frame(
      family = family_name,
      parameter = theta_names,
      estimate = as.numeric(fit@estimate),
      std_error = sqrt(as.numeric(var_est)),
      stringsAsFactors = FALSE
    )
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

fit_copula_suite <- function(input_df, sample_label) {
  if (nrow(input_df) < 50) {
    return(NULL)
  }

  # Recompute empirical margins within every segment; pooled ranks are not
  # uniform after restricting the sample to a municipality subset.
  u_matrix <- cbind(
    rank(input_df$energy_residual, ties.method = "average"),
    rank(input_df$tourism_shock, ties.method = "average")
  ) / (nrow(input_df) + 1)

  fit_results <- lapply(names(fit_specifications), function(family_name) {
    fit_one_copula(family_name, fit_specifications[[family_name]], u_matrix)
  })
  fit_results <- Filter(Negate(is.null), fit_results)

  if (length(fit_results) == 0) {
    return(NULL)
  }

  selection_tbl_local <- bind_rows(lapply(fit_results, `[[`, "metrics")) %>%
    mutate(sample = sample_label) %>%
    arrange(AIC, BIC)

  best_family_local <- selection_tbl_local$family[[1]]
  best_result_local <- fit_results[[match(best_family_local, vapply(fit_results, `[[`, character(1), "family"))]]

  parameters_tbl_local <- best_result_local$parameters %>%
    mutate(
      sample = sample_label,
      selected_family = best_family_local,
      lower_tail = selection_tbl_local$lower_tail[match(best_family_local, selection_tbl_local$family)],
      upper_tail = selection_tbl_local$upper_tail[match(best_family_local, selection_tbl_local$family)]
    )

  summary_tbl_local <- selection_tbl_local %>%
    slice(1) %>%
    transmute(
      sample = sample,
      n_obs = n_obs,
      selected_family = family,
      logLik = logLik,
      AIC = AIC,
      BIC = BIC,
      lower_tail = lower_tail,
      upper_tail = upper_tail
    )

  list(
    fit_results = fit_results,
    selection = selection_tbl_local,
    parameters = parameters_tbl_local,
    summary = summary_tbl_local,
    best_family = best_family_local,
    best_result = best_result_local
  )
}

all_result <- fit_copula_suite(tail_df, "all_comuni")

if (is.null(all_result)) {
  stop("Nessuna copula e' stata stimata con successo.", call. = FALSE)
}

selection_tbl <- all_result$selection %>% select(-sample)
best_family <- all_result$best_family
best_result <- all_result$best_result
best_parameters_tbl <- all_result$parameters %>% select(-sample)

write.csv(selection_tbl, file = file.path("report_assets", "copula_selection.csv"), row.names = FALSE)
write.csv(best_parameters_tbl, file = file.path("report_assets", "copula_parameters.csv"), row.names = FALSE)
write.csv(tail_df, file = file.path("report_assets", "copula_input_series.csv"), row.names = FALSE)

municipality_tail_tbl <- tail_df %>%
  group_by(comune_key) %>%
  summarise(
    n_obs = dplyr::n(),
    n_tourism_upper_90 = sum(tourism_upper_90),
    n_energy_upper_90 = sum(energy_upper_90),
    n_joint_upper_90 = sum(joint_upper_90),
    share_joint_upper_90 = mean(joint_upper_90),
    p_energy_given_tourism_upper_90 = ifelse(n_tourism_upper_90 > 0, n_joint_upper_90 / n_tourism_upper_90, NA_real_),
    lift_upper_90 = ifelse(mean(tourism_upper_90) > 0, mean(joint_upper_90) / (mean(tourism_upper_90) * 0.10), NA_real_),
    mean_joint_severity_90 = ifelse(n_joint_upper_90 > 0, mean((u_energy[joint_upper_90] + u_tourism[joint_upper_90]) / 2), NA_real_),
    kendall_tau = suppressWarnings(cor(u_energy, u_tourism, method = "kendall")),
    n_tourism_upper_95 = sum(tourism_upper_95),
    n_energy_upper_95 = sum(energy_upper_95),
    n_joint_upper_95 = sum(joint_upper_95),
    share_joint_upper_95 = mean(joint_upper_95),
    p_energy_given_tourism_upper_95 = ifelse(n_tourism_upper_95 > 0, n_joint_upper_95 / n_tourism_upper_95, NA_real_),
    lift_upper_95 = ifelse(mean(tourism_upper_95) > 0, mean(joint_upper_95) / (mean(tourism_upper_95) * 0.05), NA_real_),
    mean_joint_severity_95 = ifelse(n_joint_upper_95 > 0, mean((u_energy[joint_upper_95] + u_tourism[joint_upper_95]) / 2), NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    risk_score = dplyr::coalesce(p_energy_given_tourism_upper_95, 0) * 0.65 +
      dplyr::coalesce(p_energy_given_tourism_upper_90, 0) * 0.25 +
      dplyr::coalesce(mean_joint_severity_95, 0) * 0.10
  ) %>%
  arrange(desc(risk_score), desc(n_joint_upper_95), desc(n_joint_upper_90), desc(kendall_tau))

tourism_profile <- NULL
if (exists("data_fe") && all(c("comune_key", "comune_nome", "istat_muni_code", "totale_arrivi", "residenti", "kwh") %in% names(data_fe))) {
  tourism_profile <- data_fe %>%
    group_by(comune_key) %>%
    summarise(
      comune_nome = first_non_missing(as.character(comune_nome)),
      istat_muni_code = as.character(first_non_missing(istat_muni_code)),
      mean_arrivi = mean(totale_arrivi, na.rm = TRUE),
      mean_residenti = mean(residenti, na.rm = TRUE),
      mean_kwh = mean(kwh, na.rm = TRUE),
      arrivi_per_residente = ifelse(mean_residenti > 0, mean_arrivi / mean_residenti, NA_real_),
      .groups = "drop"
    ) %>%
    mutate(comune_key_norm = normalize_key(comune_key))
}

municipality_tail_enriched_tbl <- municipality_tail_tbl
if (!is.null(tourism_profile)) {
  municipality_tail_enriched_tbl <- municipality_tail_tbl %>%
    mutate(comune_key_norm = normalize_key(comune_key)) %>%
    left_join(
      tourism_profile %>% select(comune_key_norm, comune_nome, istat_muni_code, mean_arrivi, mean_residenti, mean_kwh, arrivi_per_residente),
      by = "comune_key_norm"
    ) %>%
    mutate(
      tourism_intensity_rank = dense_rank(desc(arrivi_per_residente)),
      risk_rank = row_number()
    ) %>%
    select(-comune_key_norm)
} else {
  municipality_tail_enriched_tbl <- municipality_tail_tbl %>%
    mutate(risk_rank = row_number())
}

segment_results <- list(all_comuni = all_result)

top_risk_keys <- municipality_tail_enriched_tbl %>%
  slice_head(n = 10) %>%
  pull(comune_key)

top_risk_df <- tail_df %>% filter(comune_key %in% top_risk_keys)
top_risk_result <- fit_copula_suite(top_risk_df, "top10_tail_risk")
if (!is.null(top_risk_result)) {
  segment_results$top10_tail_risk <- top_risk_result
}

if (!is.null(tourism_profile)) {
  top_tourism_keys <- tourism_profile %>%
    arrange(desc(arrivi_per_residente)) %>%
    slice_head(n = 10) %>%
    pull(comune_key)

  top_tourism_df <- tail_df %>% filter(comune_key %in% top_tourism_keys)
  top_tourism_result <- fit_copula_suite(top_tourism_df, "top10_tourism_intensity")
  if (!is.null(top_tourism_result)) {
    segment_results$top10_tourism_intensity <- top_tourism_result
  }
}

segment_selection_tbl <- bind_rows(lapply(segment_results, `[[`, "selection"))
segment_parameters_tbl <- bind_rows(lapply(segment_results, `[[`, "parameters"))
segment_summary_tbl <- bind_rows(lapply(segment_results, `[[`, "summary"))

tourism_vs_risk_tbl <- NULL
if (!is.null(tourism_profile)) {
  tourism_vs_risk_tbl <- municipality_tail_enriched_tbl %>%
    select(comune_key, comune_nome, risk_rank, risk_score, tourism_intensity_rank, arrivi_per_residente, n_joint_upper_95, p_energy_given_tourism_upper_95, kendall_tau) %>%
    arrange(risk_rank, tourism_intensity_rank)

  overlap_tbl <- data.frame(
    metric = c(
      "correlation_risk_vs_arrivi_per_residente",
      "top10_overlap_count",
      "top10_overlap_share"
    ),
    value = c(
      suppressWarnings(cor(municipality_tail_enriched_tbl$risk_score, municipality_tail_enriched_tbl$arrivi_per_residente, use = "complete.obs")),
      length(intersect(top_risk_keys, tourism_profile %>% arrange(desc(arrivi_per_residente)) %>% slice_head(n = 10) %>% pull(comune_key))),
      length(intersect(top_risk_keys, tourism_profile %>% arrange(desc(arrivi_per_residente)) %>% slice_head(n = 10) %>% pull(comune_key))) / length(top_risk_keys)
    )
  )

  write.csv(tourism_vs_risk_tbl, file = file.path("report_assets", "municipality_tail_risk_vs_tourism.csv"), row.names = FALSE)
  write.csv(overlap_tbl, file = file.path("report_assets", "municipality_tail_risk_overlap_summary.csv"), row.names = FALSE)
}

write.csv(municipality_tail_tbl, file = file.path("report_assets", "municipality_tail_risk.csv"), row.names = FALSE)
write.csv(utils::head(municipality_tail_tbl, 15), file = file.path("report_assets", "municipality_tail_risk_top15.csv"), row.names = FALSE)
write.csv(municipality_tail_enriched_tbl, file = file.path("report_assets", "municipality_tail_risk_enriched.csv"), row.names = FALSE)
write.csv(segment_selection_tbl, file = file.path("report_assets", "copula_selection_by_segment.csv"), row.names = FALSE)
write.csv(segment_parameters_tbl, file = file.path("report_assets", "copula_parameters_by_segment.csv"), row.names = FALSE)
write.csv(segment_summary_tbl, file = file.path("report_assets", "copula_segment_summary.csv"), row.names = FALSE)

grid_values <- seq(0.01, 0.99, length.out = 60)
grid_df <- expand.grid(u_energy = grid_values, u_tourism = grid_values)
grid_density <- matrix(
  dCopula(as.matrix(grid_df), best_result$copula),
  nrow = length(grid_values),
  ncol = length(grid_values)
)

png(file.path("report_assets", "copula_contour_plot.png"), width = 1400, height = 1000, res = 150)
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  tail_df$u_energy, tail_df$u_tourism,
  pch = 16,
  cex = 0.55,
  col = grDevices::rgb(0.1, 0.1, 0.1, 0.30),
  xlab = "Pseudo-osservazioni shock energetici",
  ylab = "Pseudo-osservazioni shock turismo",
  main = paste("Copula vincitrice:", best_family)
)
contour(
  x = grid_values,
  y = grid_values,
  z = grid_density,
  add = TRUE,
  drawlabels = FALSE,
  nlevels = 8,
  col = "firebrick3",
  lwd = 1.2
)
legend(
  "topleft",
  legend = c(
    paste("Family:", best_family),
    paste("AIC:", round(selection_tbl$AIC[[1]], 2)),
    paste("Lower tail:", round(selection_tbl$lower_tail[[1]], 3)),
    paste("Upper tail:", round(selection_tbl$upper_tail[[1]], 3))
  ),
  bty = "n"
)
dev.off()

plot_tbl <- utils::head(municipality_tail_tbl, 20)

png(file.path("report_assets", "municipality_tail_risk_top20.png"), width = 1600, height = 1000, res = 150)
par(mar = c(10, 5, 4, 2) + 0.1)
barplot(
  height = plot_tbl$risk_score,
  names.arg = plot_tbl$comune_key,
  las = 2,
  col = "tomato3",
  border = NA,
  main = "Top 20 comuni per tail risk congiunto turismo-energia",
  ylab = "Risk score"
)
dev.off()

cat("Copula tail risk completato. Asset salvati in Modelli/Tail Risk/report_assets\n")
