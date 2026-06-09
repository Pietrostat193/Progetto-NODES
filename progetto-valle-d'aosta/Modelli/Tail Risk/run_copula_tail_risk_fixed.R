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

source(file.path("..", "modelling_fixed_nobom.R"), local = TRUE)

required_pkgs <- c("dplyr", "copula")
to_install <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(dplyr)
library(copula)

dir.create("report_assets", showWarnings = FALSE)

if (!all(c("resid_final", "arrivi_destag_weather_z", "date", "comune_key", "comune_nome") %in% names(analysis_df))) {
  stop("Le serie necessarie per il tail risk non sono disponibili in analysis_df.", call. = FALSE)
}

tail_df <- analysis_df %>%
  transmute(
    date = as.Date(date),
    comune_key = as.character(comune_key),
    comune_nome = as.character(comune_nome),
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

  fitted_copula <- fit@copula
  theta_names <- names(fit@estimate)
  if (is.null(theta_names) || any(theta_names == "")) {
    theta_names <- paste0("param_", seq_along(fit@estimate))
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
      std_error = as.numeric(fit@var.est %||% rep(NA_real_, length(fit@estimate))) ^ 0.5,
      stringsAsFactors = FALSE
    )
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

fit_results <- lapply(names(fit_specifications), function(family_name) {
  fit_one_copula(family_name, fit_specifications[[family_name]], u_data)
})
fit_results <- Filter(Negate(is.null), fit_results)

if (length(fit_results) == 0) {
  stop("Nessuna copula e' stata stimata con successo.", call. = FALSE)
}

selection_tbl <- bind_rows(lapply(fit_results, `[[`, "metrics")) %>%
  arrange(AIC, BIC)

best_family <- selection_tbl$family[[1]]
best_result <- fit_results[[match(best_family, vapply(fit_results, `[[`, character(1), "family"))]]

best_parameters_tbl <- best_result$parameters %>%
  mutate(
    selected_family = best_family,
    lower_tail = selection_tbl$lower_tail[match(best_family, selection_tbl$family)],
    upper_tail = selection_tbl$upper_tail[match(best_family, selection_tbl$family)]
  )

write.csv(selection_tbl, file = file.path("report_assets", "copula_selection.csv"), row.names = FALSE)
write.csv(best_parameters_tbl, file = file.path("report_assets", "copula_parameters.csv"), row.names = FALSE)
write.csv(tail_df, file = file.path("report_assets", "copula_input_series.csv"), row.names = FALSE)

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

cat("Copula tail risk completato. Asset salvati in Modelli/Tail Risk/report_assets\n")
