setwd(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]))))

source("modelling.R", local = TRUE)

dir.create("report_assets", showWarnings = FALSE)

comparison_table_out <- as.data.frame(comparison_table)
write.csv(comparison_table_out, file = file.path("report_assets", "comparison_table.csv"), row.names = FALSE)

fixed_effects_out <- as.data.frame(fixed_effects_table)
write.csv(fixed_effects_out, file = file.path("report_assets", "fixed_effects_table.csv"), row.names = FALSE)

nested_anova_out <- as.data.frame(anova(m0, m0_pop, m1, m2, m3, m4))
nested_anova_out$model <- rownames(nested_anova_out)
rownames(nested_anova_out) <- NULL
nested_anova_out <- nested_anova_out[, c("model", setdiff(names(nested_anova_out), "model"))]
write.csv(nested_anova_out, file = file.path("report_assets", "nested_anova.csv"), row.names = FALSE)

if (exists("m4_arrivi_destag")) {
  arrivi_compare_out <- as.data.frame(anova(m3, m4, m4_arrivi_destag))
  arrivi_compare_out$model <- rownames(arrivi_compare_out)
  rownames(arrivi_compare_out) <- NULL
  arrivi_compare_out <- arrivi_compare_out[, c("model", setdiff(names(arrivi_compare_out), "model"))]
  write.csv(arrivi_compare_out, file = file.path("report_assets", "arrivi_compare.csv"), row.names = FALSE)

  extract_aux_fixed <- function(model_obj, model_name) {
    out <- as.data.frame(coef(summary(model_obj)))
    out$term <- rownames(out)
    rownames(out) <- NULL
    names(out) <- sub("^Estimate$", "estimate", names(out))
    names(out) <- sub("^Std\\. Error$", "std.error", names(out))
    names(out) <- sub("^t value$", "statistic", names(out))
    names(out) <- sub("^Pr\\(>\\|t\\|\\)$", "p.value", names(out))
    out$model <- model_name
    out$engine <- "lmerTest"
    if (!"df" %in% names(out)) {
      out$df <- df.residual(model_obj)
    }
    out[, c("model", "engine", "term", "estimate", "std.error", "df", "statistic", "p.value")]
  }

  arrivi_identification_out <- rbind(
    subset(fixed_effects_out, model == "M4_turismo" & term == "log_totale_arrivi_z"),
    subset(extract_aux_fixed(m4_arrivi_destag, "M4_arrivi_destag"), term == "arrivi_destag_weather_z")
  )
  write.csv(arrivi_identification_out, file = file.path("report_assets", "arrivi_identification.csv"), row.names = FALSE)
}

kwh_quality <- data.frame(
  metric = c("n_obs", "n_missing_kwh", "n_negative_kwh", "n_negative_kwh_treated_na", "n_zero_kwh"),
  value = c(
    nrow(data),
    sum(is.na(data$kwh)),
    sum(data$kwh < 0, na.rm = TRUE),
    sum(data$kwh < 0, na.rm = TRUE),
    sum(data$kwh == 0, na.rm = TRUE)
  )
)
write.csv(kwh_quality, file = file.path("report_assets", "kwh_quality.csv"), row.names = FALSE)

save_coef_plot <- function(model_name, output_name, display_name = model_name) {
  coef_df <- subset(fixed_effects_out, model == model_name & term != "(Intercept)")
  if (nrow(coef_df) == 0) {
    return(invisible(NULL))
  }

  coef_df <- coef_df[order(coef_df$estimate), ]
  lower <- coef_df$estimate - 1.96 * coef_df$std.error
  upper <- coef_df$estimate + 1.96 * coef_df$std.error

  png(file.path("report_assets", output_name), width = 1400, height = 900, res = 150)
  par(mar = c(5, 12, 4, 2))
  plot(
    coef_df$estimate,
    seq_len(nrow(coef_df)),
    xlim = range(c(lower, upper), na.rm = TRUE),
    yaxt = "n",
    ylab = "",
    xlab = "Stima coefficiente (IC 95%)",
    main = paste("Coefficienti fixed effects -", display_name),
    pch = 19,
    col = ifelse(is.na(coef_df$p.value), "gray40", ifelse(coef_df$p.value < 0.05, "firebrick", "steelblue"))
  )
  segments(lower, seq_len(nrow(coef_df)), upper, seq_len(nrow(coef_df)), col = "gray50", lwd = 2)
  abline(v = 0, lty = 2, col = "gray40")
  axis(2, at = seq_len(nrow(coef_df)), labels = coef_df$term, las = 2)
  dev.off()
}

save_coef_plot("M1_meteo", "coef_m1_meteo.png")
save_coef_plot("M0_popolazione", "coef_m0_popolazione.png")
save_coef_plot("M2_calendario", "coef_m2_calendario.png")
save_coef_plot("M3_dinamica", "coef_m3_dinamica.png")
save_coef_plot("M4_turismo", "coef_m4_arrivi_turistici.png", "M4_arrivi_turistici")

png(file.path("report_assets", "model_comparison_metrics.png"), width = 1600, height = 1000, res = 150)
par(mfrow = c(2, 2), mar = c(7, 5, 4, 2))
barplot(comparison_table_out$AIC, names.arg = comparison_table_out$model, las = 2, col = "tan", main = "AIC per modello")
barplot(comparison_table_out$BIC, names.arg = comparison_table_out$model, las = 2, col = "wheat3", main = "BIC per modello")
matplot(
  x = seq_len(nrow(comparison_table_out)),
  y = cbind(comparison_table_out$R2_marginal, comparison_table_out$R2_conditional),
  type = "b",
  pch = c(19, 17),
  lty = 1,
  col = c("steelblue4", "darkorange3"),
  xaxt = "n",
  xlab = "Modello",
  ylab = expression(R^2),
  main = expression("R"^2 ~ "marginale e condizionale")
)
axis(1, at = seq_len(nrow(comparison_table_out)), labels = comparison_table_out$model, las = 2)
legend("topleft", legend = c("R2 marginale", "R2 condizionale"), col = c("steelblue4", "darkorange3"), pch = c(19, 17), lty = 1, bty = "n")
barplot(as.numeric(comparison_table_out$singular), names.arg = comparison_table_out$model, las = 2, col = "gray60", main = "Singolarita (1 = TRUE)")
dev.off()

if (exists("m4_arrivi_destag")) {
  arrivi_aic <- c(
    m3 = AIC(m3),
    m4_arrivi = AIC(m4),
    m4_arrivi_destag = AIC(m4_arrivi_destag)
  )
  arrivi_bic <- c(
    m3 = BIC(m3),
    m4_arrivi = BIC(m4),
    m4_arrivi_destag = BIC(m4_arrivi_destag)
  )

  png(file.path("report_assets", "arrivi_model_compare.png"), width = 1400, height = 900, res = 150)
  par(mfrow = c(1, 2), mar = c(7, 5, 4, 2))
  barplot(arrivi_aic, las = 2, col = "skyblue3", main = "AIC varianti arrivi turistici")
  barplot(arrivi_bic, las = 2, col = "seagreen3", main = "BIC varianti arrivi turistici")
  dev.off()
}

png(file.path("report_assets", "final_model_residuals.png"), width = 1400, height = 1000, res = 150)
par(mfrow = c(2, 2))
plot(fitted(m4), resid(m4), xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted - M4")
abline(h = 0, lty = 2)
qqnorm(resid(m4), main = "QQ plot residuals - M4")
qqline(resid(m4))
hist(resid(m4), breaks = 40, main = "Histogram residuals - M4", xlab = "Residuals")
acf(resid(m4), main = "ACF residuals - M4")
dev.off()
