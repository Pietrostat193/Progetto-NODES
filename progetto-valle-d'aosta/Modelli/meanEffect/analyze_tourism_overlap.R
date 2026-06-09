setwd(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]))))

source("modelling.R", local = TRUE)

dir.create("report_assets", showWarnings = FALSE)

df_base <- analysis_df

fit_with_summary <- function(formula_obj, label, data) {
  model <- lmerTest::lmer(formula_obj, data = data, REML = FALSE, control = ctrl)
  fixed_tbl <- as.data.frame(coef(summary(model)))
  fixed_tbl$term <- rownames(fixed_tbl)
  rownames(fixed_tbl) <- NULL
  names(fixed_tbl) <- sub("^Estimate$", "estimate", names(fixed_tbl))
  names(fixed_tbl) <- sub("^Std\\. Error$", "std.error", names(fixed_tbl))
  names(fixed_tbl) <- sub("^t value$", "statistic", names(fixed_tbl))
  names(fixed_tbl) <- sub("^Pr\\(>\\|t\\|\\)$", "p.value", names(fixed_tbl))
  fixed_tbl$model <- label
  list(model = model, fixed = fixed_tbl)
}

base_terms <- c(structure_terms, meteo_terms, calendar_terms)

f_tourism_nolag_arrivi <- make_mixed_formula("log_kwh", c(base_terms, "log_totale_arrivi_z"))

m2_base_local <- lmerTest::lmer(f_m2, data = df_base, REML = FALSE, control = ctrl)
m2_arrivi <- fit_with_summary(f_tourism_nolag_arrivi, "M2_arrivi_no_lag", df_base)

arrivi_resid_fit <- stats::lm(make_fixed_formula("log_totale_arrivi_z", c(structure_terms, meteo_terms, calendar_terms)), data = df_base)

df_res <- df_base
df_res$resid_arrivi_season <- as.numeric(resid(arrivi_resid_fit))
df_res$resid_arrivi_season_z <- as.numeric(scale(df_res$resid_arrivi_season))

f_m4_resid <- make_mixed_formula(
  "log_kwh",
  c(structure_terms, meteo_terms, calendar_terms, dynamic_terms, "resid_arrivi_season_z")
)

f_m2_resid <- make_mixed_formula(
  "log_kwh",
  c(structure_terms, meteo_terms, calendar_terms, "resid_arrivi_season_z")
)

m3_base_local <- lmerTest::lmer(f_m3, data = df_res, REML = FALSE, control = ctrl)
m4_observed_local <- lmerTest::lmer(f_m4, data = df_res, REML = FALSE, control = ctrl)
m2_resid <- fit_with_summary(f_m2_resid, "M2_resid_tourism_no_lag", df_res)
m4_resid <- fit_with_summary(f_m4_resid, "M4_resid_tourism", df_res)

comparison_tbl <- data.frame(
  model = c("M2_base", "M2_arrivi_no_lag", "M2_resid_arrivi_no_lag", "M3_base", "M4_observed_arrivi", "M4_resid_arrivi"),
  AIC = c(AIC(m2_base_local), AIC(m2_arrivi$model), AIC(m2_resid$model), AIC(m3_base_local), AIC(m4_observed_local), AIC(m4_resid$model)),
  BIC = c(BIC(m2_base_local), BIC(m2_arrivi$model), BIC(m2_resid$model), BIC(m3_base_local), BIC(m4_observed_local), BIC(m4_resid$model)),
  logLik = c(as.numeric(logLik(m2_base_local)), as.numeric(logLik(m2_arrivi$model)), as.numeric(logLik(m2_resid$model)), as.numeric(logLik(m3_base_local)), as.numeric(logLik(m4_observed_local)), as.numeric(logLik(m4_resid$model))),
  singular = c(lme4::isSingular(m2_base_local), lme4::isSingular(m2_arrivi$model), lme4::isSingular(m2_resid$model), lme4::isSingular(m3_base_local), lme4::isSingular(m4_observed_local), lme4::isSingular(m4_resid$model))
)

write.csv(comparison_tbl, file = file.path("report_assets", "arrivi_overlap_comparison.csv"), row.names = FALSE)

anova_nolag_tbl <- as.data.frame(anova(m2_base_local, m2_arrivi$model, m2_resid$model))
anova_nolag_tbl$model <- rownames(anova_nolag_tbl)
rownames(anova_nolag_tbl) <- NULL
anova_nolag_tbl <- anova_nolag_tbl[, c("model", setdiff(names(anova_nolag_tbl), "model"))]
write.csv(anova_nolag_tbl, file = file.path("report_assets", "tourism_no_lag_compare.csv"), row.names = FALSE)

anova_resid_tbl <- as.data.frame(anova(m3_base_local, m4_observed_local, m4_resid$model))
anova_resid_tbl$model <- rownames(anova_resid_tbl)
rownames(anova_resid_tbl) <- NULL
anova_resid_tbl <- anova_resid_tbl[, c("model", setdiff(names(anova_resid_tbl), "model"))]
write.csv(anova_resid_tbl, file = file.path("report_assets", "tourism_residualized_compare.csv"), row.names = FALSE)

fixed_tbl <- rbind(
  subset(m2_arrivi$fixed, term == "log_totale_arrivi_z"),
  subset(m2_resid$fixed, term == "resid_arrivi_season_z"),
  subset(m4_resid$fixed, term == "resid_arrivi_season_z"),
  subset(fixed_effects_table, model == "M4_turismo" & term == "log_totale_arrivi_z")
)
write.csv(fixed_tbl, file = file.path("report_assets", "arrivi_overlap_fixed_effects.csv"), row.names = FALSE)

corr_tbl <- data.frame(
  series_1 = c("arrivi_vs_cos_m1", "arrivi_vs_lag12", "arrivi_vs_lag1", "arrivi_resid_vs_lag12", "arrivi_resid_vs_lag1"),
  correlation = c(
    cor(df_base$log_totale_arrivi_z, df_base$cos_m1_z, use = "complete.obs"),
    cor(df_base$log_totale_arrivi_z, df_base$log_kwh_lag12_z, use = "complete.obs"),
    cor(df_base$log_totale_arrivi_z, df_base$log_kwh_lag1_z, use = "complete.obs"),
    cor(df_res$resid_arrivi_season_z, df_res$log_kwh_lag12_z, use = "complete.obs"),
    cor(df_res$resid_arrivi_season_z, df_res$log_kwh_lag1_z, use = "complete.obs")
  )
)
write.csv(corr_tbl, file = file.path("report_assets", "arrivi_overlap_correlations.csv"), row.names = FALSE)

png(file.path("report_assets", "arrivi_overlap_aic.png"), width = 1400, height = 900, res = 150)
par(mar = c(8, 5, 4, 2))
barplot(comparison_tbl$AIC, names.arg = comparison_tbl$model, las = 2, col = "steelblue3", main = "AIC: arrivi con/senza lag e residualizzati")
dev.off()
