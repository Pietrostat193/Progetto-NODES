setwd(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]))))

source("modelling.R", local = TRUE)

dir.create("report_assets", showWarnings = FALSE)

df_base <- analysis_df

df_res <- df_base
arrivi_resid_fit <- stats::lm(
  make_fixed_formula("log_totale_arrivi_z", c(structure_terms, meteo_terms, calendar_terms)),
  data = df_base
)
df_res$resid_arrivi_season <- as.numeric(resid(arrivi_resid_fit))
df_res$resid_arrivi_season_z <- as.numeric(scale(df_res$resid_arrivi_season))

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

m2_base_local <- fit_with_summary(f_m2, "M2_base", df_base)
m2_arrivi <- fit_with_summary(
  make_mixed_formula("log_kwh", c(base_terms, "log_totale_arrivi_z")),
  "M2_arrivi_no_lag",
  df_base
)
m2_resid <- fit_with_summary(
  make_mixed_formula("log_kwh", c(base_terms, "resid_arrivi_season_z")),
  "M2_resid_arrivi_no_lag",
  df_res
)
m3_base_local <- fit_with_summary(f_m3, "M3_base", df_res)
m4_observed_local <- fit_with_summary(f_m4, "M4_observed_tourism", df_res)
m4_resid <- fit_with_summary(
  make_mixed_formula(
    "log_kwh",
    c(structure_terms, meteo_terms, calendar_terms, dynamic_terms, "resid_arrivi_season_z")
  ),
  "M4_resid_arrivi",
  df_res
)

comparison_tbl <- data.frame(
  section = "model_compare",
  model = c(
    "M2_base",
    "M2_arrivi_no_lag",
    "M2_resid_arrivi_no_lag",
    "M3_base",
    "M4_observed_tourism",
    "M4_resid_arrivi"
  ),
  term = NA_character_,
  estimate = NA_real_,
  std.error = NA_real_,
  statistic = NA_real_,
  p.value = NA_real_,
  value = c(
    AIC(m2_base_local$model),
    AIC(m2_arrivi$model),
    AIC(m2_resid$model),
    AIC(m3_base_local$model),
    AIC(m4_observed_local$model),
    AIC(m4_resid$model)
  ),
  metric = "AIC"
)

coef_tbl <- rbind(
  subset(m2_arrivi$fixed, term %in% c("log_totale_arrivi_z")),
  subset(m2_resid$fixed, term %in% c("resid_arrivi_season_z")),
  subset(m4_observed_local$fixed, term %in% c("log_totale_arrivi_z", "log_totale_presenze_z")),
  subset(m4_resid$fixed, term %in% c("resid_arrivi_season_z"))
)
coef_tbl$section <- "coefficient"
coef_tbl$value <- NA_real_
coef_tbl$metric <- NA_character_
coef_tbl <- coef_tbl[, c("section", "model", "term", "estimate", "std.error", "statistic", "p.value", "value", "metric")]

corr_tbl <- data.frame(
  section = "correlation",
  model = NA_character_,
  term = c(
    "arrivi_vs_log_kwh_lag1_z",
    "arrivi_vs_log_kwh_lag12_z",
    "arrivi_vs_cos_m1_z",
    "arrivi_resid_vs_log_kwh_lag1_z",
    "arrivi_resid_vs_log_kwh_lag12_z"
  ),
  estimate = NA_real_,
  std.error = NA_real_,
  statistic = NA_real_,
  p.value = NA_real_,
  value = c(
    cor(df_base$log_totale_arrivi_z, df_base$log_kwh_lag1_z, use = "complete.obs"),
    cor(df_base$log_totale_arrivi_z, df_base$log_kwh_lag12_z, use = "complete.obs"),
    cor(df_base$log_totale_arrivi_z, df_base$cos_m1_z, use = "complete.obs"),
    cor(df_res$resid_arrivi_season_z, df_res$log_kwh_lag1_z, use = "complete.obs"),
    cor(df_res$resid_arrivi_season_z, df_res$log_kwh_lag12_z, use = "complete.obs")
  ),
  metric = "correlation"
)

summary_tbl <- rbind(comparison_tbl, coef_tbl, corr_tbl)
write.csv(summary_tbl, file = file.path("report_assets", "arrivi_overlap_summary.csv"), row.names = FALSE)
