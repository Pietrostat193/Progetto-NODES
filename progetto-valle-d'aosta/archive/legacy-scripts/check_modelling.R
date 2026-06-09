source("Modelli/modelling_fixed.R", local = TRUE)
cat("analysis_df_exists=", exists("analysis_df"), "\n", sep = "")
if (exists("analysis_df")) {
  cat("analysis_df_dim=", paste(dim(analysis_df), collapse = "x"), "\n", sep = "")
  target_cols <- c("resid_final", "arrivi_destag_weather_z", "date", "comune_key", "comune_nome")
  for (nm in target_cols) {
    cat(nm, "=", nm %in% names(analysis_df), "\n", sep = "")
  }
}
