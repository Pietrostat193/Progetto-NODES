source("Modelli/modelling_fixed.R", local = TRUE)
status_lines <- c(
  paste0("analysis_df_exists=", exists("analysis_df"))
)
if (exists("analysis_df")) {
  status_lines <- c(status_lines, paste0("analysis_df_dim=", paste(dim(analysis_df), collapse = "x")))
  target_cols <- c("resid_final", "arrivi_destag_weather_z", "date", "comune_key", "comune_nome")
  status_lines <- c(status_lines, paste0(target_cols, "=", target_cols %in% names(analysis_df)))
}
writeLines(status_lines, "modelling_check_results.txt")
