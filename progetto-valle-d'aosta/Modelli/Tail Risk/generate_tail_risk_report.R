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

required_pkgs <- c("dplyr", "sf")
to_install <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(dplyr)
library(sf)

assets_dir <- "report_assets"

required_assets <- c(
  "copula_selection.csv",
  "copula_parameters.csv",
  "copula_selection_by_segment.csv",
  "copula_segment_summary.csv",
  "municipality_tail_risk_enriched.csv",
  "municipality_tail_risk_overlap_summary.csv",
  "municipality_tail_risk_top20.png",
  "copula_contour_plot.png"
)

missing_assets <- required_assets[!file.exists(file.path(assets_dir, required_assets))]
if (length(missing_assets) > 0) {
  stop(
    "Asset mancanti per il report Tail Risk: ",
    paste(missing_assets, collapse = ", "),
    call. = FALSE
  )
}

normalize_key <- function(x) {
  out <- trimws(tolower(as.character(x)))
  gsub("\\s+", " ", out)
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "NA", formatC(x, digits = digits, format = "f"))
}

markdown_table <- function(df) {
  header <- paste(names(df), collapse = " | ")
  sep <- paste(rep("---", ncol(df)), collapse = " | ")
  rows <- apply(df, 1, function(row) paste(row, collapse = " | "))
  c(paste0("| ", header, " |"), paste0("| ", sep, " |"), paste0("| ", rows, " |"))
}

make_palette_bins <- function(values, n_bins = 5) {
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    return(list(breaks = c(0, 1), labels = "NA", bins = NULL))
  }

  breaks <- unique(as.numeric(quantile(values, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)))
  if (length(breaks) < 3) {
    breaks <- pretty(values, n = n_bins)
  }
  labels <- levels(cut(values, breaks = breaks, include.lowest = TRUE))
  list(breaks = breaks, labels = labels, bins = length(labels))
}

plot_map_metric <- function(sf_df, value_col, title, out_path, palette_fun) {
  values <- sf_df[[value_col]]
  bins_info <- make_palette_bins(values)

  if (is.null(bins_info$bins) || bins_info$bins == 0) {
    fill_cols <- rep("gray90", nrow(sf_df))
    legend_labels <- "NA"
    legend_fill <- "gray90"
  } else {
    palette <- palette_fun(bins_info$bins)
    cuts <- cut(values, breaks = bins_info$breaks, include.lowest = TRUE, labels = FALSE)
    fill_cols <- ifelse(is.na(cuts), "gray90", palette[cuts])
    legend_labels <- bins_info$labels
    legend_fill <- palette[seq_along(legend_labels)]
  }

  png(out_path, width = 1200, height = 900, res = 150)
  par(mar = c(0, 0, 3, 0))
  plot(sf::st_geometry(sf_df), col = fill_cols, border = "white", main = title)
  legend("bottomleft", legend = legend_labels, fill = legend_fill, bty = "n", cex = 0.8)
  dev.off()
}

copula_selection <- read.csv(file.path(assets_dir, "copula_selection.csv"), stringsAsFactors = FALSE)
copula_parameters <- read.csv(file.path(assets_dir, "copula_parameters.csv"), stringsAsFactors = FALSE)
segment_selection <- read.csv(file.path(assets_dir, "copula_selection_by_segment.csv"), stringsAsFactors = FALSE)
segment_summary <- read.csv(file.path(assets_dir, "copula_segment_summary.csv"), stringsAsFactors = FALSE)
municipality_risk <- read.csv(file.path(assets_dir, "municipality_tail_risk_enriched.csv"), stringsAsFactors = FALSE)
overlap_summary <- read.csv(file.path(assets_dir, "municipality_tail_risk_overlap_summary.csv"), stringsAsFactors = FALSE)

municipality_risk <- municipality_risk %>%
  arrange(risk_rank) %>%
  mutate(comune_key_norm = normalize_key(comune_key))

map_env <- new.env()
load(normalizePath(file.path("..", "data", "vda_shapefile", "vda_sf.RData")), envir = map_env)
vda_sf <- map_env$vda_sf
vda_sf$municipality_key_norm <- normalize_key(vda_sf$municipality_key)
vda_sf$istat_muni_code <- as.character(vda_sf$istat_muni_code)

map_df <- vda_sf %>%
  left_join(
    municipality_risk,
    by = c("municipality_key_norm" = "comune_key_norm")
  )

plot_map_metric(
  map_df,
  "risk_score",
  "Tail risk congiunto turismo-energia",
  file.path(assets_dir, "tail_risk_map_score.png"),
  function(n) grDevices::colorRampPalette(c("#FFF5EB", "#E6550D", "#7F2704"))(n)
)

plot_map_metric(
  map_df,
  "p_energy_given_tourism_upper_95",
  "Probabilita' di shock energetico dato shock turistico estremo (95%)",
  file.path(assets_dir, "tail_risk_map_conditional95.png"),
  function(n) grDevices::colorRampPalette(c("#F7FCFD", "#66C2A4", "#00441B"))(n)
)

plot_map_metric(
  map_df,
  "arrivi_per_residente",
  "Intensita' turistica media: arrivi per residente",
  file.path(assets_dir, "tail_risk_map_tourism_intensity.png"),
  function(n) grDevices::colorRampPalette(c("#F7FBFF", "#6BAED6", "#08306B"))(n)
)

top10_labels <- municipality_risk %>% slice_head(n = 10)

png(file.path(assets_dir, "tail_risk_vs_tourism_scatter.png"), width = 1200, height = 900, res = 150)
par(mar = c(5, 5, 4, 2) + 0.1)
plot(
  municipality_risk$arrivi_per_residente,
  municipality_risk$risk_score,
  pch = 19,
  col = grDevices::rgb(0.15, 0.15, 0.15, 0.55),
  xlab = "Arrivi per residente",
  ylab = "Risk score tail risk",
  main = "Tail risk vs intensita' turistica"
)
abline(lm(risk_score ~ arrivi_per_residente, data = municipality_risk), lty = 2, col = "firebrick3", lwd = 2)
text(
  top10_labels$arrivi_per_residente,
  top10_labels$risk_score,
  labels = top10_labels$comune_nome,
  pos = 4,
  cex = 0.7,
  col = "firebrick4"
)
dev.off()

segment_summary_md <- segment_summary %>%
  transmute(
    Segmento = sample,
    Osservazioni = n_obs,
    `Copula vincente` = selected_family,
    LogLik = fmt_num(logLik, 3),
    AIC = fmt_num(AIC, 3),
    BIC = fmt_num(BIC, 3),
    `Tail bassa` = fmt_num(lower_tail, 3),
    `Tail alta` = fmt_num(upper_tail, 3)
  )

pooled_selection_md <- copula_selection %>%
  transmute(
    Famiglia = family,
    LogLik = fmt_num(logLik, 3),
    AIC = fmt_num(AIC, 3),
    BIC = fmt_num(BIC, 3),
    `Tail bassa` = fmt_num(lower_tail, 3),
    `Tail alta` = fmt_num(upper_tail, 3)
  )

top15_md <- municipality_risk %>%
  slice_head(n = 15) %>%
  transmute(
    Rank = risk_rank,
    Comune = comune_nome,
    `Risk score` = fmt_num(risk_score, 3),
    `P(Energy|Tourism 95%)` = fmt_num(p_energy_given_tourism_upper_95, 3),
    `Joint 95` = n_joint_upper_95,
    `P(Energy|Tourism 90%)` = fmt_num(p_energy_given_tourism_upper_90, 3),
    `Arrivi/residente` = fmt_num(arrivi_per_residente, 2),
    `Rank turismo` = tourism_intensity_rank
  )

top_overlap <- municipality_risk %>%
  filter(risk_rank <= 10 | tourism_intensity_rank <= 10) %>%
  arrange(risk_rank, tourism_intensity_rank) %>%
  transmute(
    Comune = comune_nome,
    `Rank rischio` = risk_rank,
    `Rank turismo` = tourism_intensity_rank,
    `Risk score` = fmt_num(risk_score, 3),
    `Arrivi/residente` = fmt_num(arrivi_per_residente, 2)
  )

overlap_corr <- overlap_summary$value[overlap_summary$metric == "correlation_risk_vs_arrivi_per_residente"]
overlap_count <- overlap_summary$value[overlap_summary$metric == "top10_overlap_count"]
overlap_share <- overlap_summary$value[overlap_summary$metric == "top10_overlap_share"]

winning_param <- copula_parameters %>% slice(1)

report_lines <- c(
  "# Tail Risk Report",
  "",
  "## Sintesi",
  "",
  paste0(
    "La copula pooled vincente e' **", segment_summary$selected_family[segment_summary$sample == "all_comuni"],
    "**, con tail dependence superiore pari a **", fmt_num(segment_summary$upper_tail[segment_summary$sample == "all_comuni"], 3),
    "** e tail inferiore nulla."
  ),
  paste0(
    " Nei comuni ad alto tail risk la dipendenza di coda alta sale a **",
    fmt_num(segment_summary$upper_tail[segment_summary$sample == "top10_tail_risk"], 3),
    "**, mentre nei comuni a piu' alta intensita' turistica sale a **",
    fmt_num(segment_summary$upper_tail[segment_summary$sample == "top10_tourism_intensity"], 3),
    "**."
  ),
  paste0(
    " La correlazione tra risk score e arrivi per residente e' **",
    fmt_num(overlap_corr, 3),
    "**; l'overlap tra top 10 rischio e top 10 intensita' turistica e' **",
    overlap_count,
    " comuni** (quota **",
    fmt_num(overlap_share, 2),
    "**)."
  ),
  "",
  "## Copule",
  "",
  "### Tabella riassuntiva per segmento",
  "",
  markdown_table(segment_summary_md),
  "",
  "### Famiglie testate sul campione pooled",
  "",
  markdown_table(pooled_selection_md),
  "",
  "### Parametro della copula pooled vincente",
  "",
  paste0(
    "Famiglia: **", winning_param$selected_family, "**; parametro: **", fmt_num(winning_param$estimate, 3),
    "**; standard error: **", fmt_num(winning_param$std_error, 3),
    "**; tail alta: **", fmt_num(winning_param$upper_tail, 3), "**."
  ),
  "",
  "![Contour pooled](report_assets/copula_contour_plot.png)",
  "",
  "## Localizzazione del tail risk",
  "",
  "### Top 15 comuni per rischio congiunto turismo-energia",
  "",
  markdown_table(top15_md),
  "",
  "![Top 20 risk score](report_assets/municipality_tail_risk_top20.png)",
  "",
  "### Mappe",
  "",
  "![Mappa risk score](report_assets/tail_risk_map_score.png)",
  "",
  "![Mappa probabilita condizionale 95](report_assets/tail_risk_map_conditional95.png)",
  "",
  "![Mappa intensita turistica](report_assets/tail_risk_map_tourism_intensity.png)",
  "",
  "## Rischio vs intensita' turistica",
  "",
  "![Scatter rischio vs turismo](report_assets/tail_risk_vs_tourism_scatter.png)",
  "",
  "### Comuni che compaiono in alto per rischio e/o intensita' turistica",
  "",
  markdown_table(top_overlap),
  "",
  "## Lettura operativa",
  "",
  "- Il tail risk non e' diffuso in modo uniforme: si concentra in un gruppo ristretto di comuni alpini e turistici.",
  "- La copula sui top 10 comuni risk-ranked mostra una tail dependence superiore piu' forte del campione pooled, quindi l'aggregazione regionale attenua il segnale.",
  "- L'intensita' turistica media spiega parte del ranking, ma non tutto: alcuni comuni emergono come vulnerabili pur non essendo ai primissimi posti per arrivi per residente.",
  "- Per il paper, la domanda forte diventa: dove gli shock turistici estremi si trasmettono piu' facilmente in shock energetici estremi?"
)

writeLines(report_lines, con = file.path(script_dir, "tail_risk_report.md"))

cat("Tail risk report creato in Modelli/Tail Risk/tail_risk_report.md\n")