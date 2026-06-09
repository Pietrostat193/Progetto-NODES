args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)

script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
  getwd()
}

setwd(script_dir)
Sys.setenv(MODELLING_SCRIPT_DIR = normalizePath(file.path(script_dir, "..")))
Sys.setenv(MODELLING_SKIP_BASE_PLOTS = "1")
source("./modelling.R", local = TRUE)
Sys.unsetenv("MODELLING_SCRIPT_DIR")
Sys.unsetenv("MODELLING_SKIP_BASE_PLOTS")

dir.create("report_assets", showWarnings = FALSE, recursive = TRUE)

extra_pkgs <- c("sf")
to_install <- extra_pkgs[!vapply(extra_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cran.r-project.org/")
}

library(dplyr)
library(sf)

normalize_key <- function(x) {
  out <- trimws(tolower(as.character(x)))
  gsub("\\s+", " ", out)
}

extract_fixed_local <- function(mod, model_name) {
  sm <- as.data.frame(coef(summary(mod)))
  sm$term <- rownames(sm)
  rownames(sm) <- NULL
  names(sm) <- sub("^Estimate$", "estimate", names(sm))
  names(sm) <- sub("^Std\\. Error$", "std.error", names(sm))
  names(sm) <- sub("^t value$", "statistic", names(sm))
  names(sm) <- sub("^Pr\\(>\\|t\\|\\)$", "p.value", names(sm))
  if (!"df" %in% names(sm)) {
    sm$df <- df.residual(mod)
  }
  sm$model <- model_name
  sm$engine <- "lmerTest"
  sm[, c("model", "engine", "term", "estimate", "std.error", "df", "statistic", "p.value")]
}

safe_r2_local <- function(model) {
  out <- performance::r2_nakagawa(model, tolerance = 1e-10)
  c(
    R2_marginal = unname(out$R2_marginal),
    R2_conditional = unname(out$R2_conditional)
  )
}

tourism_profile <- data_fe %>%
  group_by(comune_key) %>%
  summarise(
    istat_muni_code = dplyr::first(istat_muni_code[!is.na(istat_muni_code)]),
    comune_nome = dplyr::first(as.character(comune_nome)),
    mean_arrivi = mean(totale_arrivi, na.rm = TRUE),
    mean_residenti = mean(residenti, na.rm = TRUE),
    mean_letti = mean(numero_letti, na.rm = TRUE),
    arrivi_per_residente = ifelse(mean_residenti > 0, mean_arrivi / mean_residenti, NA_real_),
    letti_per_residente = ifelse(mean_residenti > 0, mean_letti / mean_residenti, NA_real_),
    .groups = "drop"
  )

arrivi_cutoff <- median(tourism_profile$arrivi_per_residente, na.rm = TRUE)

tourism_profile <- tourism_profile %>%
  mutate(
    istat_muni_code = as.character(istat_muni_code),
    comune_key_norm = normalize_key(comune_key),
    tourism_cluster = factor(
      ifelse(arrivi_per_residente > arrivi_cutoff, "touristico", "non_turistico"),
      levels = c("non_turistico", "touristico")
    ),
    tourism_high = ifelse(tourism_cluster == "touristico", 1, 0)
  )

analysis_mean_df <- analysis_df %>%
  left_join(
    tourism_profile %>% select(comune_key, tourism_cluster, tourism_high, arrivi_per_residente, letti_per_residente),
    by = "comune_key"
  ) %>%
  mutate(
    arrivi_x_temperatura_z = log_totale_arrivi_z * temperatura_z,
    arrivi_x_tourism_high = log_totale_arrivi_z * tourism_high
  )

f_m4_hot <- make_mixed_formula(
  "log_kwh",
  c(structure_terms, meteo_terms, calendar_terms, dynamic_terms, "log_totale_arrivi_z", "arrivi_x_temperatura_z")
)

f_m4_hetero <- make_mixed_formula(
  "log_kwh",
  c(structure_terms, meteo_terms, calendar_terms, dynamic_terms, "log_totale_arrivi_z", "tourism_high", "arrivi_x_tourism_high")
)

m4_hot <- lmerTest::lmer(f_m4_hot, data = analysis_mean_df, REML = FALSE, control = ctrl)
m4_hetero <- lmerTest::lmer(f_m4_hetero, data = analysis_mean_df, REML = FALSE, control = ctrl)

mean_effect_models <- list(
  M3_base = m3,
  M4_arrivi_turistici = m4,
  M4_arrivi_x_temperatura = m4_hot,
  M4_arrivi_eterogeneo = m4_hetero
)

mean_effect_compare <- purrr::imap_dfr(mean_effect_models, function(mod, nm) {
  r2vals <- safe_r2_local(mod)
  tibble::tibble(
    model = nm,
    AIC = AIC(mod),
    BIC = BIC(mod),
    logLik = as.numeric(logLik(mod)),
    R2_marginal = unname(r2vals["R2_marginal"]),
    R2_conditional = unname(r2vals["R2_conditional"]),
    singular = lme4::isSingular(mod, tol = 1e-5)
  )
})
write.csv(mean_effect_compare, file = file.path("report_assets", "mean_effect_model_compare.csv"), row.names = FALSE)

mean_effect_fixed <- dplyr::bind_rows(
  extract_fixed_local(m4, "M4_arrivi_turistici"),
  extract_fixed_local(m4_hot, "M4_arrivi_x_temperatura"),
  extract_fixed_local(m4_hetero, "M4_arrivi_eterogeneo")
)
write.csv(mean_effect_fixed, file = file.path("report_assets", "mean_effect_fixed_effects.csv"), row.names = FALSE)

key_effects <- mean_effect_fixed %>%
  filter(term %in% c("log_totale_arrivi_z", "arrivi_x_temperatura_z", "tourism_high", "arrivi_x_tourism_high"))
write.csv(key_effects, file = file.path("report_assets", "mean_effect_key_effects.csv"), row.names = FALSE)

base_arrivi <- key_effects %>% filter(model == "M4_arrivi_eterogeneo", term == "log_totale_arrivi_z") %>% pull(estimate)
delta_tourist <- key_effects %>% filter(model == "M4_arrivi_eterogeneo", term == "arrivi_x_tourism_high") %>% pull(estimate)

hetero_slopes <- data.frame(
  group = c("non_turistico", "touristico"),
  arrival_effect = c(base_arrivi, base_arrivi + delta_tourist)
)
write.csv(hetero_slopes, file = file.path("report_assets", "mean_effect_group_slopes.csv"), row.names = FALSE)

municipality_profile <- tourism_profile %>%
  left_join(
    random_intercepts_m4 %>% mutate(comune_key = as.character(comune_key), comune_key_norm = normalize_key(comune_key)),
    by = "comune_key_norm"
  ) %>%
  arrange(desc(arrivi_per_residente))
write.csv(municipality_profile, file = file.path("report_assets", "municipality_profile.csv"), row.names = FALSE)

png(file.path("report_assets", "mean_effect_model_compare.png"), width = 1600, height = 900, res = 150)
par(mfrow = c(1, 2), mar = c(8, 5, 4, 2))
barplot(mean_effect_compare$AIC, names.arg = mean_effect_compare$model, las = 2, col = c("gray70", "tomato", "goldenrod", "steelblue3"), main = "AIC: effetto medio degli arrivi")
barplot(mean_effect_compare$BIC, names.arg = mean_effect_compare$model, las = 2, col = c("gray70", "tomato", "goldenrod", "steelblue3"), main = "BIC: effetto medio degli arrivi")
dev.off()

coef_plot_df <- key_effects %>% mutate(label = paste(model, term, sep = " | "))
lower <- coef_plot_df$estimate - 1.96 * coef_plot_df$std.error
upper <- coef_plot_df$estimate + 1.96 * coef_plot_df$std.error
png(file.path("report_assets", "mean_effect_key_coefficients.png"), width = 1600, height = 1000, res = 150)
par(mar = c(5, 18, 4, 2))
plot(
  coef_plot_df$estimate,
  seq_len(nrow(coef_plot_df)),
  xlim = range(c(lower, upper), na.rm = TRUE),
  yaxt = "n",
  ylab = "",
  xlab = "Stima coefficiente (IC 95%)",
  main = "Effetti chiave: arrivi turistici, caldo, eterogeneita'",
  pch = 19,
  col = ifelse(coef_plot_df$p.value < 0.05, "firebrick", "steelblue")
)
segments(lower, seq_len(nrow(coef_plot_df)), upper, seq_len(nrow(coef_plot_df)), col = "gray50", lwd = 2)
abline(v = 0, lty = 2, col = "gray40")
axis(2, at = seq_len(nrow(coef_plot_df)), labels = coef_plot_df$label, las = 2)
dev.off()

map_data_path <- normalizePath(file.path("..", "data", "vda_shapefile", "vda_sf.RData"))
map_env <- new.env()
load(map_data_path, envir = map_env)
vda_sf <- map_env$vda_sf
vda_sf$istat_muni_code <- as.character(vda_sf$istat_muni_code)
vda_sf$municipality_key_norm <- normalize_key(vda_sf$municipality_key)

map_df <- vda_sf %>%
  left_join(
    municipality_profile %>% select(istat_muni_code, comune_key_norm, tourism_cluster, arrivi_per_residente, random_intercept),
    by = c("municipality_key_norm" = "comune_key_norm")
  )

png(file.path("report_assets", "municipality_maps.png"), width = 1800, height = 900, res = 150)
par(mfrow = c(1, 2), mar = c(0, 0, 3, 0))
cluster_cols <- ifelse(map_df$tourism_cluster == "touristico", "#D95F02", "#1B9E77")
cluster_cols[is.na(cluster_cols)] <- "gray85"
plot(sf::st_geometry(map_df), col = cluster_cols, border = "white", main = "Comuni turistici vs non turistici")
legend("bottomleft", legend = c("non_turistico", "touristico"), fill = c("#1B9E77", "#D95F02"), bty = "n")

ri_vals <- map_df$random_intercept
if (all(is.na(ri_vals))) {
  ri_cols <- rep("gray90", nrow(map_df))
  ri_pal <- "gray90"
  legend_labels <- "random intercept non disponibile"
} else {
  ri_breaks <- unique(as.numeric(quantile(ri_vals, probs = seq(0, 1, length.out = 6), na.rm = TRUE)))
  if (length(ri_breaks) < 3) {
    ri_breaks <- pretty(ri_vals, n = 5)
  }
  ri_bins <- cut(ri_vals, breaks = ri_breaks, include.lowest = TRUE, labels = FALSE)
  n_colors <- max(ri_bins, na.rm = TRUE)
  ri_pal <- grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(n_colors)
  ri_cols <- ifelse(is.na(ri_bins), "gray90", ri_pal[ri_bins])
  legend_labels <- levels(cut(ri_vals, breaks = ri_breaks, include.lowest = TRUE))
}
plot(sf::st_geometry(map_df), col = ri_cols, border = "white", main = "Random intercept residuo M4 arrivi turistici")
legend_fill <- if (length(ri_pal) == 1) ri_pal else ri_pal[seq_along(legend_labels)]
legend("bottomleft", legend = legend_labels, fill = legend_fill, bty = "n", cex = 0.8)
dev.off()

m4_arrivi_coef <- key_effects %>% filter(model == "M4_arrivi_turistici", term == "log_totale_arrivi_z")
m4_hot_coef <- key_effects %>% filter(model == "M4_arrivi_x_temperatura", term == "arrivi_x_temperatura_z")
m4_hetero_diff <- key_effects %>% filter(model == "M4_arrivi_eterogeneo", term == "arrivi_x_tourism_high")

top_tourism <- municipality_profile %>%
  select(comune_nome, arrivi_per_residente) %>%
  slice_head(n = 10)

top_lines <- paste0("- ", top_tourism$comune_nome, ": ", sprintf("%.2f", top_tourism$arrivi_per_residente))

report_lines <- c(
  "# Mean Effect: arrivi turistici e consumo energetico",
  "",
  "## Introduzione",
  "",
  "Questa cartella raccoglie il blocco di analisi sull'effetto medio nel percorso:",
  "",
  "[Introduzione] -> [Modelli M1-M4] -> [Copule (Residui)] -> [Forecasting]",
  "",
  "Qui l'obiettivo e' capire se gli arrivi turistici aumentano il consumo energetico medio, se l'effetto cresce nei mesi piu' caldi e se cambia tra comuni turistici e non turistici.",
  "",
  "## Modelli M1-M4",
  "",
  "La sequenza base resta quella dei modelli M0-M4, ma il blocco finale non usa piu' un generico 'turismo': usa solo gli arrivi turistici.",
  "",
  "| Modello | AIC | BIC | R2 marginale | R2 condizionale | Singolare |",
  "|---|---:|---:|---:|---:|---|",
  sprintf("| M3_base | %.1f | %.1f | %.4f | %s | %s |", mean_effect_compare$AIC[mean_effect_compare$model == "M3_base"], mean_effect_compare$BIC[mean_effect_compare$model == "M3_base"], mean_effect_compare$R2_marginal[mean_effect_compare$model == "M3_base"], ifelse(is.na(mean_effect_compare$R2_conditional[mean_effect_compare$model == "M3_base"]), "NA", sprintf("%.4f", mean_effect_compare$R2_conditional[mean_effect_compare$model == "M3_base"])), mean_effect_compare$singular[mean_effect_compare$model == "M3_base"]),
  sprintf("| M4_arrivi_turistici | %.1f | %.1f | %.4f | %.4f | %s |", mean_effect_compare$AIC[mean_effect_compare$model == "M4_arrivi_turistici"], mean_effect_compare$BIC[mean_effect_compare$model == "M4_arrivi_turistici"], mean_effect_compare$R2_marginal[mean_effect_compare$model == "M4_arrivi_turistici"], mean_effect_compare$R2_conditional[mean_effect_compare$model == "M4_arrivi_turistici"], mean_effect_compare$singular[mean_effect_compare$model == "M4_arrivi_turistici"]),
  sprintf("| M4_arrivi_x_temperatura | %.1f | %.1f | %.4f | %.4f | %s |", mean_effect_compare$AIC[mean_effect_compare$model == "M4_arrivi_x_temperatura"], mean_effect_compare$BIC[mean_effect_compare$model == "M4_arrivi_x_temperatura"], mean_effect_compare$R2_marginal[mean_effect_compare$model == "M4_arrivi_x_temperatura"], mean_effect_compare$R2_conditional[mean_effect_compare$model == "M4_arrivi_x_temperatura"], mean_effect_compare$singular[mean_effect_compare$model == "M4_arrivi_x_temperatura"]),
  sprintf("| M4_arrivi_eterogeneo | %.1f | %.1f | %.4f | %.4f | %s |", mean_effect_compare$AIC[mean_effect_compare$model == "M4_arrivi_eterogeneo"], mean_effect_compare$BIC[mean_effect_compare$model == "M4_arrivi_eterogeneo"], mean_effect_compare$R2_marginal[mean_effect_compare$model == "M4_arrivi_eterogeneo"], mean_effect_compare$R2_conditional[mean_effect_compare$model == "M4_arrivi_eterogeneo"], mean_effect_compare$singular[mean_effect_compare$model == "M4_arrivi_eterogeneo"]),
  "",
  "![Confronto modelli mean effect](report_assets/mean_effect_model_compare.png)",
  "",
  "## Interazione arrivi x temperatura",
  "",
  sprintf("Nel modello con interazione, il coefficiente base degli arrivi turistici resta positivo, mentre l'interazione `arrivi x temperatura` e' stimata a %.4f con p-value %.4g.", m4_hot_coef$estimate, m4_hot_coef$p.value),
  "",
  "Interpretazione:",
  if (m4_hot_coef$p.value < 0.05) {
    if (m4_hot_coef$estimate > 0) {
      "- l'effetto degli arrivi turistici cresce nei mesi piu' caldi;"
    } else {
      "- l'effetto degli arrivi turistici si attenua nei mesi piu' caldi;"
    }
  } else {
    "- non emerge una evidenza robusta che l'effetto degli arrivi turistici cambi con la temperatura;"
  },
  "- questo test serve a distinguere l'effetto dei flussi turistici da un semplice effetto clima/condizionamento.",
  "",
  "## Effetto eterogeneo: comuni turistici vs non turistici",
  "",
  sprintf("I comuni sono classificati usando il valore mediano degli arrivi per residente nel periodo post-2018. La soglia mediana e' %.3f, con %d comuni turistici e %d non turistici.", arrivi_cutoff, sum(tourism_profile$tourism_cluster == "touristico", na.rm = TRUE), sum(tourism_profile$tourism_cluster == "non_turistico", na.rm = TRUE)),
  "",
  sprintf("Nel modello eterogeneo, la differenza di pendenza tra comuni turistici e non turistici e' %.4f con p-value %.4g.", m4_hetero_diff$estimate, m4_hetero_diff$p.value),
  sprintf("L'effetto degli arrivi turistici e' %.4f nei comuni non turistici e %.4f nei comuni turistici.", hetero_slopes$arrival_effect[hetero_slopes$group == "non_turistico"], hetero_slopes$arrival_effect[hetero_slopes$group == "touristico"]),
  "",
  if (m4_hetero_diff$p.value < 0.05) {
    if (m4_hetero_diff$estimate > 0) {
      "Questo indica che l'impatto marginale degli arrivi turistici e' piu' forte nei comuni gia' piu' esposti al turismo."
    } else {
      "Questo indica che l'impatto marginale degli arrivi turistici e' piu' forte nei comuni meno turistici."
    }
  } else {
    "La differenza tra comuni turistici e non turistici non e' abbastanza netta da supportare una forte eterogeneita' del coefficiente medio."
  },
  "",
  "## Lettura territoriale",
  "",
  "I comuni con maggiore intensita' turistica secondo gli arrivi per residente sono:",
  top_lines,
  "",
  "La mappa a sinistra separa i comuni turistici da quelli non turistici; la mappa a destra mostra il random intercept residuo del modello M4 con arrivi turistici. Se in alcune aree turistiche il random intercept resta alto, significa che c'e' ancora eterogeneita' locale non catturata dai regressori medi del modello.",
  "",
  "![Mappe comunali](report_assets/municipality_maps.png)",
  "",
  "## Coefficienti chiave",
  "",
  sprintf("- Effetto medio degli arrivi turistici in M4: %.4f, p-value %.4g.", m4_arrivi_coef$estimate, m4_arrivi_coef$p.value),
  sprintf("- Interazione arrivi x temperatura: %.4f, p-value %.4g.", m4_hot_coef$estimate, m4_hot_coef$p.value),
  sprintf("- Extra-effetto dei comuni turistici: %.4f, p-value %.4g.", m4_hetero_diff$estimate, m4_hetero_diff$p.value),
  "",
  "![Coefficienti chiave](report_assets/mean_effect_key_coefficients.png)",
  "",
  "## Sintesi",
  "",
  "- Nel blocco M4 la nomenclatura corretta e' 'arrivi turistici', non turismo generico.",
  if (m4_hot_coef$p.value < 0.05) {
    "- L'interazione con la temperatura suggerisce che il caldo modula l'effetto degli arrivi turistici."
  } else {
    "- L'interazione con la temperatura non aggiunge evidenza robusta: gli arrivi contano, ma non soprattutto perche' coincidono coi mesi piu' caldi."
  },
  if (m4_hetero_diff$p.value < 0.05) {
    "- L'effetto medio degli arrivi non e' uniforme nello spazio: cambia tra comuni turistici e non turistici."
  } else {
    "- L'effetto medio degli arrivi appare abbastanza stabile tra comuni turistici e non turistici sotto questa classificazione."
  },
  "- Questa cartella copre l'effetto medio; le code dei residui e la pianificazione previsiva sono separate nelle cartelle Tail Risk e Forecast."
)

writeLines(report_lines, con = "README_mean_effect.md")
