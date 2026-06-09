suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sf)
  library(ggplot2)
})

normalize_key <- function(x) {
  out <- trimws(tolower(as.character(x)))
  gsub("\\s+", " ", out)
}

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
scenarios_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
modelli_root <- normalizePath(file.path(scenarios_root, ".."), winslash = "/", mustWork = FALSE)

results_dir <- file.path(scenarios_root, "results")
assets_dir <- file.path(scenarios_root, "report_assets")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(assets_dir, recursive = TRUE, showWarnings = FALSE)

forecast_town_path <- file.path(modelli_root, "Forecast", "results", "scenario_forecast", "tourism_shock_forecast_town_2025.csv")
tail_risk_path <- file.path(modelli_root, "Tail Risk", "report_assets", "municipality_tail_risk_enriched.csv")

if (!file.exists(forecast_town_path)) {
  stop("Missing forecast scenario file: ", forecast_town_path)
}
if (!file.exists(tail_risk_path)) {
  stop("Missing tail risk file: ", tail_risk_path)
}

cat("1/5: Loading scenario and tail risk inputs...\n")
forecast_town <- read_csv(forecast_town_path, show_col_types = FALSE) %>%
  mutate(
    date = as.Date(date),
    comune_key_norm = normalize_key(comune_key)
  )

tail_risk <- read_csv(tail_risk_path, show_col_types = FALSE) %>%
  mutate(comune_key_norm = normalize_key(comune_key)) %>%
  select(
    comune_key_norm,
    comune_nome,
    istat_muni_code,
    risk_score,
    p_energy_given_tourism_upper_95,
    lift_upper_95,
    p_energy_given_tourism_upper_90,
    lift_upper_90,
    tourism_intensity_rank,
    risk_rank
  )

cat("2/5: Building scenario-tail impact metrics...\n")
scenario_tail_month <- forecast_town %>%
  left_join(tail_risk, by = "comune_key_norm") %>%
  mutate(
    # Conservative fallback if conditional probability is missing.
    p_energy_given_tourism_upper_95 = ifelse(
      !is.finite(p_energy_given_tourism_upper_95) | is.na(p_energy_given_tourism_upper_95),
      0,
      p_energy_given_tourism_upper_95
    ),
    risk_score = ifelse(!is.finite(risk_score) | is.na(risk_score), 0, risk_score),
    scenario_tail_impact_kwh = delta_kwh_q95_minus_actual * p_energy_given_tourism_upper_95,
    scenario_tail_impact_weighted = scenario_tail_impact_kwh * pmax(risk_score, 0),
    abs_shock_kwh = abs(delta_kwh_q95_minus_actual),
    abs_tail_impact_kwh = abs(scenario_tail_impact_kwh)
  ) %>%
  arrange(desc(abs_tail_impact_kwh), comune_key, date)

scenario_tail_annual <- scenario_tail_month %>%
  group_by(comune_key, comune_key_norm, comune_nome, istat_muni_code, assigned_model, risk_score,
           p_energy_given_tourism_upper_95, lift_upper_95, tourism_intensity_rank, risk_rank) %>%
  summarise(
    n_months = n(),
    load_actual_assigned_sum = sum(load_actual_assigned, na.rm = TRUE),
    load_q95_assigned_sum = sum(load_q95_assigned, na.rm = TRUE),
    shock_delta_kwh_sum = sum(delta_kwh_q95_minus_actual, na.rm = TRUE),
    shock_delta_kwh_abs_sum = sum(abs_shock_kwh, na.rm = TRUE),
    scenario_tail_impact_kwh_sum = sum(scenario_tail_impact_kwh, na.rm = TRUE),
    scenario_tail_impact_weighted_sum = sum(scenario_tail_impact_weighted, na.rm = TRUE),
    scenario_tail_impact_kwh_mean = mean(scenario_tail_impact_kwh, na.rm = TRUE),
    scenario_tail_impact_kwh_max = max(scenario_tail_impact_kwh, na.rm = TRUE),
    scenario_tail_impact_kwh_min = min(scenario_tail_impact_kwh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    shock_delta_pct_vs_actual = 100 * (load_q95_assigned_sum / pmax(load_actual_assigned_sum, 1e-9) - 1)
  ) %>%
  arrange(desc(scenario_tail_impact_weighted_sum))

regional_summary <- scenario_tail_month %>%
  group_by(date) %>%
  summarise(
    load_actual_assigned = sum(load_actual_assigned, na.rm = TRUE),
    load_q95_assigned = sum(load_q95_assigned, na.rm = TRUE),
    shock_delta_kwh = sum(delta_kwh_q95_minus_actual, na.rm = TRUE),
    scenario_tail_impact_kwh = sum(scenario_tail_impact_kwh, na.rm = TRUE),
    scenario_tail_impact_weighted = sum(scenario_tail_impact_weighted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    shock_delta_pct_vs_actual = 100 * (load_q95_assigned / pmax(load_actual_assigned, 1e-9) - 1)
  ) %>%
  arrange(date)

write_csv(scenario_tail_month, file.path(results_dir, "scenario_tail_impact_by_town_month.csv"))
write_csv(scenario_tail_annual, file.path(results_dir, "scenario_tail_impact_by_town_annual.csv"))
write_csv(regional_summary, file.path(results_dir, "scenario_tail_impact_regional_2025.csv"))

cat("3/5: Joining spatial boundaries for final maps...\n")
shape_rdata <- file.path(modelli_root, "..", "data", "vda_shapefile", "vda_sf.RData")
if (!file.exists(shape_rdata)) {
  stop("Missing shapefile RData: ", shape_rdata)
}

map_env <- new.env()
load(normalizePath(shape_rdata, winslash = "/", mustWork = TRUE), envir = map_env)
if (!exists("vda_sf", envir = map_env)) {
  stop("vda_sf object not found in vda_sf.RData")
}

vda_sf <- get("vda_sf", envir = map_env)
vda_sf$municipality_key_norm <- normalize_key(vda_sf$municipality_key)

map_df <- vda_sf %>%
  left_join(scenario_tail_annual, by = c("municipality_key_norm" = "comune_key_norm"))

cat("4/5: Rendering risk maps and top-20 plot...\n")
plot_map_metric <- function(sf_df, value_col, title_text, out_path, na_label = "No data") {
  p <- ggplot(sf_df) +
    geom_sf(aes(fill = .data[[value_col]]), color = "white", linewidth = 0.1) +
    scale_fill_viridis_c(option = "C", na.value = "grey85", name = value_col) +
    labs(title = title_text, subtitle = na_label) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )

  ggsave(out_path, plot = p, width = 9, height = 6, dpi = 150)
}

plot_map_metric(
  map_df,
  "scenario_tail_impact_weighted_sum",
  "Scenario-tail risk map (weighted annual impact)",
  file.path(assets_dir, "scenario_tail_impact_weighted_map.png")
)

plot_map_metric(
  map_df,
  "scenario_tail_impact_kwh_sum",
  "Scenario-tail risk map (annual impact in kWh)",
  file.path(assets_dir, "scenario_tail_impact_kwh_map.png")
)

plot_map_metric(
  map_df,
  "shock_delta_kwh_sum",
  "Scenario shock map (annual q95 minus actual, kWh)",
  file.path(assets_dir, "scenario_shock_delta_kwh_map.png")
)

top20 <- scenario_tail_annual %>%
  slice_max(order_by = scenario_tail_impact_weighted_sum, n = 20, with_ties = FALSE) %>%
  mutate(comune_plot = ifelse(is.na(comune_nome), comune_key, comune_nome))

p_top <- ggplot(top20, aes(x = reorder(comune_plot, scenario_tail_impact_weighted_sum), y = scenario_tail_impact_weighted_sum, fill = assigned_model)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 towns by scenario-tail weighted impact",
    x = NULL,
    y = "Weighted annual impact (kWh)"
  ) +
  theme_minimal(base_size = 11)

ggsave(file.path(assets_dir, "scenario_tail_top20_weighted.png"), plot = p_top, width = 9, height = 7, dpi = 150)
write_csv(top20, file.path(results_dir, "scenario_tail_top20_weighted.csv"))

cat("5/5: Writing README summary...\n")
readme_lines <- c(
  "# Scenarios Module",
  "",
  "This module integrates q95 tourism-load scenario outputs with municipality tail-risk dependence metrics.",
  "",
  "## Inputs",
  "- ../Forecast/results/scenario_forecast/tourism_shock_forecast_town_2025.csv",
  "- ../Tail Risk/report_assets/municipality_tail_risk_enriched.csv",
  "- ../../data/vda_shapefile/vda_sf.RData",
  "",
  "## Outputs (results)",
  "- scenario_tail_impact_by_town_month.csv",
  "- scenario_tail_impact_by_town_annual.csv",
  "- scenario_tail_impact_regional_2025.csv",
  "- scenario_tail_top20_weighted.csv",
  "",
  "## Outputs (report_assets)",
  "- scenario_tail_impact_weighted_map.png",
  "- scenario_tail_impact_kwh_map.png",
  "- scenario_shock_delta_kwh_map.png",
  "- scenario_tail_top20_weighted.png",
  "",
  "## Run",
  "Rscript scripts/scenario_tail_risk_integration.R"
)

writeLines(readme_lines, con = file.path(scenarios_root, "README.md"))

cat("Done. Scenario-tail integration assets written in Modelli/Scenarios.\n")
