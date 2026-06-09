# Diagnose and visualize the comuni flagged as "implausible" by BASE (and METHOD_D)
# on the 2025 validation. Produces:
#   - report_assets/allocation_implausible_municipalities.csv   (table with reasons)
#   - report_assets/allocation_implausible_map.png              (choropleth)
#   - report_assets/allocation_implausible_forecast_vs_actual.png (small multiples)
#
# Uses cached forecasts in allocation_comparison_forecasts.csv (no model refit).

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
  library(ggplot2); library(sf); library(stringi)
})

script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE)),
  error = function(e) getwd()
)
if (!nzchar(script_dir) || is.na(script_dir)) script_dir <- getwd()

assets_dir <- file.path(script_dir, "report_assets")
data_path  <- file.path(script_dir, "..", "data_full.csv")
shp_path   <- file.path(script_dir, "..", "..", "data", "vda_shapefile",
                        "Com01012025_g_WGS84.shp")
stopifnot(file.exists(assets_dir), file.exists(data_path), file.exists(shp_path))

# ---- load -------------------------------------------------------------------
data_full <- read.csv(data_path, stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(substr(date, 1, 10)),
         totale_arrivi = pmax(as.numeric(totale_arrivi), 0)) %>%
  filter(!is.na(date))

per_c <- read.csv(file.path(assets_dir, "allocation_comparison_per_comune.csv"),
                  stringsAsFactors = FALSE)
fc    <- read.csv(file.path(assets_dir, "allocation_comparison_forecasts.csv"),
                  stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(substr(date, 1, 10)))

# ---- focus on the chosen production method ---------------------------------
target_method <- "METHOD_F+meteo_l0.10"
per_base <- per_c %>% filter(method == target_method)
fc_base  <- fc    %>% filter(method == target_method)

# Implausibility rule: |rel_total| > 0.5  OR  MAPE > 0.75
flag <- per_base %>%
  mutate(
    flag_rel  = !is.na(rel_total) & abs(rel_total) > 0.5,
    flag_mape = !is.na(mape) & mape > 0.75,
    implausible = flag_rel | flag_mape,
    reason = case_when(
      flag_rel & flag_mape ~ "rel_total>50% AND MAPE>75%",
      flag_rel             ~ "rel_total>50%",
      flag_mape            ~ "MAPE>75%",
      TRUE                 ~ "ok"
    )
  )

impl <- flag %>%
  filter(implausible) %>%
  arrange(desc(abs(rel_total))) %>%
  mutate(
    actual_total = round(actual_total),
    pred_total   = round(pred_total, 1),
    rel_total_pct = round(100 * rel_total, 1),
    mape         = round(mape, 3),
    bias         = round(bias, 1),
    mae          = round(mae, 1)
  ) %>%
  select(comune_key, comune_nome, n_obs,
         actual_total, pred_total, rel_total_pct, mape, mae, bias, reason)

cat(sprintf("\nMethod under audit: %s\n", target_method))
cat(sprintf("Implausible comuni: %d / %d  (%.1f%%)\n",
            nrow(impl), nrow(flag), 100 * mean(flag$implausible)))
print(as.data.frame(impl), row.names = FALSE)

out_tag <- gsub("[^A-Za-z0-9]+", "_", target_method)
write.csv(impl, file.path(assets_dir, sprintf("allocation_implausible_municipalities_%s.csv", out_tag)),
          row.names = FALSE)

# ---- map --------------------------------------------------------------------
make_key <- function(x) {
  x %>% as.character() %>% stri_trans_general("Latin-ASCII") %>%
    tolower() %>% gsub("[^a-z0-9]+", "", .)
}

vda_sf <- st_read(shp_path, quiet = TRUE)
names(vda_sf) <- tolower(names(vda_sf))
vda_sf <- vda_sf %>%
  filter(as.integer(cod_reg) == 2) %>%
  mutate(join_key = make_key(comune)) %>%
  select(join_key, comune_nome = comune, geometry)

# Normalize forecast-side keys to the same compact form (no spaces, no accents)
flag_join <- flag %>% mutate(join_key = make_key(comune_nome))
impl_join <- impl %>% mutate(join_key = make_key(comune_nome))

map_df <- vda_sf %>%
  left_join(flag_join %>% select(join_key, rel_total, mape, implausible),
            by = "join_key") %>%
  mutate(
    rel_pct = 100 * rel_total,
    rel_pct_capped = pmax(pmin(rel_pct, 80), -80)  # cap for color scale
  )

missing_in_sf <- setdiff(flag_join$join_key, vda_sf$join_key)
if (length(missing_in_sf) > 0) {
  cat("WARN: comuni present in forecasts but NOT in shapefile:\n  ",
      paste(missing_in_sf, collapse = ", "), "\n")
}

impl_labels <- map_df %>% filter(join_key %in% impl_join$join_key)

p_map <- ggplot(map_df) +
  geom_sf(aes(fill = rel_pct_capped), color = "grey40", linewidth = 0.15) +
  scale_fill_gradient2(low = "#2c7bb6", mid = "white", high = "#d7191c",
                       midpoint = 0, limits = c(-80, 80),
                       name = "Errore rel.\nsul totale\n2025 (%)",
                       na.value = "grey90") +
  geom_sf(data = impl_labels, fill = NA, color = "black", linewidth = 0.7) +
  geom_sf_text(data = impl_labels, aes(label = comune_nome),
               size = 2.8, fontface = "bold", check_overlap = TRUE) +
  labs(
    title = "Allocazione regionale -> comunale: errore sul totale 2025",
    subtitle = sprintf("Metodo %s  -  comuni con bordo nero: |rel_total| > 50%% o MAPE > 75%%",
                       target_method),
    caption = "Scala colore satura a +/-80% per leggibilita'."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank())

ggsave(file.path(assets_dir, sprintf("allocation_implausible_map_%s.png", out_tag)),
       p_map, width = 10, height = 7, dpi = 150)

# ---- forecast-vs-actual plot for implausible comuni -------------------------
val_start <- min(fc_base$date); val_end <- max(fc_base$date)
hist_start <- val_start %m-% years(2)

panel_df <- bind_rows(
  data_full %>%
    filter(date >= hist_start, date <= val_end,
           comune_key %in% impl$comune_key) %>%
    group_by(comune_key, comune_nome, date) %>%
    summarise(value = sum(totale_arrivi), .groups = "drop") %>%
    mutate(series = "osservato"),
  fc_base %>%
    filter(comune_key %in% impl$comune_key) %>%
    select(comune_key, comune_nome, date, value = forecast) %>%
    mutate(series = sprintf("forecast (%s)", target_method))
) %>%
  left_join(impl %>% select(comune_key, rel_total_pct, mape),
            by = "comune_key") %>%
  mutate(
    label = sprintf("%s\n(actual=%.0f, pred=%.0f, rel=%+.0f%%, MAPE=%.2f)",
                    comune_nome,
                    impl$actual_total[match(comune_key, impl$comune_key)],
                    impl$pred_total[match(comune_key, impl$comune_key)],
                    rel_total_pct, mape)
  )

p_panel <- ggplot(panel_df, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.6) +
  geom_point(data = subset(panel_df, date >= val_start), size = 1.3) +
  geom_vline(xintercept = val_start, linetype = "dashed", color = "grey40") +
  facet_wrap(~ label, scales = "free_y", ncol = 2) +
  scale_color_manual(values = setNames(c("black", "#d7191c"),
                                       c("osservato", sprintf("forecast (%s)", target_method)))) +
  labs(
    title = sprintf("Comuni 'implausibili' sotto %s: forecast vs osservato", target_method),
    subtitle = sprintf("Ultimi 24 mesi di training + validazione 2025 (%s -> %s)",
                       val_start, val_end),
    x = NULL, y = "arrivi mensili", color = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top",
        strip.text = element_text(size = 8))

ggsave(file.path(assets_dir, sprintf("allocation_implausible_forecast_vs_actual_%s.png", out_tag)),
       p_panel, width = 11, height = max(4, 2.2 * ceiling(nrow(impl) / 2)),
       dpi = 150, limitsize = FALSE)

cat("\nWritten:\n")
cat("  ", file.path(assets_dir, sprintf("allocation_implausible_municipalities_%s.csv", out_tag)), "\n")
cat("  ", file.path(assets_dir, sprintf("allocation_implausible_map_%s.png", out_tag)), "\n")
cat("  ", file.path(assets_dir, sprintf("allocation_implausible_forecast_vs_actual_%s.png", out_tag)), "\n")
