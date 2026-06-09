library(tidyverse)
library(sf)
library(ggplot2)
library(ggrepel)
library(stringi)
library(stringr)

# -----------------------------
# Helpers
# -----------------------------
normalize_muni_key <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]", "")
}

# -----------------------------
# Settings
# -----------------------------
eval_year <- 2021
data_dir <- "~/Desktop/Research/UNVDA/data"

# files produced by the variable-specific LOOCV script
chosen_file <- file.path(
  data_dir,
  paste0("chosen_stations_by_variable_loocv_", eval_year, ".csv")
)

station_ref_file <- file.path(
  data_dir,
  paste0("station_reference_by_variable_loocv_", eval_year, ".csv")
)

# -----------------------------
# Read LOOCV station outputs
# -----------------------------
chosen_stations <- read.csv(chosen_file, stringsAsFactors = FALSE)
station_reference <- read.csv(station_ref_file, stringsAsFactors = FALSE)

# -----------------------------
# Read Valle d'Aosta boundary
# -----------------------------
load("~/Desktop/Research/UNVDA/data/vda_sf.RData")

vda_outline <- vda_sf %>%
  mutate(municipality_key = normalize_muni_key(as.character(municipality_key))) %>%
  st_make_valid() %>%
  st_transform(32632) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_as_sf()

# -----------------------------
# Build sf objects from coordinates
# -----------------------------
station_reference_sf <- station_reference %>%
  st_as_sf(coords = c("x_coord", "y_coord"), crs = 32632, remove = FALSE)

chosen_stations_sf <- chosen_stations %>%
  st_as_sf(coords = c("x_coord", "y_coord"), crs = 32632, remove = FALSE)

# nicer variable labels
var_labs <- c(
  temperatura = "Temperatura",
  pressione = "Pressione",
  umidit_relativa = "Umidità relativa",
  precipitazione = "Precipitazione"
)

# -----------------------------
# Plot
# grey points = all eligible stations for that variable
# black labelled points = 3 selected LOOCV stations for that variable
# -----------------------------
p <- ggplot() +
  geom_sf(data = vda_outline, fill = NA, colour = "grey40", linewidth = 0.4) +
 # geom_sf(
#    data = station_reference_sf,
#    colour = "grey70",
#    size = 1.8,
#    alpha = 0.9
#  ) +
  geom_sf(
    data = chosen_stations_sf,
    aes(shape = distance_band),
    colour = "black",
    size = 3
  ) +
  geom_text_repel(
    data = chosen_stations_sf %>%
      cbind(st_coordinates(.)) %>%
      st_drop_geometry(),
    aes(
      X, Y,
      label = paste0(distance_band, ": ", station_location)
    ),
    size = 3,
    box.padding = 0.3,
    point.padding = 0.2,
    min.segment.length = 0
  ) +
  facet_wrap(~ variable, ncol = 2, labeller = as_labeller(var_labs)) +
  scale_shape_manual(
    values = c(16, 17, 15),
    breaks = c("near", "medium", "far")
  ) +
  labs(
    title = paste("LOOCV station locations by variable -", eval_year),
    subtitle = "Grey = eligible stations for that variable; black = selected near / medium / far stations",
    shape = "Distance band"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

print(p)

# -----------------------------
# Optional save
# -----------------------------
ggsave(
  filename = file.path(data_dir, paste0("loocv_station_locations_by_variable_", eval_year, ".png")),
  plot = p,
  width = 11,
  height = 8,
  dpi = 300
)

library(tidyverse)
library(ggplot2)
library(patchwork)

eval_year <- 2021
data_dir <- "~/Desktop/Research/UNVDA/data"

loocv_summary <- read.csv(
  file.path(data_dir, paste0("loocv_", eval_year, "_summary_metrics_by_variable.csv")),
  stringsAsFactors = FALSE
)

var_labs <- c(
  temperatura = "Temperatura",
  pressione = "Pressione",
  umidit_relativa = "Umidità relativa",
  precipitazione = "Precipitazione"
)

vars_order <- c("temperatura", "pressione", "umidit_relativa", "precipitazione")

plot_list_rmse <- lapply(vars_order, function(v) {
  df_v <- loocv_summary %>%
    filter(variable == v) %>%
    mutate(
      month_name = factor(month_name, levels = c("January", "April", "July", "October")),
      distance_band = factor(distance_band, levels = c("near", "medium", "far"))
    )
  
  ggplot(df_v, aes(x = month_name, y = distance_band, fill = rmse)) +
    geom_tile() +
    labs(
      title = var_labs[[v]],
      x = "Month",
      y = "Distance band",
      fill = "RMSE"
    ) +
    theme_minimal(base_size = 12)
})

wrap_plots(plot_list_rmse, ncol = 2) +
  plot_annotation(title = paste("RMSE heatmaps by variable -", eval_year))


plot_list_mae <- lapply(vars_order, function(v) {
  df_v <- loocv_summary %>%
    filter(variable == v) %>%
    mutate(
      month_name = factor(month_name, levels = c("January", "April", "July", "October")),
      distance_band = factor(distance_band, levels = c("near", "medium", "far"))
    )
  
  ggplot(df_v, aes(x = month_name, y = distance_band, fill = mae)) +
    geom_tile() +
    labs(
      title = var_labs[[v]],
      x = "Month",
      y = "Distance band",
      fill = "MAE"
    ) +
    theme_minimal(base_size = 12)
})

wrap_plots(plot_list_mae, ncol = 2) +
  plot_annotation(title = paste("MAE heatmaps by variable -", eval_year))


loocv_summary <- read.csv(
  file.path(data_dir, paste0("loocv_", eval_year, "_summary_metrics_by_variable.csv")),
  stringsAsFactors = FALSE
)

plot_df <- loocv_summary %>%
  mutate(
    month_name = factor(month_name, levels = c("January", "April", "July", "October")),
    distance_band = factor(distance_band, levels = c("near", "medium", "far")),
    nrmse_mean = rmse / pmax(abs(obs_mean), 1e-8),
    nmae_mean  = mae  / pmax(abs(obs_mean), 1e-8)
  )

ggplot(plot_df, aes(x = month_name, y = distance_band, fill = nrmse_mean)) +
  geom_tile() +
  facet_wrap(~ variable, ncol = 2) +
  theme_minimal(base_size = 12) +
  labs(
    title = paste("Relative RMSE heatmap by variable -", eval_year),
    subtitle = "NRMSE = RMSE / |observed mean|",
    x = "Month",
    y = "Distance band",
    fill = "NRMSE"
  )

ggplot(plot_df, aes(x = month_name, y = distance_band, fill = nmae_mean)) +
  geom_tile() +
  facet_wrap(~ variable, ncol = 2) +
  theme_minimal(base_size = 12) +
  labs(
    title = paste("Relative MAE heatmap by variable -", eval_year),
    subtitle = "NMAE = MAE / |observed mean|",
    x = "Month",
    y = "Distance band",
    fill = "NMAE"
  )

plot_df_z <- loocv_summary %>%
  mutate(
    month_name = factor(month_name, levels = c("January", "April", "July", "October")),
    distance_band = factor(distance_band, levels = c("near", "medium", "far"))
  ) %>%
  group_by(variable) %>%
  mutate(
    rmse_z = (rmse - mean(rmse, na.rm = TRUE)) / sd(rmse, na.rm = TRUE),
    mae_z  = (mae  - mean(mae,  na.rm = TRUE)) / sd(mae,  na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(plot_df_z, aes(x = month_name, y = distance_band, fill = rmse_z)) +
  geom_tile() +
  facet_wrap(~ variable, ncol = 2) +
  theme_minimal(base_size = 12) +
  labs(
    title = paste("Within-variable standardized RMSE -", eval_year),
    subtitle = "Higher values = worse than average for that variable",
    x = "Month",
    y = "Distance band",
    fill = "RMSE z-score"
  )
