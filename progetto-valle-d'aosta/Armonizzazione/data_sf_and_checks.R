# Final data view

library(sf)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(scales)
library(janitor)

data <- read.csv("~/Desktop/Research/UNVDA/data/data_full.csv")

muni <- st_read("~/Desktop/Research/UNVDA/data/Com01012025_g_WGS84.shp", quiet = TRUE) %>%
  clean_names()

names(muni)


data <- data %>%
  mutate(
    istat_muni_code = str_pad(as.character(istat_muni_code), width = 6, pad = "0")
  )

muni <- muni %>%
  mutate(
    istat_muni_code = str_pad(as.character(pro_com_t), width = 6, pad = "0")
  )

muni_vda <- muni %>%
  filter(cod_reg == 2)


data_sf <- muni_vda %>%
  select(istat_muni_code, geometry) %>%
  right_join(data, by = "istat_muni_code") %>%
  st_as_sf()

# 7. Check result
print(data_sf)
plot(st_geometry(data_sf))


data_sf <- data_sf %>%
  mutate(
    date = as.Date(date),
    istat_muni_code = str_pad(as.character(istat_muni_code), 6, pad = "0"),
    comune_nome = as.character(comune_nome)
  )

meteo_vars   <- c("temperatura", "pressione", "umidit_relativa", "precipitazione")
tourism_vars <- c("totale_presenze", "totale_arrivi")

# -----------------------------
# 1. Choose one month automatically for mapping
#    (best common coverage for temperature + precipitation)
# -----------------------------
map_vars <- c("temperatura", "precipitazione")

month_to_plot <- as.Date("2023-12-01")

cat("Month selected for municipality maps:", as.character(month_to_plot), "\n")

# -----------------------------
# 2. Municipality maps for a couple of variables
# -----------------------------
map_df <- data_sf %>%
  filter(date == month_to_plot) %>%
  select(comune_nome, date, all_of(map_vars), geometry) %>%
  pivot_longer(
    cols = all_of(map_vars),
    names_to = "variable",
    values_to = "value"
  )

p_map <- ggplot(map_df) +
  geom_sf(aes(fill = value), color = "white", linewidth = 0.15) +
  facet_wrap(~ variable, ncol = 2) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(
    title = paste("Municipality values in", format(month_to_plot, "%Y-%m")),
    fill = NULL
  ) +
  theme_minimal()

print(p_map)

# -----------------------------
# 3. Pick 4 municipalities automatically
#    (best meteo coverage across all 4 variables)
# -----------------------------
muni_sel <- data_sf %>%
  st_drop_geometry() %>%
  group_by(comune_nome) %>%
  summarise(
    score =
      sum(!is.na(temperatura)) +
      sum(!is.na(pressione)) +
      sum(!is.na(umidit_relativa)) +
      sum(!is.na(precipitazione)),
    .groups = "drop"
  ) %>%
  arrange(desc(score), comune_nome) %>%
  slice(5:8) %>%
  pull(comune_nome)

cat("Municipalities selected for time series:\n")
print(muni_sel)

# -----------------------------
# 4. Time series of the 4 meteo variables
# -----------------------------
meteo_ts <- data_sf %>%
  st_drop_geometry() %>%
  filter(comune_nome %in% muni_sel) %>%
  select(comune_nome, date, all_of(meteo_vars)) %>%
  pivot_longer(
    cols = all_of(meteo_vars),
    names_to = "variable",
    values_to = "value"
  )

p_meteo <- ggplot(meteo_ts, aes(x = date, y = value, group = comune_nome)) +
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  facet_grid(variable ~ comune_nome, scales = "free_y") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Time series of the 4 meteo variables",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_meteo)

# -----------------------------
# 5. Time series for tourism variables
# -----------------------------
tourism_ts <- data_sf %>%
  st_drop_geometry() %>%
  filter(comune_nome %in% muni_sel) %>%
  select(comune_nome, date, all_of(tourism_vars)) %>%
  pivot_longer(
    cols = all_of(tourism_vars),
    names_to = "variable",
    values_to = "value"
  )

p_tourism <- ggplot(tourism_ts, aes(x = date, y = value, group = comune_nome)) +
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  facet_grid(variable ~ comune_nome, scales = "free_y") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Tourism time series",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_tourism)

