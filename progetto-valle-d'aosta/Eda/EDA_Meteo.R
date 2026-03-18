library(tidyverse)
library(lubridate)

# ============================================================
# SECTION 2 — Meteorological variables EDA (per station + per year)
# ============================================================

data_meteo_monthly_en <- read.csv("data_meteo_monthly_en.csv")

# ---------------------------
# 0) Prepare data
# ---------------------------
meteo_eda <- data_meteo_monthly_en %>%
  mutate(
    year_month = as.Date(year_month),
    year = if ("year" %in% names(.)) as.integer(year) else lubridate::year(year_month),
    month = if ("month" %in% names(.)) as.integer(month) else lubridate::month(year_month),
    month_label = factor(month.abb[month], levels = month.abb)
  )

# Numeric metadata to exclude from "meteo parameters"
numeric_exclude <- c("station_id", "latitude", "longitude", "altitude_masl", "year", "month")

meteo_param_cols <- meteo_eda %>%
  select(where(is.numeric)) %>%
  select(-any_of(numeric_exclude)) %>%
  names()

print("Detected meteorological parameter columns:")
print(meteo_param_cols)

# Long format (best for EDA)
meteo_long <- meteo_eda %>%
  select(
    station_id, station_name, municipality, location,
    latitude, longitude, altitude_masl,
    year_month, year, month, month_label,
    all_of(meteo_param_cols)
  ) %>%
  pivot_longer(
    cols = all_of(meteo_param_cols),
    names_to = "parameter",
    values_to = "value"
  )

# ---------------------------
# 1) TIME SERIES — monthly average across all stations (regional monthly signal)
#    (combines stations, keeps time)
# ---------------------------
meteo_monthly_regional <- meteo_long %>%
  group_by(year_month, year, month, month_label, parameter) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    n_stations_with_data = n_distinct(station_id[!is.na(value)]),
    .groups = "drop"
  ) %>%
  mutate(across(c(mean_value, median_value, sd_value), ~ ifelse(is.nan(.x), NA_real_, .x)))

print(head(meteo_monthly_regional, 20))

p1 <- ggplot(meteo_monthly_regional, aes(x = year_month, y = mean_value)) +
  geom_line() +
  geom_point(size = 0.8) +
  facet_wrap(~ parameter, scales = "free_y", ncol = 2) +
  labs(
    title = "Monthly meteorological time series (average across stations)",
    x = NULL, y = "Mean across stations"
  ) +
  theme_minimal()

print(p1)

# ---------------------------
# 2) ANNUAL SUMMARY — average across months and stations (per year)
#    (combines stations + months, keeps year)
# ---------------------------
meteo_annual_regional <- meteo_long %>%
  group_by(year, parameter) %>%
  summarise(
    annual_mean_value = mean(value, na.rm = TRUE),
    annual_median_value = median(value, na.rm = TRUE),
    n_station_month_obs = sum(!is.na(value)),
    n_stations = n_distinct(station_id[!is.na(value)]),
    .groups = "drop"
  ) %>%
  mutate(across(c(annual_mean_value, annual_median_value), ~ ifelse(is.nan(.x), NA_real_, .x)))

print(head(meteo_annual_regional, 20))

p2 <- ggplot(meteo_annual_regional, aes(x = year, y = annual_mean_value)) +
  geom_line() +
  geom_point(size = 1.2) +
  facet_wrap(~ parameter, scales = "free_y", ncol = 2) +
  labs(
    title = "Annual meteorological summary (average across stations and months)",
    x = "Year", y = "Annual mean"
  ) +
  theme_minimal()

print(p2)

# ---------------------------
# 3) MONTHLY CLIMATOLOGY — average over years and stations (per month)
#    (combines years + stations, keeps month)
# ---------------------------
meteo_month_climatology <- meteo_long %>%
  group_by(month, month_label, parameter) %>%
  summarise(
    climatology_mean = mean(value, na.rm = TRUE),
    climatology_median = median(value, na.rm = TRUE),
    climatology_sd = sd(value, na.rm = TRUE),
    n_obs = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  mutate(across(c(climatology_mean, climatology_median, climatology_sd), ~ ifelse(is.nan(.x), NA_real_, .x)))

print(head(meteo_month_climatology, 20))

p3 <- ggplot(meteo_month_climatology, aes(x = month_label, y = climatology_mean, group = 1)) +
  geom_line() +
  geom_point(size = 1.5) +
  facet_wrap(~ parameter, scales = "free_y", ncol = 2) +
  labs(
    title = "Monthly climatology (average over years and stations)",
    x = "Month", y = "Climatological mean"
  ) +
  theme_minimal()

print(p3)

# ---------------------------
# 4) PER STATION + YEAR — annual average by station (outputs per station and per year)
# ---------------------------
meteo_station_year <- meteo_long %>%
  group_by(station_id, station_name, municipality, location, altitude_masl, year, parameter) %>%
  summarise(
    annual_mean_value = mean(value, na.rm = TRUE),
    n_months_with_data = n_distinct(month[!is.na(value)]),
    .groups = "drop"
  ) %>%
  mutate(annual_mean_value = ifelse(is.nan(annual_mean_value), NA_real_, annual_mean_value))

print(head(meteo_station_year, 30))

# A simple station-year time series (faceted by station) for each parameter
# To avoid too many panels, keep top stations with most data
top_stations <- meteo_station_year %>%
  group_by(station_id, station_name) %>%
  summarise(n = sum(!is.na(annual_mean_value)), .groups = "drop") %>%
  arrange(desc(n)) %>%
  slice_head(n = 12) %>%
  pull(station_id)

meteo_station_year_top <- meteo_station_year %>%
  filter(station_id %in% top_stations)

p4 <- ggplot(meteo_station_year_top, aes(x = year, y = annual_mean_value, group = station_id)) +
  geom_line(alpha = 0.7) +
  geom_point(size = 1) +
  facet_grid(parameter ~ station_name, scales = "free_y") +
  labs(
    title = "Station-level annual averages (top stations by data availability)",
    x = "Year", y = "Annual mean"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p4)

# ---------------------------
# 5) PER STATION MONTHLY CLIMATOLOGY — average over years (station seasonality)
#    (combines years, keeps station + month)
# ---------------------------
meteo_station_month_climatology <- meteo_long %>%
  group_by(station_id, station_name, municipality, location, month, month_label, parameter) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    n_years_with_data = n_distinct(year[!is.na(value)]),
    .groups = "drop"
  ) %>%
  mutate(mean_value = ifelse(is.nan(mean_value), NA_real_, mean_value))

print(head(meteo_station_month_climatology, 30))

meteo_station_month_climatology_top <- meteo_station_month_climatology %>%
  filter(station_id %in% top_stations)

p5 <- ggplot(meteo_station_month_climatology_top,
             aes(x = month_label, y = mean_value, group = station_id)) +
  geom_line(alpha = 0.7) +
  geom_point(size = 1) +
  facet_grid(parameter ~ station_name, scales = "free_y") +
  labs(
    title = "Station monthly climatology (average over years)",
    x = "Month", y = "Mean value"
  ) +
  theme_minimal()

print(p5)

# ---------------------------
# 6) Coverage check (simple)
# ---------------------------
meteo_coverage_station_year <- meteo_long %>%
  group_by(station_id, station_name, year, parameter) %>%
  summarise(
    n_months_with_data = n_distinct(month[!is.na(value)]),
    .groups = "drop"
  )

print(head(meteo_coverage_station_year, 30))

p6 <- ggplot(
  meteo_coverage_station_year %>% filter(station_id %in% top_stations),
  aes(x = factor(year), y = fct_reorder(station_name, station_id), fill = n_months_with_data)
) +
  geom_tile(color = "white", linewidth = 0.2) +
  facet_wrap(~ parameter, ncol = 2) +
  labs(
    title = "Coverage by station-year (# months with data)",
    x = "Year", y = "Station", fill = "# months"
  ) +
  theme_minimal()

print(p6)

# ---------------------------
# 7) Save core EDA tables (optional)
# ---------------------------
readr::write_csv(meteo_monthly_regional, "eda2_meteo_monthly_regional.csv", na = "")
readr::write_csv(meteo_annual_regional, "eda2_meteo_annual_regional.csv", na = "")
readr::write_csv(meteo_month_climatology, "eda2_meteo_month_climatology.csv", na = "")
readr::write_csv(meteo_station_year, "eda2_meteo_station_year.csv", na = "")
readr::write_csv(meteo_station_month_climatology, "eda2_meteo_station_month_climatology.csv", na = "")
readr::write_csv(meteo_coverage_station_year, "eda2_meteo_coverage_station_year.csv", na = "")
