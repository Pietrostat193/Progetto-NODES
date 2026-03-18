library(tidyverse)
library(lubridate)

# ============================================================
# SECTION 3 — Combined tourism + meteo EDA (simple / robust)
# ============================================================

flows_meteo_combined <- read.csv("~/Desktop/Research/UNVDA/data/flows_meteo_combined_english.csv")

# ---------------------------
# 0) Prepare combined dataset
# ---------------------------
combined_eda <- flows_meteo_combined %>%
  mutate(
    year_month = as.Date(year_month),
    year = if ("year" %in% names(.)) as.integer(year) else lubridate::year(year_month),
    month = if ("month" %in% names(.)) as.integer(month) else lubridate::month(year_month),
    month_label = factor(month.abb[month], levels = month.abb),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5)  ~ "Spring",
      month %in% c(6, 7, 8)  ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn",
      TRUE ~ NA_character_
    ),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")),
    days_in_month = lubridate::days_in_month(year_month),
    
    # tourism derived indicators
    avg_length_of_stay = if_else(total_arrivals > 0, total_presences / total_arrivals, NA_real_),
    tourism_pressure = if_else(residents > 0, total_presences / residents, NA_real_),
    bed_utilization_proxy = if_else(number_beds > 0,
                                    total_presences / (number_beds * as.numeric(days_in_month)),
                                    NA_real_)
  )

# ---------------------------
# 1) Pick columns (simple explicit lists)
# ---------------------------
tourism_cols <- intersect(
  c("total_presences", "total_arrivals", "avg_length_of_stay",
    "tourism_pressure", "bed_utilization_proxy", "kwh"),
  names(combined_eda)
)

meteo_cols <- intersect(
  c("temperature", "precipitation", "pressure", "relative_humidity"),
  names(combined_eda)
)

print("Tourism variables used:")
print(tourism_cols)

print("Meteorological variables used:")
print(meteo_cols)

# ---------------------------
# 2) REGIONAL MONTHLY TIME SERIES
#    Average across municipalities (same month)
# ---------------------------
regional_monthly_tourism <- combined_eda %>%
  select(year_month, year, month, month_label, all_of(tourism_cols)) %>%
  pivot_longer(cols = all_of(tourism_cols), names_to = "variable", values_to = "value") %>%
  group_by(year_month, year, month, month_label, variable) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    n_municipalities = n_distinct(combined_eda$municipality[match(year_month, combined_eda$year_month)]),
    n_obs = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  mutate(
    mean_value = ifelse(is.nan(mean_value), NA_real_, mean_value),
    median_value = ifelse(is.nan(median_value), NA_real_, median_value)
  )

regional_monthly_meteo <- combined_eda %>%
  select(year_month, year, month, month_label, all_of(meteo_cols)) %>%
  pivot_longer(cols = all_of(meteo_cols), names_to = "variable", values_to = "value") %>%
  group_by(year_month, year, month, month_label, variable) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    n_obs = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  mutate(
    mean_value = ifelse(is.nan(mean_value), NA_real_, mean_value),
    median_value = ifelse(is.nan(median_value), NA_real_, median_value)
  )

# Plot 2a: Tourism regional monthly time series
p1 <- ggplot(regional_monthly_tourism, aes(x = year_month, y = mean_value)) +
  geom_line() +
  geom_point(size = 0.8) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(
    title = "Regional monthly tourism series (average across municipalities)",
    x = NULL, y = "Mean across municipalities"
  ) +
  theme_minimal()
print(p1)

# Plot 2b: Meteo regional monthly time series
p2 <- ggplot(regional_monthly_meteo, aes(x = year_month, y = mean_value)) +
  geom_line() +
  geom_point(size = 0.8) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(
    title = "Regional monthly meteorological series (average across municipalities)",
    x = NULL, y = "Mean across municipalities"
  ) +
  theme_minimal()
print(p2)

# ---------------------------
# 3) MONTHLY CLIMATOLOGY (average over years)
#    - Tourism and meteo seasonality side by side (separate plots)
# ---------------------------
tourism_month_climatology <- combined_eda %>%
  select(month, month_label, season, all_of(tourism_cols)) %>%
  pivot_longer(cols = all_of(tourism_cols), names_to = "variable", values_to = "value") %>%
  group_by(month, month_label, season, variable) %>%
  summarise(
    climatology_mean = mean(value, na.rm = TRUE),
    climatology_median = median(value, na.rm = TRUE),
    n_obs = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  mutate(
    climatology_mean = ifelse(is.nan(climatology_mean), NA_real_, climatology_mean),
    climatology_median = ifelse(is.nan(climatology_median), NA_real_, climatology_median)
  )

meteo_month_climatology_combined <- combined_eda %>%
  select(month, month_label, season, all_of(meteo_cols)) %>%
  pivot_longer(cols = all_of(meteo_cols), names_to = "variable", values_to = "value") %>%
  group_by(month, month_label, season, variable) %>%
  summarise(
    climatology_mean = mean(value, na.rm = TRUE),
    climatology_median = median(value, na.rm = TRUE),
    n_obs = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  mutate(
    climatology_mean = ifelse(is.nan(climatology_mean), NA_real_, climatology_mean),
    climatology_median = ifelse(is.nan(climatology_median), NA_real_, climatology_median)
  )

p3 <- ggplot(tourism_month_climatology, aes(x = month_label, y = climatology_mean, group = 1)) +
  geom_line() +
  geom_point(size = 1.2) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(
    title = "Tourism monthly climatology (average over years and municipalities)",
    x = "Month", y = "Climatological mean"
  ) +
  theme_minimal()
print(p3)

p4 <- ggplot(meteo_month_climatology_combined, aes(x = month_label, y = climatology_mean, group = 1)) +
  geom_line() +
  geom_point(size = 1.2) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(
    title = "Meteorological monthly climatology (average over years and municipalities)",
    x = "Month", y = "Climatological mean"
  ) +
  theme_minimal()
print(p4)

# ---------------------------
# 4) MUNICIPALITY-LEVEL SAME-MONTH CORRELATIONS
#    (simple, interpretable screen for weather sensitivity)
# ---------------------------
corr_long <- combined_eda %>%
  select(municipality, year_month, all_of(tourism_cols), all_of(meteo_cols)) %>%
  pivot_longer(cols = all_of(tourism_cols), names_to = "tourism_var", values_to = "tourism_value") %>%
  pivot_longer(cols = all_of(meteo_cols), names_to = "meteo_var", values_to = "meteo_value") %>%
  group_by(municipality, tourism_var, meteo_var) %>%
  summarise(
    n_pairs = sum(!is.na(tourism_value) & !is.na(meteo_value)),
    correlation = suppressWarnings(cor(tourism_value, meteo_value, use = "complete.obs")),
    .groups = "drop"
  )

# Summary across municipalities (mean correlation by variable pair)
corr_summary <- corr_long %>%
  group_by(tourism_var, meteo_var) %>%
  summarise(
    mean_corr = mean(correlation, na.rm = TRUE),
    median_corr = median(correlation, na.rm = TRUE),
    min_corr = min(correlation, na.rm = TRUE),
    max_corr = max(correlation, na.rm = TRUE),
    municipalities_n = sum(!is.na(correlation)),
    .groups = "drop"
  )

print(corr_summary)

p5 <- ggplot(corr_summary, aes(x = meteo_var, y = tourism_var, fill = mean_corr)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = round(mean_corr, 2)), size = 3) +
  labs(
    title = "Average same-month correlation across municipalities",
    x = "Meteorological variable", y = "Tourism variable", fill = "Mean corr"
  ) +
  theme_minimal()
print(p5)

# ---------------------------
# 5) SIMPLE SCATTER PLOTS (same-month relationship)
#    Example: top municipalities by total presences
# ---------------------------
top_municipalities <- combined_eda %>%
  group_by(municipality) %>%
  summarise(total_presences_sum = sum(total_presences, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_presences_sum)) %>%
  slice_head(n = 8) %>%
  pull(municipality)

# Scatter 5a: temperature vs total_presences (faceted by municipality), colored by season
scatter_temp_pres <- combined_eda %>%
  filter(municipality %in% top_municipalities) %>%
  select(municipality, season, temperature, total_presences) %>%
  drop_na()

p6 <- ggplot(scatter_temp_pres, aes(x = temperature, y = total_presences, color = season)) +
  geom_point(alpha = 0.65, size = 1.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
  facet_wrap(~ municipality, scales = "free_y") +
  labs(
    title = "Temperature vs tourism presences (same month)",
    x = "Temperature", y = "Total presences"
  ) +
  theme_minimal()
print(p6)

# Scatter 5b: precipitation vs total_arrivals (faceted by municipality)
scatter_prec_arr <- combined_eda %>%
  filter(municipality %in% top_municipalities) %>%
  select(municipality, season, precipitation, total_arrivals) %>%
  drop_na()

p7 <- ggplot(scatter_prec_arr, aes(x = precipitation, y = total_arrivals, color = season)) +
  geom_point(alpha = 0.65, size = 1.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
  facet_wrap(~ municipality, scales = "free_y") +
  labs(
    title = "Precipitation vs tourism arrivals (same month)",
    x = "Precipitation", y = "Total arrivals"
  ) +
  theme_minimal()
print(p7)

# ---------------------------
# 6) OPTIONAL SIMPLE LAG (1-month weather -> current tourism)
#    Still simple, but often insightful
# ---------------------------
combined_lag1 <- combined_eda %>%
  arrange(municipality, year_month) %>%
  group_by(municipality) %>%
  mutate(
    across(all_of(meteo_cols), ~ lag(.x, 1), .names = "{.col}_lag1")
  ) %>%
  ungroup()

lag_corr_summary <- combined_lag1 %>%
  select(municipality, all_of(tourism_cols), ends_with("_lag1")) %>%
  pivot_longer(cols = all_of(tourism_cols), names_to = "tourism_var", values_to = "tourism_value") %>%
  pivot_longer(cols = ends_with("_lag1"), names_to = "meteo_var_lag1", values_to = "meteo_value_lag1") %>%
  group_by(municipality, tourism_var, meteo_var_lag1) %>%
  summarise(
    n_pairs = sum(!is.na(tourism_value) & !is.na(meteo_value_lag1)),
    correlation = suppressWarnings(cor(tourism_value, meteo_value_lag1, use = "complete.obs")),
    .groups = "drop"
  ) %>%
  group_by(tourism_var, meteo_var_lag1) %>%
  summarise(
    mean_corr = mean(correlation, na.rm = TRUE),
    municipalities_n = sum(!is.na(correlation)),
    .groups = "drop"
  )

print(lag_corr_summary)

# ---------------------------
# 7) Save core tables (optional)
# ---------------------------
readr::write_csv(regional_monthly_tourism, "eda3_regional_monthly_tourism.csv", na = "")
readr::write_csv(regional_monthly_meteo, "eda3_regional_monthly_meteo.csv", na = "")
readr::write_csv(tourism_month_climatology, "eda3_tourism_month_climatology.csv", na = "")
readr::write_csv(meteo_month_climatology_combined, "eda3_meteo_month_climatology_combined.csv", na = "")
readr::write_csv(corr_long, "eda3_municipality_same_month_correlations.csv", na = "")
readr::write_csv(corr_summary, "eda3_correlation_summary_across_municipalities.csv", na = "")
readr::write_csv(lag_corr_summary, "eda3_lag1_correlation_summary_across_municipalities.csv", na = "")