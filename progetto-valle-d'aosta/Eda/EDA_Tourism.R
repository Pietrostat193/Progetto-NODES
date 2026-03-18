# Exploratory Analysis Tourism Flows

library(tidyverse)
library(lubridate)
library(slider)

# ============================================================
# SECTION 1 — Tourism flows only EDA
# ============================================================

data_flows_en <- read.csv("~/Desktop/Research/UNVDA/data/data_flows_en.csv")

# ---------------------------
# 0) Build tourism EDA variables
# ---------------------------
flows_eda <- data_flows_en %>%
  mutate(
    year_month = as.Date(year_month),
    year = year(year_month),
    month = month(year_month),
    days_in_month = lubridate::days_in_month(year_month),
    
    log_kwh = log1p(kwh),
    kwh_per_resident = if_else(residents > 0, kwh / residents, NA_real_),
    kwh_per_presence = if_else(total_presences > 0, kwh / total_presences, NA_real_),
    kwh_per_arrival  = if_else(total_arrivals  > 0, kwh / total_arrivals,  NA_real_),
    
    # Core interpretable indicators
    avg_length_of_stay = if_else(total_arrivals > 0, total_presences / total_arrivals, NA_real_),
    tourism_pressure = if_else(residents > 0, total_presences / residents, NA_real_),
    bed_utilization_proxy = if_else(number_beds > 0,
                                    total_presences / (number_beds * as.numeric(days_in_month)),
                                    NA_real_),
    
    # Useful transformed outcome for plots/models
    log_presences = log1p(total_presences),
    
    # Seasons (meteorological)
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5)  ~ "Spring",
      month %in% c(6, 7, 8)  ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn",
      TRUE ~ NA_character_
    ),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")),
    month_label = factor(month.abb[month], levels = month.abb)
  )

flows_eda <- flows_eda %>% mutate(kwh = as.numeric(kwh))

# ---------------------------
# 1) Municipality summary table (tourism signature)
# ---------------------------
tourism_signature <- flows_eda %>%
  group_by(municipality) %>%
  summarise(
    n_months = n_distinct(year_month),
    total_presences_sum = sum(total_presences, na.rm = TRUE),
    total_arrivals_sum = sum(total_arrivals, na.rm = TRUE),
    avg_monthly_presences = mean(total_presences, na.rm = TRUE),
    avg_length_of_stay = mean(avg_length_of_stay, na.rm = TRUE),
    avg_tourism_pressure = mean(tourism_pressure, na.rm = TRUE),
    avg_bed_utilization_proxy = mean(bed_utilization_proxy, na.rm = TRUE),
    sd_monthly_presences = sd(total_presences, na.rm = TRUE),
    cv_monthly_presences = sd_monthly_presences / avg_monthly_presences,
    peak_month_share = max(total_presences, na.rm = TRUE) / sum(total_presences, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_presences_sum))

print(tourism_signature, n = 20)

# Choose top municipalities by total presences for plots
top_n_municipalities <- 12
top_municipalities <- tourism_signature %>%
  slice_head(n = top_n_municipalities) %>%
  pull(municipality)

# ---------------------------
# 2) Monthly time series + 12-month rolling mean (top municipalities)
# ---------------------------
flows_ts_top <- flows_eda %>%
  filter(municipality %in% top_municipalities) %>%
  arrange(municipality, year_month) %>%
  group_by(municipality) %>%
  mutate(
    presences_roll12 = slide_dbl(
      total_presences,
      ~ mean(.x, na.rm = TRUE),
      .before = 11, .complete = FALSE
    ),
    arrivals_roll12 = slide_dbl(
      total_arrivals,
      ~ mean(.x, na.rm = TRUE),
      .before = 11, .complete = FALSE
    )
  ) %>%
  ungroup()

# Plot 2a: Presences time series
ggplot(flows_ts_top, aes(x = year_month, y = total_presences)) +
  geom_line(alpha = 0.45) +
  geom_line(aes(y = presences_roll12), linewidth = 0.7) +
  facet_wrap(~ municipality, scales = "free_y") +
  labs(
    title = "Monthly tourism presences by municipality",
    subtitle = "Raw monthly values + 12-month rolling mean",
    x = "Month", y = "Total presences"
  ) +
  theme_minimal()

# Plot 2b: Arrivals time series (optional)
ggplot(flows_ts_top, aes(x = year_month, y = total_arrivals)) +
  geom_line(alpha = 0.45) +
  geom_line(aes(y = arrivals_roll12), linewidth = 0.7) +
  facet_wrap(~ municipality, scales = "free_y") +
  labs(
    title = "Monthly tourism arrivals by municipality",
    subtitle = "Raw monthly values + 12-month rolling mean",
    x = NULL, y = "Total arrivals"
  ) +
  theme_minimal()

# ---------------------------
# 3) Seasonality heatmap (pick one municipality or loop)
# ---------------------------
# Example: first top municipality
example_municipality <- top_municipalities[1]

flows_heatmap_one <- flows_eda %>%
  filter(municipality == example_municipality) %>%
  mutate(year = factor(year),
         month_label = factor(month.abb[month], levels = month.abb))

ggplot(flows_heatmap_one, aes(x = month_label, y = year, fill = total_presences)) +
  geom_tile(color = "white", linewidth = 0.2) +
  labs(
    title = paste("Seasonality heatmap:", example_municipality),
    x = "Month", y = "Year", fill = "Presences"
  ) +
  theme_minimal()

# Optional: Heatmap faceted for top 6 municipalities (average over years instead)
flows_heatmap_profile <- flows_eda %>%
  filter(municipality %in% top_municipalities[1:min(6, length(top_municipalities))]) %>%
  group_by(municipality, month, month_label) %>%
  summarise(mean_presences = mean(total_presences, na.rm = TRUE), .groups = "drop")

ggplot(flows_heatmap_profile, aes(x = month_label, y = municipality, fill = mean_presences)) +
  geom_tile(color = "white", linewidth = 0.2) +
  labs(
    title = "Average seasonal pattern (top municipalities)",
    x = "Month", y = NULL, fill = "Mean presences"
  ) +
  theme_minimal()

# ---------------------------
# 4) Month-of-year tourism profile (seasonal signature)
# ---------------------------
monthly_profile <- flows_eda %>%
  filter(municipality %in% top_municipalities) %>%
  group_by(municipality, month, month_label) %>%
  summarise(
    mean_presences = mean(total_presences, na.rm = TRUE),
    mean_arrivals = mean(total_arrivals, na.rm = TRUE),
    mean_length_of_stay = mean(avg_length_of_stay, na.rm = TRUE),
    mean_tourism_pressure = mean(tourism_pressure, na.rm = TRUE),
    mean_bed_utilization = mean(bed_utilization_proxy, na.rm = TRUE),
    .groups = "drop"
  )

# Plot 4a: Seasonal profile of presences
ggplot(monthly_profile, aes(x = month_label, y = mean_presences, group = municipality)) +
  geom_line(alpha = 0.7) +
  geom_point(size = 1.2) +
  facet_wrap(~ municipality, scales = "free_y") +
  labs(
    title = "Average month-of-year tourism profile",
    subtitle = "Seasonality signature by municipality",
    x = "Month", y = "Average monthly presences"
  ) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

# Plot 4b: Seasonal profile of tourism pressure (optional)
ggplot(monthly_profile, aes(x = month_label, y = mean_tourism_pressure, group = municipality)) +
  geom_line(alpha = 0.7) +
  geom_point(size = 1.2) +
  facet_wrap(~ municipality, scales = "free_y") +
  labs(
    title = "Average month-of-year tourism pressure",
    subtitle = "Presences / residents",
    x = "Month", y = "Tourism pressure"
  ) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

# ---------------------------
# 5) Structural change check (pre/post split by year)
#    Useful if you want to inspect shifts in seasonality over time
# ---------------------------
split_year <- 2020  # change if needed

monthly_profile_prepost <- flows_eda %>%
  filter(municipality %in% top_municipalities) %>%
  mutate(period = if_else(year <= split_year, paste0("<= ", split_year), paste0("> ", split_year))) %>%
  group_by(municipality, period, month, month_label) %>%
  summarise(mean_presences = mean(total_presences, na.rm = TRUE), .groups = "drop")

ggplot(monthly_profile_prepost,
       aes(x = month_label, y = mean_presences, color = period, group = period)) +
  geom_line() +
  geom_point(size = 1.1) +
  facet_wrap(~ municipality, scales = "free_y") +
  labs(
    title = "Seasonality profile before vs after split year",
    x = "Month", y = "Average monthly presences", color = "Period"
  ) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

# ---------------------------
# 6) Save EDA tables (optional)
# ---------------------------
readr::write_csv(tourism_signature, "eda_tourism_signature_by_municipality.csv", na = "")
readr::write_csv(monthly_profile, "eda_tourism_monthly_profile_by_municipality.csv", na = "")
readr::write_csv(monthly_profile_prepost, "eda_tourism_monthly_profile_prepost.csv", na = "")


# ============================================================
# SECTION 1B — Energy consumption EDA (municipality level)
# ============================================================

# ---------------------------
# 1) Municipality energy signature (rank + intensity proxies)
# ---------------------------
energy_signature <- flows_eda %>%
  group_by(municipality) %>%
  summarise(
    total_kwh_sum = sum(kwh, na.rm = TRUE),
    avg_monthly_kwh = mean(kwh, na.rm = TRUE),
    avg_kwh_per_resident = mean(kwh_per_resident, na.rm = TRUE),
    avg_kwh_per_presence = mean(kwh_per_presence, na.rm = TRUE),
    sd_monthly_kwh = sd(kwh, na.rm = TRUE),
    cv_monthly_kwh = sd_monthly_kwh / avg_monthly_kwh,
    .groups = "drop"
  ) %>%
  arrange(desc(total_kwh_sum))

print(energy_signature, n = 20)

top_energy_municipalities <- energy_signature %>%
  slice_head(n = 12) %>%
  pull(municipality)

# ---------------------------
# 2) Energy time series + 12-month rolling mean (top by kWh)
# ---------------------------
energy_ts_top <- flows_eda %>%
  filter(municipality %in% top_energy_municipalities) %>%
  arrange(municipality, year_month) %>%
  group_by(municipality) %>%
  mutate(
    kwh_roll12 = slider::slide_dbl(
      kwh,
      ~ mean(.x, na.rm = TRUE),
      .before = 11, .complete = FALSE
    )
  ) %>%
  ungroup()

ggplot(energy_ts_top, aes(x = year_month, y = kwh)) +
  geom_line(alpha = 0.45) +
  geom_line(aes(y = kwh_roll12), linewidth = 0.7) +
  facet_wrap(~ municipality, scales = "free_y") +
  labs(
    title = "Monthly energy consumption (kWh) by municipality",
    subtitle = "Raw monthly values + 12-month rolling mean (top municipalities by total kWh)",
    x = "Month", y = "kWh"
  ) +
  theme_minimal()

# ---------------------------
# 3) System-level shock plot: indexed Tourism vs Energy (all municipalities)
#    Helps see COVID-like breaks / regime shifts immediately
# ---------------------------
agg_series <- flows_eda %>%
  group_by(year_month) %>%
  summarise(
    presences = sum(total_presences, na.rm = TRUE),
    arrivals  = sum(total_arrivals,  na.rm = TRUE),
    kwh_total = sum(kwh,            na.rm = TRUE),
    .groups = "drop"
  )

baseline_year <- 2019
baseline_vals <- agg_series %>%
  filter(lubridate::year(year_month) == baseline_year) %>%
  summarise(
    base_presences = mean(presences, na.rm = TRUE),
    base_kwh = mean(kwh_total, na.rm = TRUE)
  )

agg_indexed <- agg_series %>%
  mutate(
    presences_index = 100 * presences / baseline_vals$base_presences,
    kwh_index       = 100 * kwh_total / baseline_vals$base_kwh
  ) %>%
  select(year_month, presences_index, kwh_index) %>%
  pivot_longer(-year_month, names_to = "series", values_to = "index") %>%
  mutate(series = recode(series,
                         presences_index = "Tourism presences (index)",
                         kwh_index       = "Energy kWh (index)"))

ggplot(agg_indexed, aes(x = year_month, y = index, color = series)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = paste0("Tourism vs Energy (indexed to ", baseline_year, " average = 100)"),
    subtitle = "Aggregated over all municipalities: highlights structural breaks and relative volatility",
    x = "Month", y = "Index", color = NULL
  ) +
  theme_minimal()

# ---------------------------
# 4) Energy–Tourism relationship (log-log scatter), by season
#    Quick way to see coupling/decoupling and seasonal clusters
# ---------------------------
scatter_df <- flows_eda %>%
  filter(
    municipality %in% top_municipalities,   # reuse your top tourism list (or switch to top_energy_municipalities)
    !is.na(kwh), !is.na(total_presences),
    kwh > 0, total_presences > 0
  )

ggplot(scatter_df, aes(x = total_presences, y = kwh, color = season)) +
  geom_point(alpha = 0.35, size = 1.1) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  facet_wrap(~ municipality, scales = "free") +
  labs(
    title = "Energy consumption vs tourism presences (log-log)",
    subtitle = "Season-colored; fitted line per facet helps detect coupling and outliers",
    x = "Total presences (log scale)", y = "kWh (log scale)", color = "Season"
  ) +
  theme_minimal()

# ---------------------------
# 5) Seasonality profile for energy (month-of-year signature)
#    Add mean_kwh to your existing monthly_profile pipeline and plot it
# ---------------------------
monthly_profile_energy <- flows_eda %>%
  filter(municipality %in% top_municipalities) %>%
  group_by(municipality, month, month_label) %>%
  summarise(
    mean_kwh = mean(kwh, na.rm = TRUE),
    mean_kwh_per_resident = mean(kwh_per_resident, na.rm = TRUE),
    mean_kwh_per_presence = mean(kwh_per_presence, na.rm = TRUE),
    .groups = "drop"
  )

# Plot 5a: Seasonal profile of kWh
ggplot(monthly_profile_energy, aes(x = month_label, y = mean_kwh, group = municipality)) +
  geom_line(alpha = 0.7) +
  geom_point(size = 1.2) +
  facet_wrap(~ municipality, scales = "free_y") +
  labs(
    title = "Average month-of-year energy profile",
    subtitle = "Seasonality signature of kWh by municipality",
    x = "Month", y = "Average monthly kWh"
  ) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

# Plot 5b: Seasonal profile of kWh per presence (tourism-adjusted intensity)
ggplot(monthly_profile_energy, aes(x = month_label, y = mean_kwh_per_presence, group = municipality)) +
  geom_line(alpha = 0.7) +
  geom_point(size = 1.2) +
  facet_wrap(~ municipality, scales = "free_y") +
  labs(
    title = "Average month-of-year energy intensity per tourist presence",
    subtitle = "kWh / presences (higher values suggest energy not explained by tourism alone)",
    x = "Month", y = "kWh per presence"
  ) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

# ---------------------------
# 6) Lag diagnostic: does energy track tourism with delay?
#    Correlation of kWh with presences shifted by 0..12 months (aggregate series)
# ---------------------------
lag_df <- tibble(lag_months = 0:12) %>%
  mutate(
    corr = purrr::map_dbl(lag_months, ~ cor(
      agg_series$kwh_total,
      dplyr::lag(agg_series$presences, .x),
      use = "pairwise.complete.obs"
    ))
  )

ggplot(lag_df, aes(x = lag_months, y = corr)) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  scale_x_continuous(breaks = 0:12) +
  labs(
    title = "Lag correlation: Energy vs Tourism",
    subtitle = "Correlation between total kWh and presences shifted by lag months (0 = same month)",
    x = "Lag (months): presences shifted back", y = "Correlation"
  ) +
  theme_minimal()

