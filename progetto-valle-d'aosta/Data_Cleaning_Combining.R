library(readxl)
library(lubridate)
library(tidyverse)
library(janitor)
library(stringr)
library(stringi)

data_flussi <- read_excel("~/Desktop/Research/UNVDA/data/Flussi_consumi_finoSETTEBRE25.xlsx")
data_meteo_daily <- read.csv("~/Desktop/Research/UNVDA/data/meteo_clean_wide_all_stations.csv")

# ============================================================
# 0) KEYING: make municipality keys shapefile-style (no spaces/punct, no accents)
#    + fixes common garbling like ch^atillon, verr`es, pr e saint didier
# ============================================================
make_municipality_key <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- NA_character_
  
  # ensure UTF-8, remove replacement chars
  x <- stringi::stri_enc_toutf8(x, is_unknown_8bit = TRUE)
  x <- str_replace_all(x, "\uFFFD", "")
  
  # fix typical mojibake artifacts observed in your output
  #   ch^atillon -> chatillon
  #   verr`es    -> verres
  x <- str_replace_all(x, "\\^([AEIOUaeiou])", "\\1")
  x <- str_replace_all(x, "([AEIOUaeiou])`", "\\1")
  
  # unify apostrophes, squish whitespace
  x <- str_replace_all(x, "[’`]", "'")
  x <- str_squish(x)
  
  # remove accents properly, then create key: lowercase + only [a-z0-9]
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- str_replace_all(x, "[^a-z0-9]+", "")
  x <- na_if(x, "")
  
  x
}

# ============================================================
# 1) Meteo monthly summaries (safe NA for all-missing groups)
# ============================================================
safe_min    <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max    <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
safe_mean   <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_median <- function(x) if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
safe_sd     <- function(x) if (all(is.na(x))) NA_real_ else sd(x, na.rm = TRUE)

data_meteo_monthly <- data_meteo_daily %>%
  mutate(
    date = as.POSIXct(date, tz = "UTC"),
    year_month = floor_date(date, unit = "month")
  ) %>%
  group_by(
    station_id,
    station_full,
    station_municipality,
    station_location,
    station_latitude,
    station_longitude,
    altitude_mslm,
    year_month
  ) %>%
  summarise(
    across(
      where(is.numeric) & !any_of(c("station_id","station_latitude","station_longitude","altitude_mslm")),
      list(
        mean   = safe_mean,
        median = safe_median,
        min    = safe_min,
        max    = safe_max,
        sd     = safe_sd
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# ============================================================
# 2) Helpers
# ============================================================
nan_to_na <- function(df) {
  df %>% mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x)))
}

# ============================================================
# 3) Rename columns to English + build keys (ALIGNED A PRIORI)
# ============================================================
meteo_translation <- c(
  "precipitazione"   = "precipitation",
  "pressione"        = "pressure",
  "temperatura"      = "temperature",
  "umidit_relativa"  = "relative_humidity"
)

data_flows_en <- data_flussi %>%
  rename(
    municipality = Comune,
    year = Anno,
    month = Mese_num,
    territorial_unit = `Unità territoriale`,
    month_name_it = Mese,
    total_presences = Totale_Presenze,
    total_arrivals = Totale_Arrivi,
    number_accommodations = Numero_alloggi,
    number_beds = Numero_letti,
    residents = Residenti,
    kwh = kWH
  ) %>%
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    year_month = as.Date(sprintf("%04d-%02d-01", year, month)),
    # IMPORTANT: key is now shapefile-style
    municipality_key = make_municipality_key(municipality)
  )

data_meteo_monthly_en <- data_meteo_monthly %>%
  rename(
    station_name = station_full,
    municipality = station_municipality,
    location = station_location,
    latitude = station_latitude,
    longitude = station_longitude,
    altitude_masl = altitude_mslm
  ) %>%
  rename(any_of(meteo_translation)) %>%
  mutate(
    year_month = as.Date(year_month),
    year = year(year_month),
    month = month(year_month),
    # IMPORTANT: key is now shapefile-style
    municipality_key = make_municipality_key(municipality)
  )

# ============================================================
# 4) Identify meteo parameter columns automatically
# ============================================================
meteo_exclude_numeric <- c("station_id", "latitude", "longitude", "altitude_masl", "year", "month")

meteo_param_cols <- data_meteo_monthly_en %>%
  select(where(is.numeric)) %>%
  select(-any_of(meteo_exclude_numeric)) %>%
  names()

print(meteo_param_cols)

# ============================================================
# 5) Aggregate meteo station-month -> municipality-month
#    NOTE: group by municipality_key to avoid splits due to spelling/encoding differences
# ============================================================
meteo_municipality_monthly <- data_meteo_monthly_en %>%
  group_by(municipality_key, year, month, year_month) %>%
  summarise(
    municipality = first(na.omit(municipality)),
    n_stations = n_distinct(station_id),
    
    latitude = mean(latitude, na.rm = TRUE),
    longitude = mean(longitude, na.rm = TRUE),
    altitude_masl = mean(altitude_masl, na.rm = TRUE),
    
    across(all_of(meteo_param_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  nan_to_na()

# ============================================================
# 6) Join flows + meteo (keep all flows rows)
# ============================================================
flows_meteo_combined <- data_flows_en %>%
  left_join(
    meteo_municipality_monthly %>%
      select(
        municipality_key, year_month,
        n_stations, latitude, longitude, altitude_masl,
        all_of(meteo_param_cols)
      ),
    by = c("municipality_key", "year_month")
  ) %>%
  mutate(
    year = year(year_month),
    month = month(year_month)
  ) %>%
  relocate(year, month, year_month, .after = municipality) %>%
  relocate(municipality_key, .after = year_month)

# ============================================================
# 7) Dictionaries (updated grouping consistent with keys)
# ============================================================
station_municipality_dictionary <- data_meteo_monthly_en %>%
  group_by(station_id, station_name, municipality_key, municipality, location) %>%
  summarise(
    latitude = first(na.omit(latitude)),
    longitude = first(na.omit(longitude)),
    altitude_masl = first(na.omit(altitude_masl)),
    first_year_month = min(year_month, na.rm = TRUE),
    last_year_month = max(year_month, na.rm = TRUE),
    n_months = n_distinct(year_month),
    .groups = "drop"
  ) %>%
  arrange(municipality_key, station_id)

municipality_station_dictionary <- station_municipality_dictionary %>%
  group_by(municipality_key) %>%
  summarise(
    municipality = first(na.omit(municipality)),
    n_stations = n_distinct(station_id),
    station_ids = paste(sort(unique(station_id)), collapse = ", "),
    station_names = paste(sort(unique(station_name)), collapse = " | "),
    station_locations = paste(sort(unique(location)), collapse = " | "),
    .groups = "drop"
  ) %>%
  arrange(municipality)

meteo_parameter_dictionary <- data_meteo_monthly_en %>%
  select(station_id, municipality_key, year_month, all_of(meteo_param_cols)) %>%
  pivot_longer(
    cols = all_of(meteo_param_cols),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  group_by(parameter) %>%
  summarise(
    available = any(!is.na(value)),
    non_missing_station_months = sum(!is.na(value)),
    missing_station_months = sum(is.na(value)),
    n_stations_with_data = n_distinct(station_id[!is.na(value)]),
    n_municipalities_with_data = n_distinct(municipality_key[!is.na(value)]),
    start_year_month = suppressWarnings(min(year_month[!is.na(value)], na.rm = TRUE)),
    end_year_month = suppressWarnings(max(year_month[!is.na(value)], na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    start_year_month = ifelse(is.infinite(start_year_month), NA, start_year_month),
    end_year_month = ifelse(is.infinite(end_year_month), NA, end_year_month)
  ) %>%
  arrange(parameter)

municipality_parameter_availability <- data_meteo_monthly_en %>%
  select(municipality_key, municipality, year_month, all_of(meteo_param_cols)) %>%
  pivot_longer(
    cols = all_of(meteo_param_cols),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  group_by(municipality_key, municipality, parameter) %>%
  summarise(
    n_months_with_data = sum(!is.na(value)),
    n_months_total = n_distinct(year_month),
    share_months_with_data = n_months_with_data / n_months_total,
    .groups = "drop"
  ) %>%
  arrange(municipality, parameter)

# ============================================================
# 8) Optional checks for unmatched rows (flows with no meteo)
# ============================================================
unmatched_flows <- flows_meteo_combined %>%
  filter(if_all(all_of(meteo_param_cols), is.na))

n_unmatched <- nrow(unmatched_flows)
print(n_unmatched)

unmatched_flows %>%
  distinct(municipality, municipality_key) %>%
  arrange(municipality) %>%
  print(n = 200)

# ============================================================
# 9) Save outputs
# ============================================================
write_csv(data_flows_en, "data_flows_en.csv", na = "")
write_csv(data_meteo_monthly_en, "data_meteo_monthly_en.csv", na = "")
write_csv(flows_meteo_combined, "flows_meteo_combined_english.csv", na = "")
write_csv(station_municipality_dictionary, "dictionary_station_municipality.csv", na = "")
write_csv(municipality_station_dictionary, "dictionary_municipality_stations.csv", na = "")
write_csv(meteo_parameter_dictionary, "dictionary_meteo_parameters_available.csv", na = "")
write_csv(municipality_parameter_availability, "dictionary_municipality_parameter_availability.csv", na = "")

# Preview
glimpse(flows_meteo_combined)
head(flows_meteo_combined)
