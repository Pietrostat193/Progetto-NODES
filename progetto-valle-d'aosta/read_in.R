# Data Scraping for meteorological variables

setwd("~/Desktop/Research/UNVDA/data")

library(tidyverse)
library(lubridate)
library(janitor)

# ---------------------------
# SETTINGS
# ---------------------------
raw_dir <- "Meteo_Raw"
out_file <- file.path("~/Desktop/Research/UNVDA/data", "meteo_clean_wide_all_stations.csv")
param_dict_file <- file.path(raw_dir, "meteo_parameter_dictionary.csv")

# ---------------------------
# HELPERS
# ---------------------------
first_non_na <- function(x) {
  y <- x[!is.na(x)]
  if (length(y) == 0) NA_real_ else y[1]
}

extract_meta <- function(meta_lines, field) {
  pat <- paste0("^", field, "\\s*:\\s*")
  hit <- meta_lines[str_detect(meta_lines, pat)][1]
  if (is.na(hit)) return(NA_character_)
  str_trim(str_remove(hit, pat))
}

read_meteo_file <- function(path) {
  # Read raw lines (try to keep accents)
  
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- str_replace_all(lines, "\ufeff", "")  # remove BOM if present
  lines <- str_replace_all(lines, "\r", "")
  lines[lines == ""] <- NA
  lines <- lines[!is.na(lines)]
  
  # Find first actual data line (starts with timestamp)
  data_start <- which(str_detect(lines, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"))[1]
  if (is.na(data_start)) {
    warning("No data rows found in: ", basename(path))
    return(tibble())
  }
  
  meta <- str_trim(lines[seq_len(data_start - 1)])
  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[str_trim(data_lines) != ""]
  
  # ---- Metadata ----
  station_full <- extract_meta(meta, "Stazione")
  lat_chr      <- extract_meta(meta, "Latitudine wgs84")
  lon_chr      <- extract_meta(meta, "Longitudine wgs84")
  alt_chr      <- extract_meta(meta, "Quota mslm")
  param_label  <- extract_meta(meta, "Parametro")
  unit_label   <- extract_meta(meta, "Unit� misura")
  
  # Split "Municipality - Location" (separator is space-hyphen-space)
  st_split <- str_split_fixed(station_full %||% "", " - ", 2)
  station_municipality <- na_if(str_trim(st_split[, 1]), "")
  station_location     <- na_if(str_trim(st_split[, 2]), "")
  
  # Station ID from filename, e.g. Dati_3550-Precipitazione.csv -> 3550
  station_id <- str_extract(basename(path), "(?<=Dati_)\\d+")
  
  # Clean parameter column name (e.g. "Precipitazione (pluvio bascula)" -> "precipitazione")
  # Keep only the text before parentheses to avoid long names
  param_base <- param_label %>%
    str_remove("\\s*\\(.*\\)\\s*$") %>%
    str_trim()
  parameter_name <- make_clean_names(param_base)
  
  # ---- Data rows ----
  # Extract timestamp at start + last numeric token in line as value (robust to weird separators)
  out <- tibble(raw_line = data_lines) %>%
    mutate(
      date_chr  = str_extract(raw_line, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"),
      # last numeric token (integer/decimal, comma or dot, optional scientific notation)
      value_chr = str_extract(raw_line, "[-+]?\\d+(?:[\\.,]\\d+)?(?:[eE][-+]?\\d+)?\\s*$"),
      value_chr = str_trim(value_chr),
      value_chr = na_if(value_chr, ""),
      value_num = suppressWarnings(as.numeric(str_replace(value_chr, ",", "."))),
      date      = ymd_hms(date_chr, quiet = TRUE)
    ) %>%
    transmute(
      station_id = station_id,
      station_full = station_full,
      station_municipality = station_municipality,
      station_location = station_location,
      station_latitude = suppressWarnings(as.numeric(str_replace(lat_chr, ",", "."))),
      station_longitude = suppressWarnings(as.numeric(str_replace(lon_chr, ",", "."))),
      altitude_mslm = suppressWarnings(as.numeric(str_replace(alt_chr, ",", "."))),
      parameter = param_label,
      parameter_name = parameter_name,
      unit = unit_label,
      date = date,
      value = value_num,
      source_file = basename(path)
    ) %>%
    filter(!is.na(date))
  
  out
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

# ---------------------------
# READ ALL FILES
# ---------------------------
files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)

meteo_long <- map_dfr(files, read_meteo_file)


# Optional: inspect parameter names found
message("Parameters found (clean names):")
print(sort(unique(meteo_long$parameter_name)))

# Save a parameter dictionary (clean name -> original label + unit)
parameter_dictionary <- meteo_long %>%
  distinct(parameter_name, parameter, unit) %>%
  arrange(parameter_name)

write_csv(parameter_dictionary, param_dict_file, na = "")

# ---------------------------
# BUILD FINAL CLEAN WIDE DATASET
# ---------------------------
meteo_clean_wide <- meteo_long %>%
  group_by(
    station_id, station_full, station_municipality, station_location,
    station_latitude, station_longitude, altitude_mslm,
    date, parameter_name
  ) %>%
  summarise(value = first_non_na(value), .groups = "drop") %>%
  pivot_wider(
    names_from = parameter_name,
    values_from = value
  ) %>%
  arrange(station_id, date)

meteo_clean_wide <- meteo_clean_wide %>% mutate(date = as.Date(date))

meteo_clean_wide <- meteo_clean_wide %>%
  mutate(across(
    where(is.character),
    ~ stringr::str_replace_all(.x, "\uFFFD", "")
  ))

# Write final dataset
write_csv(meteo_clean_wide, out_file, na = "")





