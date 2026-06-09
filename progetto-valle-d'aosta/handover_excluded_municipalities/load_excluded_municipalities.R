# =============================================================================
# load_excluded_municipalities.R
#
# PURPOSE
#   Load the historical municipality-month panel for the municipalities that
#   were EXCLUDED from the production tourism-forecasting pipeline because no
#   candidate model reached acceptable out-of-sample accuracy (validation
#   MAPE > 20% on the 2025 window).
#
#   This script ONLY uploads/prepares the data. It fits no models. The goal is
#   to hand a clean, ready-to-use data frame to a collaborator who will try to
#   find appropriate forecasting models for these difficult municipalities.
#
# OUTPUT
#   - In memory: data frame `panel` (all excluded comuni, long format) and a
#     named list `panel_by_comune` (one tibble per municipality).
#   - On disk (in ./data/):
#       excluded_municipalities_panel.csv      (all comuni stacked)
#       <comune_key>.csv                       (one file per municipality)
#
# USAGE
#   From R / RStudio:   source("load_excluded_municipalities.R")
#   From a terminal:    Rscript load_excluded_municipalities.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

# ----------------------------------------------------------------------------
# 0. OPTIONAL: set the data path manually.
#     If you ran into a "Could not find data_full.csv" error (e.g. because you
#     pasted the script into an R console instead of running the whole file),
#     just put the full path to data_full.csv here, for example:
#       data_path_manual <- "C:/path/to/handover_excluded_municipalities/data_full.csv"
#     Otherwise leave it as NULL and the script will auto-detect it.
# ----------------------------------------------------------------------------
data_path_manual <- NULL

# ----------------------------------------------------------------------------
# 1. Locate the source data (data_full.csv) regardless of where this is run.
# ----------------------------------------------------------------------------
get_script_dir <- function() {
  # Works when sourced; falls back to the working directory otherwise.
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(ofile)) {
    return(dirname(normalizePath(ofile, winslash = "/", mustWork = FALSE)))
  }
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]),
                                 winslash = "/", mustWork = FALSE)))
  }
  getwd()
}

script_dir <- get_script_dir()

# data_full.csv ships inside this folder (self-contained). We also search a few
# likely places in case you keep the folder next to the main repository.
data_candidates <- c(
  if (!is.null(data_path_manual)) data_path_manual,             # manual override (section 0)
  file.path(script_dir, "data_full.csv"),                       # copy alongside this script
  file.path(getwd(), "data_full.csv"),                          # current working directory
  file.path(getwd(), "handover_excluded_municipalities", "data_full.csv"),
  file.path(script_dir, "..", "Modelli", "data_full.csv"),       # repo/Modelli
  file.path(script_dir, "..", "Armonizzazione", "data_full.csv"),# repo/Armonizzazione
  file.path(getwd(), "Modelli", "data_full.csv")
)

data_path <- data_candidates[file.exists(data_candidates)][1]

# If still not found, walk a few parent folders up from the working directory
# (covers the case where R was started somewhere inside the repository tree).
if (is.na(data_path)) {
  probe <- getwd()
  for (up in 0:5) {
    hit <- list.files(probe, pattern = "^data_full\\.csv$",
                      recursive = TRUE, full.names = TRUE)
    if (length(hit) > 0) { data_path <- hit[1]; break }
    parent <- dirname(probe)
    if (identical(parent, probe)) break   # reached filesystem root
    probe <- parent
  }
}

# Last resort: if we are in an interactive R session (e.g. the script was
# pasted into the console), just ask the user to point at the file directly.
if (is.na(data_path) && interactive()) {
  message("\nCould not auto-locate data_full.csv.")
  message("A file dialog will now open: please select 'data_full.csv'.\n")
  picked <- tryCatch(file.choose(), error = function(e) NA_character_)
  if (!is.na(picked) && file.exists(picked)) data_path <- picked
}

if (is.na(data_path)) {
  stop(
    "Could not find data_full.csv.\n",
    "  Easiest fix: set the working directory to the folder that contains\n",
    "  data_full.csv, then re-run, e.g.\n",
    "    setwd(\"<path to handover_excluded_municipalities>\")\n",
    "    source(\"load_excluded_municipalities.R\")\n",
    "  Or set `data_path_manual` at the top of this script to the full path of\n",
    "  data_full.csv (e.g. \"C:/.../handover_excluded_municipalities/data_full.csv\").\n",
    "  Paths checked:\n  ",
    paste(data_candidates, collapse = "\n  "),
    call. = FALSE
  )
}
data_path <- normalizePath(data_path, winslash = "/", mustWork = TRUE)
cat("Reading panel data from:\n  ", data_path, "\n", sep = "")

# ----------------------------------------------------------------------------
# 2. The 11 excluded municipalities (comune_key as stored in data_full.csv).
# ----------------------------------------------------------------------------
excluded_keys <- c(
  "valpelline",
  "allein",
  "valgrisenche",
  "pont saint martin",
  "avise",
  "bionaz",
  "saint denis",
  "la magdeleine",
  "doues",
  "rhemes notre dame",
  "la thuile"
)

# ----------------------------------------------------------------------------
# 3. Load, type, and filter.
# ----------------------------------------------------------------------------
raw <- read.csv(data_path, stringsAsFactors = FALSE, check.names = TRUE)

# Columns most relevant for tourism / electricity modelling. Everything else in
# data_full.csv is kept too, but these are the ones a forecaster typically needs.
core_cols <- c(
  "comune_key", "comune_nome", "istat_muni_code", "date", "year", "month_num",
  "totale_arrivi", "totale_presenze", "numero_alloggi", "numero_letti",
  "residenti", "kwh",
  "temperatura", "precipitazione", "pressione", "umidit_relativa"
)
missing_core <- setdiff(core_cols, names(raw))
if (length(missing_core) > 0) {
  warning("Some expected columns are absent and will be skipped: ",
          paste(missing_core, collapse = ", "))
  core_cols <- intersect(core_cols, names(raw))
}

panel <- raw %>%
  mutate(comune_key = tolower(trimws(as.character(comune_key)))) %>%
  filter(comune_key %in% excluded_keys) %>%
  mutate(
    date          = as.Date(substr(as.character(date), 1, 10)),
    totale_arrivi = pmax(as.numeric(totale_arrivi), 0),
    kwh           = ifelse(as.numeric(kwh) < 0, NA_real_, as.numeric(kwh)),
    # Common transforms a modeller will likely want:
    log_arrivi    = log1p(totale_arrivi),
    log_kwh       = log1p(kwh)
  ) %>%
  arrange(comune_key, date)

# Sanity check: did we find all 11?
found_keys <- sort(unique(panel$comune_key))
not_found  <- setdiff(excluded_keys, found_keys)
if (length(not_found) > 0) {
  warning("These excluded keys were NOT found in data_full.csv: ",
          paste(not_found, collapse = ", "))
}

# ----------------------------------------------------------------------------
# 4. Per-municipality list and a short coverage summary.
# ----------------------------------------------------------------------------
panel_by_comune <- split(panel, panel$comune_key)

coverage <- panel %>%
  group_by(comune_key, comune_nome) %>%
  summarise(
    n_months    = dplyr::n(),
    first_month = min(date, na.rm = TRUE),
    last_month  = max(date, na.rm = TRUE),
    n_kwh_obs   = sum(!is.na(kwh)),
    n_arr_obs   = sum(!is.na(totale_arrivi)),
    .groups = "drop"
  ) %>%
  arrange(comune_key)

cat("\nExcluded municipalities loaded:", length(found_keys), "of",
    length(excluded_keys), "\n\n")
print(as.data.frame(coverage), row.names = FALSE)

# ----------------------------------------------------------------------------
# 5. Write tidy CSVs for the collaborator.
# ----------------------------------------------------------------------------
out_dir <- file.path(script_dir, "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(panel,
          file.path(out_dir, "excluded_municipalities_panel.csv"),
          row.names = FALSE)

for (k in names(panel_by_comune)) {
  safe <- gsub("[^a-z0-9]+", "_", k)
  write.csv(panel_by_comune[[k]],
            file.path(out_dir, paste0(safe, ".csv")),
            row.names = FALSE)
}

cat("\nWrote combined panel and", length(panel_by_comune),
    "per-municipality CSVs to:\n  ", normalizePath(out_dir, winslash = "/"),
    "\n", sep = "")
cat("\nObjects available in the session:\n",
    "  panel            - all excluded comuni (long format)\n",
    "  panel_by_comune  - named list, one tibble per municipality\n",
    "  coverage         - per-municipality data coverage summary\n", sep = "")
