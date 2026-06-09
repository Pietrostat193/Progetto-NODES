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

parse_arg <- function(args, key, default) {
  hit <- grep(paste0("^--", key, "="), args, value = TRUE)
  if (length(hit) == 0) {
    return(default)
  }
  sub(paste0("^--", key, "="), "", hit[[1]])
}

required_pkgs <- c("dplyr", "forecast", "ggplot2", "lme4", "lubridate", "slider", "tibble", "tidyr", "gbm")
to_install <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages(to_install)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(forecast)
  library(ggplot2)
  library(lme4)
  library(lubridate)
  library(slider)
  library(tibble)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
horizon <- as.integer(parse_arg(args, "horizon", "12"))
nsim <- as.integer(parse_arg(args, "nsim", "300"))
validation_start <- as.Date(parse_arg(args, "validation_start", "2025-01-01"))
validation_end <- as.Date(parse_arg(args, "validation_end", "2025-09-01"))
training_start <- as.Date("2021-01-01")
run_mode <- tolower(parse_arg(args, "mode", "full"))

if (!run_mode %in% c("select", "full")) {
  stop("`mode` must be either 'select' (regional model selection only, no bootstrap) or 'full' (default: model selection + bootstrap + scenarios).", call. = FALSE)
}

if (is.na(horizon) || horizon < 1) {
  stop("`horizon` must be a positive integer.", call. = FALSE)
}

if (is.na(nsim) || nsim < 10) {
  stop("`nsim` must be an integer >= 10.", call. = FALSE)
}

cat("Run mode:", run_mode, "(use --mode=select to skip the bootstrap and run only the regional model-selection block).\n")

script_dir <- get_script_dir()
output_dir <- file.path(script_dir, "report_assets")

data_candidates <- c(
  file.path(script_dir, "data_full.csv"),
  file.path(script_dir, "..", "data_full.csv"),
  file.path(getwd(), "Modelli", "data_full.csv"),
  file.path(getwd(), "data_full.csv")
)

data_hits <- data_candidates[file.exists(data_candidates)]
if (length(data_hits) == 0) {
  stop("File CSV non trovato. Percorsi controllati: ", paste(data_candidates, collapse = " | "), call. = FALSE)
}

data_path <- normalizePath(data_hits[[1]], winslash = "/", mustWork = TRUE)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("Reading tourism forecast data from:", data_path, "\n")

data <- read.csv(data_path, stringsAsFactors = FALSE)

required_cols <- c(
  "date", "comune_key", "comune_nome", "totale_arrivi",
  "temperatura", "precipitazione", "pressione", "umidit_relativa"
)

missing_cols <- setdiff(required_cols, names(data))
if (length(missing_cols) > 0) {
  stop("Colonne mancanti nel dataset: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

data <- data %>%
  mutate(
    date = as.Date(substr(as.character(date), 1, 10)),
    comune_key = as.factor(comune_key),
    comune_nome = as.character(comune_nome),
    totale_arrivi = pmax(as.numeric(totale_arrivi), 0),
    temperatura = as.numeric(temperatura),
    precipitazione = pmax(as.numeric(precipitazione), 0),
    pressione = as.numeric(pressione),
    umidit_relativa = as.numeric(umidit_relativa)
  ) %>%
  filter(!is.na(date)) %>%
  arrange(comune_key, date)

global_start <- min(data$date, na.rm = TRUE)

data_fe <- data %>%
  mutate(
    year = year(date),
    month_num = month(date),
    log_totale_arrivi = log1p(totale_arrivi),
    log_precipitazione = log1p(precipitazione),
    t_global = 12 * (year(date) - year(global_start)) + (month(date) - month(global_start))
  ) %>%
  group_by(comune_key) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    log_totale_arrivi_lag1 = lag(log_totale_arrivi, 1),
    log_totale_arrivi_lag12 = lag(log_totale_arrivi, 12),
    sin_m1 = sin(2 * pi * month_num / 12),
    cos_m1 = cos(2 * pi * month_num / 12),
    sin_m2 = sin(2 * pi * 2 * month_num / 12),
    cos_m2 = cos(2 * pi * 2 * month_num / 12),
    sin_m3 = sin(2 * pi * 3 * month_num / 12),
    cos_m3 = cos(2 * pi * 3 * month_num / 12)
  ) %>%
  ungroup() %>%
  filter(date >= training_start)

train_end <- validation_start %m-% months(1)

fit_df <- data_fe %>%
  filter(
    date >= training_start,
    date <= train_end,
    complete.cases(
      log_totale_arrivi,
      log_totale_arrivi_lag1,
      log_totale_arrivi_lag12,
      temperatura,
      log_precipitazione,
      pressione,
      umidit_relativa,
      t_global,
      sin_m1, cos_m1, sin_m2, cos_m2, sin_m3, cos_m3
    )
  )

if (nrow(fit_df) == 0) {
  stop("Nessuna osservazione disponibile per il fit del modello turismo.", call. = FALSE)
}

validation_df <- data_fe %>%
  filter(date >= validation_start, date <= validation_end)

if (nrow(validation_df) == 0) {
  stop("Nessuna osservazione disponibile nel blocco di validazione richiesto.", call. = FALSE)
}

cat("Fitting tourism mixed-effects model on", nrow(fit_df), "rows.\n")

tourism_formula <- log_totale_arrivi ~
  log_totale_arrivi_lag1 +
  log_totale_arrivi_lag12 +
  temperatura +
  log_precipitazione +
  pressione +
  umidit_relativa +
  t_global +
  sin_m1 + cos_m1 +
  sin_m2 + cos_m2 +
  sin_m3 + cos_m3 +
  (1 | comune_key)

ctrl <- lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
tourism_model <- lme4::lmer(tourism_formula, data = fit_df, REML = FALSE, control = ctrl)

cat("Model fit completed. Preparing recursive simulations.\n")

sigma_eps <- sigma(tourism_model)
fixef_vec <- lme4::fixef(tourism_model)
ranef_vec <- lme4::ranef(tourism_model)$comune_key[, "(Intercept)"]
fixed_terms <- stats::delete.response(stats::terms(lme4::nobars(tourism_formula)))

municipality_lookup <- fit_df %>%
  group_by(comune_key) %>%
  summarise(comune_nome = dplyr::first(na.omit(comune_nome)), .groups = "drop")

municipality_names <- setNames(
  as.character(municipality_lookup$comune_nome),
  as.character(municipality_lookup$comune_key)
)

monthly_climatology <- fit_df %>%
  group_by(comune_key, month_num) %>%
  summarise(
    temperatura = mean(temperatura, na.rm = TRUE),
    log_precipitazione = mean(log_precipitazione, na.rm = TRUE),
    pressione = mean(pressione, na.rm = TRUE),
    umidit_relativa = mean(umidit_relativa, na.rm = TRUE),
    .groups = "drop"
  )

global_climatology <- fit_df %>%
  group_by(month_num) %>%
  summarise(
    temperatura = mean(temperatura, na.rm = TRUE),
    log_precipitazione = mean(log_precipitazione, na.rm = TRUE),
    pressione = mean(pressione, na.rm = TRUE),
    umidit_relativa = mean(umidit_relativa, na.rm = TRUE),
    .groups = "drop"
  )

overall_climatology <- fit_df %>%
  summarise(
    temperatura = mean(temperatura, na.rm = TRUE),
    log_precipitazione = mean(log_precipitazione, na.rm = TRUE),
    pressione = mean(pressione, na.rm = TRUE),
    umidit_relativa = mean(umidit_relativa, na.rm = TRUE)
  )

weather_cols <- c("temperatura", "log_precipitazione", "pressione", "umidit_relativa")

monthly_weather_lookup <- setNames(
  split(monthly_climatology[weather_cols], paste(monthly_climatology$comune_key, monthly_climatology$month_num, sep = "|")),
  paste(monthly_climatology$comune_key, monthly_climatology$month_num, sep = "|")
)

global_weather_lookup <- setNames(
  split(global_climatology[weather_cols], as.character(global_climatology$month_num)),
  as.character(global_climatology$month_num)
)

actual_weather_lookup <- setNames(
  split(validation_df[weather_cols], paste(validation_df$comune_key, validation_df$date, sep = "|")),
  paste(validation_df$comune_key, validation_df$date, sep = "|")
)

history_tbl <- data_fe %>%
  select(comune_key, comune_nome, date, log_totale_arrivi) %>%
  arrange(comune_key, date)

history_by_comune <- split(history_tbl$log_totale_arrivi, history_tbl$comune_key)

train_history_tbl <- history_tbl %>%
  filter(date <= train_end)

train_history_by_comune <- split(train_history_tbl$log_totale_arrivi, train_history_tbl$comune_key)

last_date <- max(history_tbl$date, na.rm = TRUE)
future_dates <- seq(floor_date(last_date %m+% months(1), unit = "month"), by = "1 month", length.out = horizon)

validation_dates <- seq(validation_start, validation_end, by = "1 month")

scenario_tbl <- tibble::tribble(
  ~scenario, ~multiplier,
  "baseline", 1.00,
  "high_tourism", 1.10,
  "low_tourism", 0.90,
  "shock", 1.25
)

get_weather_values <- function(comune_id, future_date, month_id, weather_source) {
  comune_id <- as.character(comune_id)
  month_key <- paste(comune_id, month_id, sep = "|")
  date_key <- paste(comune_id, as.character(future_date), sep = "|")

  weather_row <- NULL

  if (weather_source == "actual") {
    weather_row <- actual_weather_lookup[[date_key]]
  }

  if (is.null(weather_row)) {
    weather_row <- monthly_weather_lookup[[month_key]]
  }

  global_row <- global_weather_lookup[[as.character(month_id)]]

  tibble(
    temperatura = dplyr::coalesce(
      if (!is.null(weather_row)) weather_row$temperatura[[1]] else NA_real_,
      if (!is.null(global_row)) global_row$temperatura[[1]] else NA_real_,
      overall_climatology$temperatura[[1]]
    ),
    log_precipitazione = dplyr::coalesce(
      if (!is.null(weather_row)) weather_row$log_precipitazione[[1]] else NA_real_,
      if (!is.null(global_row)) global_row$log_precipitazione[[1]] else NA_real_,
      overall_climatology$log_precipitazione[[1]]
    ),
    pressione = dplyr::coalesce(
      if (!is.null(weather_row)) weather_row$pressione[[1]] else NA_real_,
      if (!is.null(global_row)) global_row$pressione[[1]] else NA_real_,
      overall_climatology$pressione[[1]]
    ),
    umidit_relativa = dplyr::coalesce(
      if (!is.null(weather_row)) weather_row$umidit_relativa[[1]] else NA_real_,
      if (!is.null(global_row)) global_row$umidit_relativa[[1]] else NA_real_,
      overall_climatology$umidit_relativa[[1]]
    )
  )
}

get_random_intercept <- function(comune_id) {
  comune_effect <- unname(ranef_vec[as.character(comune_id)])
  if (length(comune_effect) == 0 || is.na(comune_effect)) {
    return(0)
  }
  comune_effect
}

simulate_one_path <- function(history_values, comune_id, scenario_mult, target_dates, weather_source = c("climatology", "actual")) {
  weather_source <- match.arg(weather_source)
  simulated_log <- numeric(length(target_dates))
  history_extended <- history_values

  for (h in seq_along(target_dates)) {
    future_date <- target_dates[[h]]
    month_id <- month(future_date)
    t_global_future <- 12 * (year(future_date) - year(global_start)) + (month(future_date) - month(global_start))

    climate_vals <- get_weather_values(comune_id, future_date, month_id, weather_source)

    lag1_val <- history_extended[[length(history_extended)]]
    lag12_val <- history_extended[[length(history_extended) - 11]]

    newdata <- tibble(
      log_totale_arrivi_lag1 = lag1_val,
      log_totale_arrivi_lag12 = lag12_val,
      temperatura = climate_vals$temperatura[[1]],
      log_precipitazione = climate_vals$log_precipitazione[[1]],
      pressione = climate_vals$pressione[[1]],
      umidit_relativa = climate_vals$umidit_relativa[[1]],
      t_global = t_global_future,
      sin_m1 = sin(2 * pi * month_id / 12),
      cos_m1 = cos(2 * pi * month_id / 12),
      sin_m2 = sin(2 * pi * 2 * month_id / 12),
      cos_m2 = cos(2 * pi * 2 * month_id / 12),
      sin_m3 = sin(2 * pi * 3 * month_id / 12),
      cos_m3 = cos(2 * pi * 3 * month_id / 12)
    )

    design_matrix <- model.matrix(fixed_terms, newdata)
    eta <- as.numeric(design_matrix %*% fixef_vec) + get_random_intercept(comune_id) + log(scenario_mult)
    y_next <- stats::rnorm(1, mean = eta, sd = sigma_eps)
    y_next <- max(y_next, 0)

    simulated_log[[h]] <- y_next
    history_extended <- c(history_extended, y_next)
  }

  simulated_log
}

forecast_multilevel_path <- function(history_values, comune_id, target_dates, weather_source = c("climatology", "actual")) {
  weather_source <- match.arg(weather_source)
  predicted_log <- numeric(length(target_dates))
  lower_log <- numeric(length(target_dates))
  upper_log <- numeric(length(target_dates))
  history_extended <- history_values

  for (h in seq_along(target_dates)) {
    future_date <- target_dates[[h]]
    month_id <- month(future_date)
    t_global_future <- 12 * (year(future_date) - year(global_start)) + (month(future_date) - month(global_start))

    climate_vals <- get_weather_values(comune_id, future_date, month_id, weather_source)
    lag1_val <- history_extended[[length(history_extended)]]
    lag12_val <- history_extended[[length(history_extended) - 11]]

    newdata <- tibble(
      log_totale_arrivi_lag1 = lag1_val,
      log_totale_arrivi_lag12 = lag12_val,
      temperatura = climate_vals$temperatura[[1]],
      log_precipitazione = climate_vals$log_precipitazione[[1]],
      pressione = climate_vals$pressione[[1]],
      umidit_relativa = climate_vals$umidit_relativa[[1]],
      t_global = t_global_future,
      sin_m1 = sin(2 * pi * month_id / 12),
      cos_m1 = cos(2 * pi * month_id / 12),
      sin_m2 = sin(2 * pi * 2 * month_id / 12),
      cos_m2 = cos(2 * pi * 2 * month_id / 12),
      sin_m3 = sin(2 * pi * 3 * month_id / 12),
      cos_m3 = cos(2 * pi * 3 * month_id / 12)
    )

    design_matrix <- model.matrix(fixed_terms, newdata)
    eta <- as.numeric(design_matrix %*% fixef_vec) + get_random_intercept(comune_id)

    predicted_log[[h]] <- eta
    lower_log[[h]] <- eta - 1.645 * sigma_eps
    upper_log[[h]] <- eta + 1.645 * sigma_eps
    history_extended <- c(history_extended, eta)
  }

  tibble(
    date = target_dates,
    q05 = pmax(exp(lower_log) - 1, 0),
    q50 = pmax(exp(predicted_log) - 1, 0),
    q95 = pmax(exp(upper_log) - 1, 0)
  )
}

set.seed(123)

if (run_mode == "select") {
  cat("--mode=select: skipping forward scenarios and validation simulations (nsim loops).\n")
  sim_results <- tibble()
  quantile_summary <- tibble()
  regional_summary <- tibble()
  validation_results <- tibble()
  validation_quantiles <- tibble()
  validation_regional <- tibble()
} else {

sim_results <- vector("list", length = nrow(scenario_tbl) * length(levels(fit_df$comune_key)) * nsim)
row_id <- 1L

cat("Running forward scenarios with", nsim, "simulations per municipality and scenario.\n")

for (scenario_idx in seq_len(nrow(scenario_tbl))) {
  scenario_name <- scenario_tbl$scenario[[scenario_idx]]
  scenario_mult <- scenario_tbl$multiplier[[scenario_idx]]

  for (comune_id in levels(fit_df$comune_key)) {
    history_values <- history_by_comune[[as.character(comune_id)]]
    comune_nome <- municipality_names[[as.character(comune_id)]]

    if (length(history_values) < 12) {
      next
    }

    for (sim_idx in seq_len(nsim)) {
      simulated_log <- simulate_one_path(
        history_values = history_values,
        comune_id = comune_id,
        scenario_mult = scenario_mult,
        target_dates = future_dates,
        weather_source = "climatology"
      )
      sim_results[[row_id]] <- tibble(
        scenario = scenario_name,
        sim = sim_idx,
        comune_key = comune_id,
        comune_nome = comune_nome,
        date = future_dates,
        log_totale_arrivi_sim = simulated_log,
        totale_arrivi_sim = pmax(exp(simulated_log) - 1, 0)
      )
      row_id <- row_id + 1L
    }
  }
}

sim_results <- bind_rows(sim_results)

if (nrow(sim_results) == 0) {
  stop("La simulazione non ha prodotto risultati. Controllare la cronologia disponibile per comune.", call. = FALSE)
}

quantile_summary <- sim_results %>%
  group_by(scenario, comune_key, comune_nome, date) %>%
  summarise(
    mean_arrivals = mean(totale_arrivi_sim, na.rm = TRUE),
    q05 = quantile(totale_arrivi_sim, 0.05, na.rm = TRUE),
    q25 = quantile(totale_arrivi_sim, 0.25, na.rm = TRUE),
    q50 = quantile(totale_arrivi_sim, 0.50, na.rm = TRUE),
    q75 = quantile(totale_arrivi_sim, 0.75, na.rm = TRUE),
    q95 = quantile(totale_arrivi_sim, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

regional_summary <- sim_results %>%
  group_by(scenario, sim, date) %>%
  summarise(total_arrivals = sum(totale_arrivi_sim, na.rm = TRUE), .groups = "drop") %>%
  group_by(scenario, date) %>%
  summarise(
    mean_arrivals = mean(total_arrivals, na.rm = TRUE),
    q05 = quantile(total_arrivals, 0.05, na.rm = TRUE),
    q25 = quantile(total_arrivals, 0.25, na.rm = TRUE),
    q50 = quantile(total_arrivals, 0.50, na.rm = TRUE),
    q75 = quantile(total_arrivals, 0.75, na.rm = TRUE),
    q95 = quantile(total_arrivals, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

validation_results <- vector("list", length = nrow(scenario_tbl) * length(levels(fit_df$comune_key)) * nsim)
row_id <- 1L

cat("Running validation scenarios for", as.character(validation_start), "to", as.character(validation_end), ".\n")

for (scenario_idx in seq_len(nrow(scenario_tbl))) {
  scenario_name <- scenario_tbl$scenario[[scenario_idx]]
  scenario_mult <- scenario_tbl$multiplier[[scenario_idx]]

  for (comune_id in levels(fit_df$comune_key)) {
    history_values <- train_history_by_comune[[as.character(comune_id)]]
    comune_nome <- municipality_names[[as.character(comune_id)]]

    if (length(history_values) < 12) {
      next
    }

    for (sim_idx in seq_len(nsim)) {
      simulated_log <- simulate_one_path(
        history_values = history_values,
        comune_id = comune_id,
        scenario_mult = scenario_mult,
        target_dates = validation_dates,
        weather_source = "actual"
      )

      validation_results[[row_id]] <- tibble(
        scenario = scenario_name,
        sim = sim_idx,
        comune_key = comune_id,
        comune_nome = comune_nome,
        date = validation_dates,
        log_totale_arrivi_sim = simulated_log,
        totale_arrivi_sim = pmax(exp(simulated_log) - 1, 0)
      )
      row_id <- row_id + 1L
    }
  }
}

validation_results <- bind_rows(validation_results)

if (nrow(validation_results) == 0) {
  stop("La simulazione di validazione non ha prodotto risultati.", call. = FALSE)
}

validation_quantiles <- validation_results %>%
  group_by(scenario, comune_key, comune_nome, date) %>%
  summarise(
    mean_arrivals = mean(totale_arrivi_sim, na.rm = TRUE),
    q05 = quantile(totale_arrivi_sim, 0.05, na.rm = TRUE),
    q25 = quantile(totale_arrivi_sim, 0.25, na.rm = TRUE),
    q50 = quantile(totale_arrivi_sim, 0.50, na.rm = TRUE),
    q75 = quantile(totale_arrivi_sim, 0.75, na.rm = TRUE),
    q95 = quantile(totale_arrivi_sim, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

validation_regional <- validation_results %>%
  group_by(scenario, sim, date) %>%
  summarise(total_arrivals = sum(totale_arrivi_sim, na.rm = TRUE), .groups = "drop") %>%
  group_by(scenario, date) %>%
  summarise(
    mean_arrivals = mean(total_arrivals, na.rm = TRUE),
    q05 = quantile(total_arrivals, 0.05, na.rm = TRUE),
    q25 = quantile(total_arrivals, 0.25, na.rm = TRUE),
    q50 = quantile(total_arrivals, 0.50, na.rm = TRUE),
    q75 = quantile(total_arrivals, 0.75, na.rm = TRUE),
    q95 = quantile(total_arrivals, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

} # end of `if (run_mode == "select") else` (heavy nsim loops)

validation_actual_regional <- validation_df %>%
  group_by(date) %>%
  summarise(actual_arrivals = sum(totale_arrivi, na.rm = TRUE), .groups = "drop")

regional_actual_history <- data_fe %>%
  group_by(date) %>%
  summarise(actual_arrivals = sum(totale_arrivi, na.rm = TRUE), .groups = "drop") %>%
  filter(date >= training_start)

multilevel_validation_paths <- lapply(levels(fit_df$comune_key), function(comune_id) {
  history_values <- train_history_by_comune[[as.character(comune_id)]]

  if (length(history_values) < 12) {
    return(NULL)
  }

  forecast_multilevel_path(
    history_values = history_values,
    comune_id = comune_id,
    target_dates = validation_dates,
    weather_source = "actual"
  ) %>%
    mutate(comune_key = comune_id)
})

multilevel_validation_regional <- bind_rows(multilevel_validation_paths) %>%
  group_by(date) %>%
  summarise(
    q05 = sum(q05, na.rm = TRUE),
    q50 = sum(q50, na.rm = TRUE),
    q95 = sum(q95, na.rm = TRUE),
    mean_arrivals = q50,
    .groups = "drop"
  ) %>%
  left_join(validation_actual_regional, by = "date")

baseline_validation <- if (run_mode == "select" || nrow(validation_regional) == 0) {
  tibble()
} else {
  validation_regional %>%
    filter(scenario == "baseline") %>%
    left_join(validation_actual_regional, by = "date") %>%
    mutate(
      error_mean = mean_arrivals - actual_arrivals,
      error_q50 = q50 - actual_arrivals,
      abs_error_q50 = abs(error_q50),
      inside_90 = actual_arrivals >= q05 & actual_arrivals <= q95
    )
}

regional_train <- regional_actual_history %>%
  filter(date <= train_end) %>%
  arrange(date) %>%
  mutate(log_arrivals = log1p(actual_arrivals))

regional_train_ts <- ts(
  regional_train$log_arrivals,
  start = c(year(min(regional_train$date)), month(min(regional_train$date))),
  frequency = 12
)

arima_fit <- forecast::auto.arima(regional_train_ts, seasonal = TRUE)
nnetar_fit <- forecast::nnetar(regional_train_ts)

arima_validation_fc <- forecast::forecast(arima_fit, h = length(validation_dates), level = 90)
nnetar_validation_fc <- forecast::forecast(nnetar_fit, h = length(validation_dates), PI = TRUE, level = 90)

# -----------------------------------------------------------------------------
# Quantile Gradient Boosted Regression Tree (QGBRT) regional benchmark.
# Features: calendar month (cyclic encoding), trend index, and the last 1, 2,
# 3 and 12 lags of log_arrivals. Three separate gbm models are fit with
# distribution="quantile" at tau in {0.05, 0.50, 0.95}. Forecasts over the
# validation window are produced recursively, feeding the median prediction
# back into the lag features so that the model can run h>1 steps ahead.
# -----------------------------------------------------------------------------
qgbrt_build_features <- function(df) {
  df %>%
    arrange(date) %>%
    mutate(
      m = month(date),
      sin_m = sin(2 * pi * m / 12),
      cos_m = cos(2 * pi * m / 12),
      trend = as.numeric(date - min(date)) / 30.4375,
      lag1  = dplyr::lag(log_arrivals, 1),
      lag2  = dplyr::lag(log_arrivals, 2),
      lag3  = dplyr::lag(log_arrivals, 3),
      lag12 = dplyr::lag(log_arrivals, 12)
    )
}

qgbrt_train_df <- qgbrt_build_features(regional_train) %>%
  filter(!is.na(lag12))

qgbrt_features <- c("sin_m", "cos_m", "trend", "lag1", "lag2", "lag3", "lag12")
qgbrt_formula <- as.formula(paste("log_arrivals ~", paste(qgbrt_features, collapse = " + ")))

fit_qgbrt <- function(tau) {
  set.seed(20250101 + round(tau * 100))
  gbm::gbm(
    formula = qgbrt_formula,
    data = as.data.frame(qgbrt_train_df),
    distribution = list(name = "quantile", alpha = tau),
    n.trees = 800,
    interaction.depth = 3,
    shrinkage = 0.03,
    bag.fraction = 0.75,
    n.minobsinnode = 5,
    verbose = FALSE
  )
}

qgbrt_q05 <- fit_qgbrt(0.05)
qgbrt_q50 <- fit_qgbrt(0.50)
qgbrt_q95 <- fit_qgbrt(0.95)

# Recursive multi-step prediction over the validation window.
qgbrt_history <- regional_train %>% select(date, log_arrivals)
trend_origin <- min(regional_train$date)
qgbrt_preds <- vector("list", length(validation_dates))
for (k in seq_along(validation_dates)) {
  d <- validation_dates[k]
  recent <- qgbrt_history %>% arrange(date)
  new_row <- tibble(
    sin_m = sin(2 * pi * month(d) / 12),
    cos_m = cos(2 * pi * month(d) / 12),
    trend = as.numeric(d - trend_origin) / 30.4375,
    lag1  = tail(recent$log_arrivals, 1),
    lag2  = tail(recent$log_arrivals, 2)[1],
    lag3  = tail(recent$log_arrivals, 3)[1],
    lag12 = recent$log_arrivals[nrow(recent) - 11]
  )
  p50 <- predict(qgbrt_q50, newdata = new_row, n.trees = qgbrt_q50$n.trees)
  p05 <- predict(qgbrt_q05, newdata = new_row, n.trees = qgbrt_q05$n.trees)
  p95 <- predict(qgbrt_q95, newdata = new_row, n.trees = qgbrt_q95$n.trees)
  qgbrt_preds[[k]] <- tibble(date = d, q05 = p05, q50 = p50, q95 = p95)
  qgbrt_history <- bind_rows(qgbrt_history, tibble(date = d, log_arrivals = p50))
}
qgbrt_validation_df <- bind_rows(qgbrt_preds) %>%
  mutate(
    q05 = pmax(exp(pmin(q05, q50)) - 1, 0),
    q95 = pmax(exp(pmax(q95, q50)) - 1, 0),
    q50 = pmax(exp(q50) - 1, 0)
  )

benchmark_validation <- bind_rows(
  multilevel_validation_regional %>%
    transmute(
      model = "multilevel",
      date,
      mean_arrivals = mean_arrivals,
      q05 = q05,
      q50 = q50,
      q95 = q95,
      actual_arrivals = actual_arrivals
    ),
  tibble(
    model = "auto.arima",
    date = validation_dates,
    mean_arrivals = pmax(exp(as.numeric(arima_validation_fc$mean)) - 1, 0),
    q05 = pmax(exp(as.numeric(arima_validation_fc$lower[, 1])) - 1, 0),
    q50 = pmax(exp(as.numeric(arima_validation_fc$mean)) - 1, 0),
    q95 = pmax(exp(as.numeric(arima_validation_fc$upper[, 1])) - 1, 0),
    actual_arrivals = validation_actual_regional$actual_arrivals
  ),
  tibble(
    model = "nnetar",
    date = validation_dates,
    mean_arrivals = pmax(exp(as.numeric(nnetar_validation_fc$mean)) - 1, 0),
    q05 = pmax(exp(as.numeric(nnetar_validation_fc$lower[, 1])) - 1, 0),
    q50 = pmax(exp(as.numeric(nnetar_validation_fc$mean)) - 1, 0),
    q95 = pmax(exp(as.numeric(nnetar_validation_fc$upper[, 1])) - 1, 0),
    actual_arrivals = validation_actual_regional$actual_arrivals
  ),
  tibble(
    model = "qgbrt",
    date = validation_dates,
    mean_arrivals = qgbrt_validation_df$q50,
    q05 = qgbrt_validation_df$q05,
    q50 = qgbrt_validation_df$q50,
    q95 = qgbrt_validation_df$q95,
    actual_arrivals = validation_actual_regional$actual_arrivals
  )
) %>%
  mutate(
    error = q50 - actual_arrivals,
    abs_error = abs(error),
    sq_error = error^2
  )

benchmark_metrics <- benchmark_validation %>%
  group_by(model) %>%
  summarise(
    mae = mean(abs_error, na.rm = TRUE),
    rmse = sqrt(mean(sq_error, na.rm = TRUE)),
    bias = mean(error, na.rm = TRUE),
    mape = mean(abs_error / pmax(actual_arrivals, 1), na.rm = TRUE),
    coverage_90 = mean(actual_arrivals >= q05 & actual_arrivals <= q95, na.rm = TRUE),
    .groups = "drop"
  )

# Persist the model-selection outputs immediately so that --mode=select can
# exit here without paying the cost of the bootstrap and scenario blocks.
write.csv(benchmark_validation, file.path(output_dir, "tourism_arrivals_model_comparison_validation.csv"), row.names = FALSE)
write.csv(benchmark_metrics, file.path(output_dir, "tourism_arrivals_model_comparison_metrics.csv"), row.names = FALSE)

if (run_mode == "select") {
  cat("\nRegional model-selection metrics (validation window",
      format(validation_start), "to", format(validation_end), "):\n")
  print(benchmark_metrics)
  cat("\nSaved:\n  -",
      file.path(output_dir, "tourism_arrivals_model_comparison_metrics.csv"),
      "\n  -",
      file.path(output_dir, "tourism_arrivals_model_comparison_validation.csv"),
      "\n")
  cat("--mode=select complete. Skipping bootstrap and forward scenarios.\n")
  quit(save = "no", status = 0)
}

# =============================================================================
# Regional bootstrap (block-bootstrap residuals + nnetar) on validation +
# forward horizon in a single pass, then split.
# =============================================================================
bootstrap_series <- forecast::bld.mbb.bootstrap(regional_train_ts, num = nsim)

bootstrap_horizon_dates <- c(validation_dates, future_dates)
bootstrap_h <- length(bootstrap_horizon_dates)

bootstrap_paths_regional_all <- lapply(seq_along(bootstrap_series), function(rep_id) {
  boot_fit <- forecast::nnetar(bootstrap_series[[rep_id]])
  boot_fc <- forecast::forecast(boot_fit, h = bootstrap_h)
  tibble(
    rep = rep_id,
    date = bootstrap_horizon_dates,
    arrivals = pmax(exp(as.numeric(boot_fc$mean)) - 1, 0)
  )
}) %>%
  bind_rows()

bootstrap_paths_regional <- bootstrap_paths_regional_all %>%
  filter(date %in% validation_dates)

bootstrap_paths_regional_future <- bootstrap_paths_regional_all %>%
  filter(date %in% future_dates)

bootstrap_quantiles_regional <- bootstrap_paths_regional %>%
  group_by(date) %>%
  summarise(
    bagged_mean = mean(arrivals, na.rm = TRUE),
    q05 = quantile(arrivals, 0.05, na.rm = TRUE),
    q50 = quantile(arrivals, 0.50, na.rm = TRUE),
    q95 = quantile(arrivals, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(validation_actual_regional, by = "date")

bootstrap_quantiles_regional_future <- bootstrap_paths_regional_future %>%
  group_by(date) %>%
  summarise(
    bagged_mean = mean(arrivals, na.rm = TRUE),
    q05 = quantile(arrivals, 0.05, na.rm = TRUE),
    q50 = quantile(arrivals, 0.50, na.rm = TRUE),
    q95 = quantile(arrivals, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

# =============================================================================
# Municipal allocation: METHOD_F + meteo (production baseline).
#
# Empirical winner of the allocation-method comparison (see
# `compare_allocation_methods.R` and `report_assets/allocation_comparison_*`):
#   * Drops implausible-comuni count from 8 -> 5 on 2025 validation (-37%)
#   * Strict Pareto improvement on the implausibility set (no new comune
#     becomes implausible w.r.t. BASE)
#   * Lowest mean MAPE and lowest mean |rel_total| across 12 candidates
#
# Recipe:
#   F  : per-(comune, month) historical share built with exponentially-weighted
#        median over a 3-year window, halflife = 12 months.
#   meteo: log-space additive blend with per-comune weather-only WLS deviation
#          signal (lambda = 0.10), centered at training-period climatology.
#   For forward dates (unknown weather) we feed comune-monthly climatology, so
#   the meteo deviation is ~ 0 and the allocation reduces to pure F shares.
# =============================================================================
weighted_median <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(NA_real_)
  if (length(x) == 1) return(x)
  ord <- order(x); x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}

# ---- F shares: exp-weighted weighted-median over 3yr window -----------------
halflife_months_F <- 12
alpha_F <- 0.5 ^ (1 / halflife_months_F)
share_source_start_F <- max(training_start, last_date %m-% years(3))

shares_F_raw <- data_fe %>%
  filter(date >= share_source_start_F, date <= last_date) %>%
  group_by(date) %>%
  mutate(tot = sum(totale_arrivi, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    s = ifelse(tot > 0, totale_arrivi / tot, NA_real_),
    months_back = as.numeric(difftime(last_date, date, units = "days")) / 30.4375,
    w_F = alpha_F ^ pmax(months_back, 0)
  )

monthly_share_F <- shares_F_raw %>%
  group_by(comune_key, comune_nome, month_num) %>%
  summarise(s_bar = weighted_median(s, w_F), .groups = "drop")

monthly_share_F_fallback <- shares_F_raw %>%
  group_by(comune_key, comune_nome) %>%
  summarise(
    s_fb = weighted.mean(s, w_F, na.rm = TRUE),
    .groups = "drop"
  )

# ---- weather-only WLS per comune to produce delta signal --------------------
alpha_decay_meteo <- 0.7   # ~2-year half-life
max_train_date_meteo <- max(data_fe$date[data_fe$date <= train_end], na.rm = TRUE)

meteo_train_w <- data_fe %>%
  filter(date >= training_start, date <= train_end) %>%
  mutate(
    years_back = as.numeric(difftime(max_train_date_meteo, date, units = "days")) / 365.25,
    w = alpha_decay_meteo ^ years_back,
    log_y = log1p(totale_arrivi)
  )

fit_one_comune_weather <- function(df_c) {
  vars <- c("temperatura", "log_precipitazione", "pressione", "umidit_relativa")
  if (nrow(df_c) < 18 || sum(df_c$totale_arrivi, na.rm = TRUE) < 1) return(NULL)
  df_c2 <- df_c[, c("log_y", "w", vars)]
  df_c2 <- df_c2[stats::complete.cases(df_c2), , drop = FALSE]
  if (nrow(df_c2) < 18) return(NULL)
  if (any(vapply(df_c2[, vars], function(z) length(unique(z)) < 3, logical(1)))) return(NULL)
  fml <- as.formula(paste("log_y ~", paste(vars, collapse = " + ")))
  tryCatch(stats::lm(fml, data = df_c2, weights = df_c2$w),
           error = function(e) NULL)
}

# Build per-date weather inputs:
#  - validation_dates  -> actual weather from validation_df
#  - future_dates      -> comune-monthly climatology (delta ~ 0 after centering)
future_weather <- tibble::tibble(
  comune_key = rep(levels(fit_df$comune_key), each = length(future_dates)),
  date = rep(future_dates, times = length(levels(fit_df$comune_key)))
) %>%
  mutate(month_num = month(date)) %>%
  left_join(monthly_climatology, by = c("comune_key", "month_num")) %>%
  select(comune_key, date, temperatura, log_precipitazione, pressione, umidit_relativa)

validation_weather <- validation_df %>%
  select(comune_key, date, temperatura, log_precipitazione, pressione, umidit_relativa)

alloc_weather <- bind_rows(validation_weather, future_weather) %>%
  mutate(comune_key = as.character(comune_key))

delta_predictions <- lapply(levels(fit_df$comune_key), function(ck) {
  df_c <- meteo_train_w %>% filter(as.character(comune_key) == ck)
  mod <- fit_one_comune_weather(df_c)
  nd <- alloc_weather %>% filter(comune_key == ck) %>% arrange(date)
  if (is.null(mod) || nrow(nd) == 0) {
    return(tibble(comune_key = ck, date = nd$date, delta = 0))
  }
  train_fit <- tryCatch(predict(mod, newdata = df_c), error = function(e) NA_real_)
  center <- mean(train_fit, na.rm = TRUE)
  nd_pred <- tryCatch(predict(mod, newdata = nd), error = function(e) rep(NA_real_, nrow(nd)))
  delta <- nd_pred - center
  delta[is.na(delta)] <- 0
  tibble(comune_key = ck, date = nd$date, delta = delta)
}) %>%
  bind_rows()

# ---- assemble allocation template (F + meteo blend, lambda = 0.10) ---------
lambda_meteo <- 0.10
eps_alloc <- 1e-9
alloc_target_dates <- c(validation_dates, future_dates)

municipal_alloc_template <- expand.grid(
  date = alloc_target_dates,
  comune_key = levels(fit_df$comune_key),
  stringsAsFactors = FALSE
) %>%
  tibble::as_tibble() %>%
  mutate(month_num = month(date)) %>%
  left_join(municipality_lookup, by = "comune_key") %>%
  left_join(monthly_share_F, by = c("comune_key", "comune_nome", "month_num")) %>%
  left_join(monthly_share_F_fallback, by = c("comune_key", "comune_nome")) %>%
  mutate(s_base = dplyr::coalesce(s_bar, s_fb, 0)) %>%
  group_by(date) %>%
  mutate(s_base = if (sum(s_base, na.rm = TRUE) > 0)
    s_base / sum(s_base, na.rm = TRUE) else 0) %>%
  ungroup() %>%
  left_join(delta_predictions, by = c("comune_key", "date")) %>%
  mutate(
    delta = ifelse(is.na(delta), 0, delta),
    log_s = log(s_base + eps_alloc) + lambda_meteo * delta
  ) %>%
  group_by(date) %>%
  mutate(
    log_s = log_s - max(log_s, na.rm = TRUE),
    share = exp(log_s),
    share = if (sum(share, na.rm = TRUE) > 0)
      share / sum(share, na.rm = TRUE) else 0
  ) %>%
  ungroup() %>%
  select(date, comune_key, comune_nome, share)

cat(sprintf("Municipal allocation: METHOD_F+meteo (halflife=%dm, 3yr window, lambda_meteo=%.2f)\n",
            halflife_months_F, lambda_meteo))

# ---- apply allocation to bootstrap regional paths --------------------------
alloc_validation <- municipal_alloc_template %>% filter(date %in% validation_dates)
alloc_future     <- municipal_alloc_template %>% filter(date %in% future_dates)

bootstrap_paths_municipal <- bootstrap_paths_regional %>%
  left_join(alloc_validation, by = "date", relationship = "many-to-many") %>%
  mutate(arrivals_comune = arrivals * share)

bootstrap_quantiles_municipal <- bootstrap_paths_municipal %>%
  group_by(comune_key, comune_nome, date) %>%
  summarise(
    bagged_mean = mean(arrivals_comune, na.rm = TRUE),
    q05 = quantile(arrivals_comune, 0.05, na.rm = TRUE),
    q50 = quantile(arrivals_comune, 0.50, na.rm = TRUE),
    q95 = quantile(arrivals_comune, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

bootstrap_paths_municipal_future <- bootstrap_paths_regional_future %>%
  left_join(alloc_future, by = "date", relationship = "many-to-many") %>%
  mutate(arrivals_comune = arrivals * share)

bootstrap_quantiles_municipal_future <- bootstrap_paths_municipal_future %>%
  group_by(comune_key, comune_nome, date) %>%
  summarise(
    bagged_mean = mean(arrivals_comune, na.rm = TRUE),
    q05 = quantile(arrivals_comune, 0.05, na.rm = TRUE),
    q50 = quantile(arrivals_comune, 0.50, na.rm = TRUE),
    q95 = quantile(arrivals_comune, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

validation_actual_municipal <- validation_df %>%
  group_by(comune_key, comune_nome, date) %>%
  summarise(actual_arrivals = sum(totale_arrivi, na.rm = TRUE), .groups = "drop")

# -----------------------------------------------------------------------------
# Per-municipality forecast feasibility diagnostics.
#
# The bootstrap-municipal path takes the *regional* nnetar forecast and
# allocates it across comuni using historical monthly shares. That carries no
# municipality-specific dynamics, so comuni whose tourism profile is not well
# proxied by population / historical share (the heterogeneity issue) will be
# systematically mis-forecast even when the regional total looks fine.
#
# Here we (a) compute per-comune error metrics for both the multilevel forecast
# and the nnetar+share forecast against 2025 actuals, (b) flag comuni where
# the nnetar+share approach is implausible, and (c) build a reconciled
# forecast that uses the multilevel comune-specific dynamics rescaled so the
# comuni sum matches the validated regional nnetar total.
# -----------------------------------------------------------------------------

multilevel_validation_municipal <- bind_rows(multilevel_validation_paths) %>%
  left_join(municipality_lookup, by = "comune_key") %>%
  rename(mean_arrivals = q50) %>%
  mutate(mean_arrivals_q50 = mean_arrivals) %>%
  left_join(validation_actual_municipal, by = c("comune_key", "comune_nome", "date"))

per_comune_metrics <- function(df, model_name) {
  df %>%
    mutate(
      error = q50 - actual_arrivals,
      abs_error = abs(error),
      sq_error = error^2,
      ape = ifelse(actual_arrivals > 0, abs_error / actual_arrivals, NA_real_),
      inside_90 = actual_arrivals >= q05 & actual_arrivals <= q95
    ) %>%
    group_by(comune_key, comune_nome) %>%
    summarise(
      model = model_name,
      n_obs = sum(!is.na(actual_arrivals)),
      actual_total = sum(actual_arrivals, na.rm = TRUE),
      pred_total = sum(q50, na.rm = TRUE),
      mae = mean(abs_error, na.rm = TRUE),
      rmse = sqrt(mean(sq_error, na.rm = TRUE)),
      bias = mean(error, na.rm = TRUE),
      mape = mean(ape, na.rm = TRUE),
      coverage_90 = mean(inside_90, na.rm = TRUE),
      total_rel_error = ifelse(actual_total > 0, (pred_total - actual_total) / actual_total, NA_real_),
      .groups = "drop"
    ) %>%
    select(model, comune_key, comune_nome, n_obs, actual_total, pred_total,
           mae, rmse, bias, mape, coverage_90, total_rel_error)
}

municipal_metrics_multilevel <- per_comune_metrics(
  multilevel_validation_municipal %>%
    transmute(comune_key, comune_nome, date, q05, q50 = mean_arrivals_q50, q95, actual_arrivals),
  "multilevel"
)

municipal_metrics_share <- per_comune_metrics(
  bootstrap_quantiles_municipal %>%
    left_join(validation_actual_municipal, by = c("comune_key", "comune_nome", "date")),
  "nnetar_share"
)

# Flag comuni where the share-based forecast is implausible:
#  - |relative total error| > 50%, OR
#  - MAPE > 0.75, OR
#  - 90% coverage below 0.5
implausible_threshold_rel <- 0.5
implausible_threshold_mape <- 0.75
implausible_threshold_cov <- 0.5

municipal_feasibility <- municipal_metrics_share %>%
  mutate(
    implausible = (
      (!is.na(total_rel_error) & abs(total_rel_error) > implausible_threshold_rel) |
        (!is.na(mape) & mape > implausible_threshold_mape) |
        (!is.na(coverage_90) & coverage_90 < implausible_threshold_cov)
    )
  ) %>%
  arrange(desc(abs(total_rel_error)))

municipal_metrics_comparison <- bind_rows(
  municipal_metrics_multilevel,
  municipal_metrics_share
)

cat("Municipal feasibility check (nnetar+share):",
    sum(municipal_feasibility$implausible, na.rm = TRUE),
    "of", nrow(municipal_feasibility),
    "municipalities flagged as implausible.\n")

# -----------------------------------------------------------------------------
# Reconciled forecast: keep municipality-specific dynamics from the multilevel
# model but rescale each month so that the comuni sum equals the regional
# nnetar median (which is the headline benchmark). This is a simple bottom-up
# / top-down hybrid: shape from multilevel, level from nnetar.
# -----------------------------------------------------------------------------

multilevel_municipal_for_reconciliation <- bind_rows(multilevel_validation_paths) %>%
  left_join(municipality_lookup, by = "comune_key") %>%
  select(comune_key, comune_nome, date, q05_ml = q05, q50_ml = q50, q95_ml = q95)

regional_nnetar_target <- benchmark_validation %>%
  filter(model == "nnetar") %>%
  select(date, regional_target = q50)

reconciled_municipal <- multilevel_municipal_for_reconciliation %>%
  left_join(regional_nnetar_target, by = "date") %>%
  group_by(date) %>%
  mutate(
    ml_sum = sum(q50_ml, na.rm = TRUE),
    scale_factor = ifelse(ml_sum > 0 & !is.na(regional_target), regional_target / ml_sum, 1)
  ) %>%
  ungroup() %>%
  mutate(
    q05 = pmax(q05_ml * scale_factor, 0),
    q50 = pmax(q50_ml * scale_factor, 0),
    q95 = pmax(q95_ml * scale_factor, 0)
  ) %>%
  select(comune_key, comune_nome, date, q05, q50, q95, scale_factor)

municipal_metrics_reconciled <- per_comune_metrics(
  reconciled_municipal %>%
    left_join(validation_actual_municipal, by = c("comune_key", "comune_nome", "date")),
  "reconciled"
)

municipal_metrics_comparison <- bind_rows(
  municipal_metrics_comparison,
  municipal_metrics_reconciled
)

municipal_metrics_summary <- municipal_metrics_comparison %>%
  group_by(model) %>%
  summarise(
    n_comuni = dplyr::n(),
    median_mape = median(mape, na.rm = TRUE),
    median_abs_rel_total_error = median(abs(total_rel_error), na.rm = TRUE),
    mean_coverage_90 = mean(coverage_90, na.rm = TRUE),
    share_implausible = mean(
      (!is.na(total_rel_error) & abs(total_rel_error) > implausible_threshold_rel) |
        (!is.na(mape) & mape > implausible_threshold_mape) |
        (!is.na(coverage_90) & coverage_90 < implausible_threshold_cov),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

cat("Municipal model comparison (share of implausible comuni):\n")
print(municipal_metrics_summary)

validation_metrics <- tibble(
  metric = c("regional_mae_q50", "regional_rmse_q50", "regional_bias_q50", "coverage_90"),
  value = c(
    mean(baseline_validation$abs_error_q50, na.rm = TRUE),
    sqrt(mean((baseline_validation$error_q50)^2, na.rm = TRUE)),
    mean(baseline_validation$error_q50, na.rm = TRUE),
    mean(baseline_validation$inside_90, na.rm = TRUE)
  )
)

model_summary <- tibble(
  term = names(fixef_vec),
  estimate = as.character(unname(fixef_vec))
) %>%
  add_row(term = "sigma_eps", estimate = as.character(sigma_eps)) %>%
  add_row(term = "aic", estimate = as.character(AIC(tourism_model))) %>%
  add_row(term = "bic", estimate = as.character(BIC(tourism_model))) %>%
  add_row(term = "training_start", estimate = as.character(training_start)) %>%
  add_row(term = "bootstrap_baseline_model", estimate = "nnetar") %>%
  add_row(term = "bootstrap_replications", estimate = as.character(nsim)) %>%
  add_row(term = "train_end", estimate = as.character(train_end)) %>%
  add_row(term = "validation_start", estimate = as.character(validation_start)) %>%
  add_row(term = "validation_end", estimate = as.character(validation_end))

write.csv(model_summary, file.path(output_dir, "tourism_arrivals_model_summary.csv"), row.names = FALSE)
write.csv(quantile_summary, file.path(output_dir, "tourism_arrivals_quantile_forecast.csv"), row.names = FALSE)
write.csv(regional_summary, file.path(output_dir, "tourism_arrivals_regional_quantiles.csv"), row.names = FALSE)
write.csv(scenario_tbl, file.path(output_dir, "tourism_arrivals_scenarios.csv"), row.names = FALSE)
write.csv(validation_quantiles, file.path(output_dir, "tourism_arrivals_validation_quantiles.csv"), row.names = FALSE)
write.csv(validation_regional, file.path(output_dir, "tourism_arrivals_validation_regional_quantiles.csv"), row.names = FALSE)
write.csv(validation_metrics, file.path(output_dir, "tourism_arrivals_validation_metrics.csv"), row.names = FALSE)
write.csv(benchmark_validation, file.path(output_dir, "tourism_arrivals_model_comparison_validation.csv"), row.names = FALSE)
write.csv(benchmark_metrics, file.path(output_dir, "tourism_arrivals_model_comparison_metrics.csv"), row.names = FALSE)
write.csv(bootstrap_paths_regional, file.path(output_dir, "tourism_arrivals_bootstrap_paths_regional.csv"), row.names = FALSE)
write.csv(bootstrap_quantiles_regional, file.path(output_dir, "tourism_arrivals_bootstrap_quantiles_regional.csv"), row.names = FALSE)
write.csv(bootstrap_paths_municipal, file.path(output_dir, "tourism_arrivals_bootstrap_paths_municipal.csv"), row.names = FALSE)
write.csv(bootstrap_quantiles_municipal, file.path(output_dir, "tourism_arrivals_bootstrap_quantiles_municipal.csv"), row.names = FALSE)
write.csv(bootstrap_paths_regional_future, file.path(output_dir, "tourism_arrivals_bootstrap_paths_regional_future.csv"), row.names = FALSE)
write.csv(bootstrap_quantiles_regional_future, file.path(output_dir, "tourism_arrivals_bootstrap_quantiles_regional_future.csv"), row.names = FALSE)
write.csv(bootstrap_paths_municipal_future, file.path(output_dir, "tourism_arrivals_bootstrap_paths_municipal_future.csv"), row.names = FALSE)
write.csv(bootstrap_quantiles_municipal_future, file.path(output_dir, "tourism_arrivals_bootstrap_quantiles_municipal_future.csv"), row.names = FALSE)
write.csv(municipal_alloc_template, file.path(output_dir, "tourism_arrivals_municipal_allocation_shares.csv"), row.names = FALSE)
write.csv(municipal_metrics_comparison, file.path(output_dir, "tourism_arrivals_municipal_metrics_comparison.csv"), row.names = FALSE)
write.csv(municipal_metrics_summary, file.path(output_dir, "tourism_arrivals_municipal_metrics_summary.csv"), row.names = FALSE)
write.csv(municipal_feasibility, file.path(output_dir, "tourism_arrivals_municipal_feasibility.csv"), row.names = FALSE)
write.csv(reconciled_municipal, file.path(output_dir, "tourism_arrivals_reconciled_municipal.csv"), row.names = FALSE)

cat("CSV outputs written. Saving forecast figures.\n")

regional_plot <- benchmark_validation %>%
  ggplot(aes(x = date)) +
  annotate(
    "rect",
    xmin = min(regional_actual_history$date),
    xmax = train_end,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey92",
    alpha = 0.7
  ) +
  geom_line(
    data = regional_actual_history,
    aes(x = date, y = actual_arrivals),
    inherit.aes = FALSE,
    color = "grey35",
    linewidth = 0.9
  ) +
  geom_line(aes(y = q50, color = model), linewidth = 1) +
  geom_vline(xintercept = validation_start, linetype = "dashed", color = "black", linewidth = 0.6) +
  labs(
    title = "Tourism arrivals: train 2021-2024, test 2025",
    subtitle = "Grey line = observed history; coloured lines = 2025 forecasts from multilevel, auto.arima, and nnetar",
    x = NULL,
    y = "Regional tourist arrivals",
    color = "Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  filename = file.path(output_dir, "tourism_arrivals_regional_forecast.png"),
  plot = regional_plot,
  width = 10,
  height = 6,
  dpi = 150
)

validation_plot <- benchmark_validation %>%
  ggplot(aes(x = date, y = q50, color = model)) +
  geom_line(linewidth = 1) +
  geom_line(
    data = validation_actual_regional,
    aes(x = date, y = actual_arrivals),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 1.1,
    linetype = "solid"
  ) +
  labs(
    title = "2025 validation: model comparison",
    subtitle = "Observed regional arrivals against multilevel, auto.arima, and nnetar forecasts",
    x = NULL,
    y = "Regional tourist arrivals",
    color = "Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  filename = file.path(output_dir, "tourism_arrivals_validation_regional.png"),
  plot = validation_plot,
  width = 10,
  height = 6,
  dpi = 150
)

baseline_validation_plot <- benchmark_validation %>%
  filter(model == "multilevel") %>%
  ggplot(aes(x = date)) +
  annotate(
    "rect",
    xmin = min(regional_actual_history$date),
    xmax = train_end,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey92",
    alpha = 0.7
  ) +
  geom_line(
    data = regional_actual_history,
    aes(x = date, y = actual_arrivals),
    inherit.aes = FALSE,
    color = "grey55",
    linewidth = 0.8
  ) +
  geom_ribbon(aes(ymin = q05, ymax = q95), fill = "steelblue", alpha = 0.20, linewidth = 0) +
  geom_line(aes(y = q50), color = "steelblue4", linewidth = 1) +
  geom_line(aes(y = actual_arrivals), color = "black", linewidth = 1.1) +
  geom_vline(xintercept = validation_start, linetype = "dashed", color = "black", linewidth = 0.6) +
  labs(
    title = "Baseline forecast with train/test split",
    subtitle = "Observed history from 2021 through 2024, then multilevel median and 90% band on the 2025 validation block",
    x = NULL,
    y = "Regional tourist arrivals"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "tourism_arrivals_validation_baseline.png"),
  plot = baseline_validation_plot,
  width = 10,
  height = 6,
  dpi = 150
)

bootstrap_plot <- bootstrap_quantiles_regional %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = q05, ymax = q95), fill = "steelblue", alpha = 0.16, linewidth = 0) +
  geom_line(aes(y = bagged_mean), color = "steelblue4", linewidth = 1) +
  geom_line(aes(y = q50), color = "steelblue2", linewidth = 0.9, linetype = "dashed") +
  geom_line(
    data = validation_actual_regional,
    aes(x = date, y = actual_arrivals),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 1
  ) +
  labs(
    title = "Regional tourism bootstrap distribution on the 2025 reference year",
    subtitle = "Black line = observed 2025 arrivals; blue band = bootstrap 90% interval from bagged nnetar paths",
    x = NULL,
    y = "Regional tourist arrivals",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "tourism_arrivals_bootstrap_regional.png"),
  plot = bootstrap_plot,
  width = 10,
  height = 6,
  dpi = 150
)

bootstrap_reps <- sort(unique(bootstrap_paths_regional$rep))
bootstrap_sample_reps <- bootstrap_reps[unique(round(seq(1, length(bootstrap_reps), length.out = min(25, length(bootstrap_reps)))))]

bootstrap_paths_plot <- bootstrap_paths_regional %>%
  filter(rep %in% bootstrap_sample_reps) %>%
  ggplot(aes(x = date, y = arrivals, group = rep)) +
  geom_line(color = "grey70", alpha = 0.45, linewidth = 0.5) +
  geom_line(
    data = bootstrap_quantiles_regional,
    aes(x = date, y = q50),
    inherit.aes = FALSE,
    color = "steelblue4",
    linewidth = 1
  ) +
  geom_line(
    data = validation_actual_regional,
    aes(x = date, y = actual_arrivals),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 1
  ) +
  labs(
    title = "Regional bootstrap paths on the 2025 reference year",
    subtitle = "Grey lines = subset of bootstrap nnetar paths; blue = median path; black = observed 2025 arrivals",
    x = NULL,
    y = "Regional tourist arrivals"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "tourism_arrivals_bootstrap_paths_regional.png"),
  plot = bootstrap_paths_plot,
  width = 10,
  height = 6,
  dpi = 150
)

bootstrap_spread_plot <- bootstrap_quantiles_regional %>%
  transmute(
    date = date,
    lower_spread = q50 - q05,
    upper_spread = q95 - q50
  ) %>%
  tidyr::pivot_longer(
    cols = c(lower_spread, upper_spread),
    names_to = "band_side",
    values_to = "spread"
  ) %>%
  mutate(
    band_side = dplyr::recode(
      band_side,
      lower_spread = "Median - q05",
      upper_spread = "q95 - Median"
    )
  ) %>%
  ggplot(aes(x = date, y = spread, color = band_side)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Regional bootstrap interval asymmetry",
    subtitle = "The upper segment can exceed the lower segment because bootstrap quantiles are computed on the arrivals scale",
    x = NULL,
    y = "Distance from median",
    color = NULL
  ) +
  scale_color_manual(values = c("Median - q05" = "steelblue4", "q95 - Median" = "tomato3")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  filename = file.path(output_dir, "tourism_arrivals_bootstrap_asymmetry_regional.png"),
  plot = bootstrap_spread_plot,
  width = 10,
  height = 6,
  dpi = 150
)

top_municipalities <- bootstrap_quantiles_municipal %>%
  group_by(comune_key, comune_nome) %>%
  summarise(total_bagged = sum(bagged_mean, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_bagged)) %>%
  slice_head(n = 9)

top_municipality_levels <- top_municipalities$comune_nome

bootstrap_municipal_plot_data <- bootstrap_quantiles_municipal %>%
  semi_join(top_municipalities, by = c("comune_key", "comune_nome")) %>%
  mutate(comune_nome = factor(comune_nome, levels = top_municipality_levels))

validation_municipal_plot_data <- validation_actual_municipal %>%
  semi_join(top_municipalities, by = c("comune_key", "comune_nome")) %>%
  mutate(comune_nome = factor(comune_nome, levels = top_municipality_levels))

bootstrap_top_municipal_plot <- bootstrap_municipal_plot_data %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = q05, ymax = q95), fill = "steelblue", alpha = 0.16, linewidth = 0) +
  geom_line(aes(y = q50), color = "steelblue4", linewidth = 0.9) +
  geom_line(
    data = validation_municipal_plot_data,
    aes(x = date, y = actual_arrivals),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.8
  ) +
  facet_wrap(~ comune_nome, scales = "free_y") +
  labs(
    title = "Bootstrap municipal forecasts for the main tourism municipalities",
    subtitle = "Top 9 municipalities by total bagged 2025 arrivals; black = observed, blue = median, band = 90% interval",
    x = NULL,
    y = "Tourist arrivals"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = file.path(output_dir, "tourism_arrivals_bootstrap_top_municipalities.png"),
  plot = bootstrap_top_municipal_plot,
  width = 13,
  height = 9,
  dpi = 150
)

# -----------------------------------------------------------------------------
# Diagnostic plot: implausible municipalities under the nnetar+share approach.
# Show the observed 2025 series against the three forecasts (share, multilevel,
# reconciled) for up to 9 comuni with the largest absolute relative total
# error from the share approach. This makes visible *where* the share-based
# allocation breaks down and whether reconciliation recovers a plausible path.
# -----------------------------------------------------------------------------

flagged_comuni <- municipal_feasibility %>%
  filter(implausible) %>%
  arrange(desc(abs(total_rel_error))) %>%
  slice_head(n = 9) %>%
  select(comune_key, comune_nome)

if (nrow(flagged_comuni) == 0) {
  cat("No municipality flagged as implausible by the nnetar+share approach.\n")
} else {
  flagged_levels <- flagged_comuni$comune_nome

  share_plot_df <- bootstrap_quantiles_municipal %>%
    semi_join(flagged_comuni, by = c("comune_key", "comune_nome")) %>%
    transmute(comune_nome, date, q05, q50, q95, model = "nnetar_share")

  multilevel_plot_df <- multilevel_validation_municipal %>%
    semi_join(flagged_comuni, by = c("comune_key", "comune_nome")) %>%
    transmute(comune_nome, date, q05, q50 = mean_arrivals_q50, q95, model = "multilevel")

  reconciled_plot_df <- reconciled_municipal %>%
    semi_join(flagged_comuni, by = c("comune_key", "comune_nome")) %>%
    transmute(comune_nome, date, q05, q50, q95, model = "reconciled")

  flagged_forecasts <- bind_rows(share_plot_df, multilevel_plot_df, reconciled_plot_df) %>%
    mutate(comune_nome = factor(comune_nome, levels = flagged_levels))

  flagged_actuals <- validation_actual_municipal %>%
    semi_join(flagged_comuni, by = c("comune_key", "comune_nome")) %>%
    mutate(comune_nome = factor(comune_nome, levels = flagged_levels))

  flagged_municipal_plot <- ggplot(flagged_forecasts, aes(x = date, y = q50, color = model)) +
    geom_line(linewidth = 0.9) +
    geom_line(
      data = flagged_actuals,
      aes(x = date, y = actual_arrivals),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.9
    ) +
    facet_wrap(~ comune_nome, scales = "free_y") +
    labs(
      title = "Municipalities flagged as implausible under the nnetar+share approach",
      subtitle = "Black = observed 2025; coloured lines = forecast median by approach (share / multilevel / reconciled)",
      x = NULL,
      y = "Tourist arrivals",
      color = "Approach"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  ggsave(
    filename = file.path(output_dir, "tourism_arrivals_implausible_municipalities.png"),
    plot = flagged_municipal_plot,
    width = 13,
    height = 9,
    dpi = 150
  )
}

# -----------------------------------------------------------------------------
# Forward bootstrap plots (METHOD_F + meteo allocation): regional fan chart
# over training/validation/future, top-9 municipal forecasts on future horizon,
# and an allocation-share heatmap showing the production share matrix.
# -----------------------------------------------------------------------------

bootstrap_quantiles_regional_combined <- bind_rows(
  bootstrap_quantiles_regional %>%
    transmute(date, bagged_mean, q05, q50, q95, period = "validation"),
  bootstrap_quantiles_regional_future %>%
    transmute(date, bagged_mean, q05, q50, q95, period = "future")
)

bootstrap_forward_regional_plot <- ggplot() +
  annotate(
    "rect",
    xmin = min(regional_actual_history$date),
    xmax = train_end,
    ymin = -Inf, ymax = Inf,
    fill = "grey92", alpha = 0.7
  ) +
  geom_line(
    data = regional_actual_history,
    aes(x = date, y = actual_arrivals),
    color = "grey35", linewidth = 0.9
  ) +
  geom_ribbon(
    data = bootstrap_quantiles_regional_combined,
    aes(x = date, ymin = q05, ymax = q95, fill = period),
    alpha = 0.20, linewidth = 0
  ) +
  geom_line(
    data = bootstrap_quantiles_regional_combined,
    aes(x = date, y = q50, color = period),
    linewidth = 1
  ) +
  geom_vline(xintercept = validation_start, linetype = "dashed",
             color = "black", linewidth = 0.6) +
  geom_vline(xintercept = min(future_dates), linetype = "dashed",
             color = "black", linewidth = 0.6) +
  scale_color_manual(values = c(validation = "steelblue4", future = "tomato3")) +
  scale_fill_manual(values  = c(validation = "steelblue",  future = "tomato")) +
  labs(
    title = "Regional tourism: bootstrap fan chart (validation + forward horizon)",
    subtitle = sprintf(
      "Grey = observed history; blue band = 2025 validation 90%% interval; red band = next %d months forecast",
      length(future_dates)
    ),
    x = NULL, y = "Regional tourist arrivals",
    color = "Period", fill = "Period"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  filename = file.path(output_dir, "tourism_arrivals_bootstrap_regional_future.png"),
  plot = bootstrap_forward_regional_plot,
  width = 11, height = 6, dpi = 150
)

# Forward bootstrap municipal forecasts for top tourism comuni
top_future_municipalities <- bootstrap_quantiles_municipal_future %>%
  group_by(comune_key, comune_nome) %>%
  summarise(total_bagged = sum(bagged_mean, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_bagged)) %>%
  slice_head(n = 9)

top_future_levels <- top_future_municipalities$comune_nome

municipal_history_top <- data_fe %>%
  filter(comune_key %in% top_future_municipalities$comune_key,
         date >= (min(future_dates) %m-% years(3))) %>%
  group_by(comune_key, comune_nome, date) %>%
  summarise(actual = sum(totale_arrivi, na.rm = TRUE), .groups = "drop") %>%
  mutate(comune_nome = factor(comune_nome, levels = top_future_levels))

municipal_future_top <- bootstrap_quantiles_municipal_future %>%
  semi_join(top_future_municipalities, by = c("comune_key", "comune_nome")) %>%
  mutate(comune_nome = factor(comune_nome, levels = top_future_levels))

bootstrap_forward_municipal_plot <- ggplot() +
  geom_line(
    data = municipal_history_top,
    aes(x = date, y = actual),
    color = "grey35", linewidth = 0.6
  ) +
  geom_ribbon(
    data = municipal_future_top,
    aes(x = date, ymin = q05, ymax = q95),
    fill = "tomato", alpha = 0.20
  ) +
  geom_line(
    data = municipal_future_top,
    aes(x = date, y = q50),
    color = "tomato3", linewidth = 0.9
  ) +
  geom_vline(xintercept = min(future_dates), linetype = "dashed",
             color = "black", linewidth = 0.5) +
  facet_wrap(~ comune_nome, scales = "free_y") +
  labs(
    title = "Forward bootstrap forecasts: top tourism municipalities",
    subtitle = sprintf(
      "Allocation = METHOD_F + meteo (lambda=%.2f); grey = history (last 3y), red = bootstrap median + 90%% band over next %d months",
      lambda_meteo, length(future_dates)
    ),
    x = NULL, y = "Tourist arrivals"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = file.path(output_dir, "tourism_arrivals_bootstrap_top_municipalities_future.png"),
  plot = bootstrap_forward_municipal_plot,
  width = 13, height = 9, dpi = 150
)

# Allocation share matrix: heatmap of monthly share for top 20 comuni
top20_alloc <- municipal_alloc_template %>%
  group_by(comune_key, comune_nome) %>%
  summarise(mean_share = mean(share, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_share)) %>%
  slice_head(n = 20)

alloc_heatmap_df <- municipal_alloc_template %>%
  semi_join(top20_alloc, by = c("comune_key", "comune_nome")) %>%
  mutate(
    comune_nome = factor(comune_nome, levels = rev(top20_alloc$comune_nome)),
    period = ifelse(date %in% validation_dates, "validation", "future")
  )

alloc_heatmap_plot <- ggplot(alloc_heatmap_df,
                             aes(x = date, y = comune_nome, fill = 100 * share)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "C", name = "Share (%)") +
  geom_vline(xintercept = min(future_dates) - 15, linetype = "dashed",
             color = "white", linewidth = 0.6) +
  labs(
    title = "Municipal allocation shares: METHOD_F + meteo (top 20 comuni)",
    subtitle = sprintf(
      "Halflife=%dm, 3-yr window, lambda_meteo=%.2f. Left of dashed line = validation, right = forward horizon (climatology)",
      halflife_months_F, lambda_meteo
    ),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(output_dir, "tourism_arrivals_allocation_shares_heatmap.png"),
  plot = alloc_heatmap_plot,
  width = 11, height = 7, dpi = 150
)

cat("Tourism forecasting block completed.\n")
cat("Model sample:", nrow(fit_df), "rows across", n_distinct(fit_df$comune_key), "municipalities.\n")
cat("Forecast horizon:", horizon, "months. Simulations:", nsim, "per municipality and scenario.\n")
cat("Validation block:", as.character(validation_start), "to", as.character(validation_end), "\n")
cat("Outputs written to:", output_dir, "\n")