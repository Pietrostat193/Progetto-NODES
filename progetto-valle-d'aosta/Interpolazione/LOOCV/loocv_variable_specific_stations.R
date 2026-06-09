# ============================================================
# Variable-specific targeted LOOCV on representative months
# Gaussian Process structure consistent with main pipeline:
# predictors = x, y, time, altitude
# responses  = temperatura, pressione, umidit_relativa, precipitazione
#
# Key design choice:
# - Each environmental variable gets its OWN eligible stations
# - Each variable gets its OWN 3 representative holdout stations
# - Holdout stations are selected empirically from the available
#   nearest-neighbour distance distribution for that variable:
#       near   = least isolated available station
#       medium = station closest to the median isolation
#       far    = most isolated available station
#
# Validation design per variable:
# - choose one evaluation year (default below = 2021)
# - choose 4 representative months: Jan, Apr, Jul, Oct
# - for each selected station and month:
#     * remove the whole station from training
#     * train GP only on other stations in the same month
#     * predict the daily series at the held-out station
#
# Outputs:
#   1) station_reference_by_variable_loocv_<year>.csv
#   2) chosen_stations_by_variable_loocv_<year>.csv
#   3) loocv_<year>_daily_predictions_by_variable.csv
#   4) loocv_<year>_summary_metrics_by_variable.csv
# ============================================================

library(lubridate)
library(tidyverse)
library(sf)
library(terra)
library(hetGP)
library(stringi)
library(stringr)

# -----------------------------
# Parameters
# -----------------------------
work_crs <- 32632

vars_to_model <- c(
  "temperatura",
  "pressione",
  "umidit_relativa",
  "precipitazione"
)

covtype_gp <- "Matern5_2"
maxit_gp <- 80

eval_year <- 2021
months_to_use <- c(1, 4, 7, 10)
month_labels <- c(
  `1` = "January",
  `4` = "April",
  `7` = "July",
  `10` = "October"
)

# station selection labels are empirical, not fixed absolute distances
station_band_labels <- c("near", "medium", "far")

# minimum number of observed days required in EACH selected month,
# separately for each variable
min_obs_per_month <- 20

out_dir <- "~/Desktop/Research/UNVDA/data"

# -----------------------------
# Helpers from original pipeline
# -----------------------------
normalize_muni_key <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]", "")
}

safe_scale_train_pred <- function(X_train, X_pred) {
  center <- colMeans(X_train, na.rm = TRUE)
  scale_ <- apply(X_train, 2, sd, na.rm = TRUE)
  scale_[!is.finite(scale_) | scale_ == 0] <- 1

  X_train_sc <- sweep(X_train, 2, center, "-")
  X_train_sc <- sweep(X_train_sc, 2, scale_, "/")

  X_pred_sc <- sweep(X_pred, 2, center, "-")
  X_pred_sc <- sweep(X_pred_sc, 2, scale_, "/")

  list(
    X_train_sc = X_train_sc,
    X_pred_sc = X_pred_sc,
    center = center,
    scale = scale_
  )
}

scale_new_data <- function(X_new, center, scale_) {
  X_new_sc <- sweep(X_new, 2, center, "-")
  X_new_sc <- sweep(X_new_sc, 2, scale_, "/")
  X_new_sc
}

fit_local_homgp <- function(train_df, var_name, covtype = "Matern5_2", maxit = 80) {
  feature_cols <- c("x_coord", "y_coord", "time_num", "altitude_model")

  train_df <- train_df %>%
    select(all_of(feature_cols), all_of(var_name)) %>%
    rename(y = all_of(var_name)) %>%
    filter(if_all(all_of(c(feature_cols, "y")), ~ is.finite(.x)))

  if (nrow(train_df) == 0) return(NULL)

  X_train <- as.matrix(train_df[, feature_cols, drop = FALSE])
  y_train <- train_df$y

  response_transform <- "identity"
  if (var_name == "precipitazione") {
    y_train <- log1p(pmax(y_train, 0))
    response_transform <- "log1p"
  }

  n_unique_x <- nrow(unique(as.data.frame(X_train)))
  y_sd <- sd(y_train, na.rm = TRUE)

  if (nrow(X_train) < 2 || n_unique_x < 2 || !is.finite(y_sd) || y_sd == 0) {
    const_val <- mean(y_train, na.rm = TRUE)
    return(list(
      model_type = "constant",
      constant = const_val,
      feature_cols = feature_cols,
      center = rep(0, length(feature_cols)),
      scale = rep(1, length(feature_cols)),
      response_transform = response_transform,
      var_name = var_name
    ))
  }

  sc <- safe_scale_train_pred(X_train, X_train)

  fit <- tryCatch(
    mleHomGP(
      X = sc$X_train_sc,
      Z = y_train,
      covtype = covtype,
      maxit = maxit
    ),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    const_val <- mean(y_train, na.rm = TRUE)
    return(list(
      model_type = "constant",
      constant = const_val,
      feature_cols = feature_cols,
      center = sc$center,
      scale = sc$scale,
      response_transform = response_transform,
      var_name = var_name
    ))
  }

  list(
    model_type = "homgp",
    model = fit,
    feature_cols = feature_cols,
    center = sc$center,
    scale = sc$scale,
    response_transform = response_transform,
    var_name = var_name
  )
}

predict_local_homgp <- function(fit_obj, new_df) {
  if (is.null(fit_obj)) return(rep(NA_real_, nrow(new_df)))

  if (fit_obj$model_type == "constant") {
    pred_mean <- rep(fit_obj$constant, nrow(new_df))
  } else {
    X_new <- as.matrix(new_df[, fit_obj$feature_cols, drop = FALSE])
    X_new_sc <- scale_new_data(X_new, fit_obj$center, fit_obj$scale)
    pr <- predict(fit_obj$model, x = X_new_sc)
    pred_mean <- pr$mean
  }

  if (fit_obj$response_transform == "log1p") {
    pred_mean <- pmax(expm1(pred_mean), 0)
  }

  pred_mean
}

rmse_vec <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm = TRUE))
mae_vec  <- function(obs, pred) mean(abs(obs - pred), na.rm = TRUE)
bias_vec <- function(obs, pred) mean(pred - obs, na.rm = TRUE)

select_representative_stations <- function(station_reference, band_labels = c("near", "medium", "far")) {
  if (nrow(station_reference) < 3) {
    stop("Need at least 3 eligible stations to choose near/medium/far.")
  }

  station_reference <- station_reference %>%
    arrange(nearest_station_distance_km, station_id)

  idx_near <- 1L
  idx_far  <- nrow(station_reference)

  remaining_idx <- setdiff(seq_len(nrow(station_reference)), c(idx_near, idx_far))
  target_median <- median(station_reference$nearest_station_distance_km, na.rm = TRUE)

  if (length(remaining_idx) == 0) {
    idx_medium <- setdiff(seq_len(nrow(station_reference)), idx_near)[1]
  } else {
    idx_medium <- remaining_idx[
      which.min(abs(station_reference$nearest_station_distance_km[remaining_idx] - target_median))
    ]
  }

  chosen_idx <- c(idx_near, idx_medium, idx_far)

  chosen <- station_reference[chosen_idx, , drop = FALSE] %>%
    mutate(
      distance_band = band_labels,
      target_nearest_distance_km = c(
        min(nearest_station_distance_km, na.rm = TRUE),
        target_median,
        max(nearest_station_distance_km, na.rm = TRUE)
      ),
      selection_rule = "empirical_min_median_max"
    )

  chosen
}

# -----------------------------
# Read and prepare station data
# -----------------------------
data_meteo_daily <- read.csv(
  "~/Desktop/Research/UNVDA/data/meteo_clean_wide_all_stations.csv",
  stringsAsFactors = FALSE
)

data_meteo_daily$municipality_key <- data_meteo_daily$station_municipality
data_meteo_daily$station_municipality <- NULL
data_meteo_daily$date <- as.Date(data_meteo_daily$date)

load("~/Desktop/Research/UNVDA/data/vda_sf.RData")

data_meteo_daily$municipality_key <- normalize_muni_key(data_meteo_daily$municipality_key)
data_meteo_daily$municipality_key[
  data_meteo_daily$municipality_key == "gressoneylatrinit"
] <- "gressoneylatrinite"

vda_sf <- vda_sf %>%
  mutate(municipality_key = normalize_muni_key(as.character(municipality_key)))

vda_sf$municipality_key[
  vda_sf$municipality_key == "gressoneylatrinit"
] <- "gressoneylatrinite"

muni_polys <- vda_sf %>%
  select(municipality_key, istat_muni_code) %>%
  filter(!st_is_empty(geometry)) %>%
  st_make_valid() %>%
  st_transform(work_crs) %>%
  group_by(municipality_key, istat_muni_code) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf() %>%
  st_make_valid() %>%
  arrange(municipality_key) %>%
  mutate(muni_poly_id = row_number())

data_meteo_daily <- data_meteo_daily %>%
  left_join(
    muni_polys %>%
      st_drop_geometry() %>%
      select(municipality_key, istat_muni_code, muni_poly_id),
    by = "municipality_key"
  )

stations_sf <- data_meteo_daily %>%
  filter(!is.na(station_longitude), !is.na(station_latitude)) %>%
  st_as_sf(
    coords = c("station_longitude", "station_latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(work_crs)

# DEM altitude fallback
# If altitude_mslm is missing, use DEM extraction.
dem_dir <- "~/Desktop/Research/UNVDA/data/Tif Altitude"
tif_files <- list.files(dem_dir, pattern = "\\.tif$", full.names = TRUE)
if (length(tif_files) == 0) stop("No tif files found in DEM folder")

dem_vda <- tif_files %>%
  lapply(terra::rast) %>%
  (\(x) do.call(terra::merge, x))()

stations_for_dem <- st_transform(stations_sf, crs = terra::crs(dem_vda))
stations_sf$altitude_dem <- terra::extract(
  dem_vda,
  terra::vect(stations_for_dem),
  ID = FALSE
)[[1]]

stations_sf$altitude_model <- dplyr::coalesce(
  stations_sf$altitude_mslm,
  stations_sf$altitude_dem
)

sta_coords <- st_coordinates(stations_sf)

stations_df_obs <- stations_sf %>%
  st_drop_geometry() %>%
  mutate(
    x_coord = sta_coords[, 1],
    y_coord = sta_coords[, 2]
  )

dup_station_date <- stations_df_obs %>%
  count(station_id, date, name = "n") %>%
  filter(n > 1)

if (nrow(dup_station_date) > 0) {
  stop("Duplicate station_id-date rows found. Please aggregate or deduplicate before fitting.")
}

t0 <- min(stations_df_obs$date, na.rm = TRUE)

stations_df_obs <- stations_df_obs %>%
  mutate(
    year = year(date),
    month = month(date),
    time_num = as.numeric(date - t0)
  )

# -----------------------------
# Restrict to the evaluation design
# -----------------------------
eval_df <- stations_df_obs %>%
  filter(year == eval_year, month %in% months_to_use)

if (nrow(eval_df) == 0) {
  stop(paste0("No data found for eval_year = ", eval_year, " and chosen months."))
}

# -----------------------------
# Variable-specific station selection + LOOCV
# -----------------------------
station_reference_results <- list()
chosen_stations_results <- list()
daily_results <- list()
summary_results <- list()

res_counter_ref <- 1L
res_counter_chosen <- 1L
res_counter_daily <- 1L
res_counter_summary <- 1L

for (v in vars_to_model) {
  message("\n========================================")
  message("Variable-specific LOOCV: ", v)
  message("========================================")

  # -------------------------
  # Build variable-specific station eligibility
  # -------------------------
  station_month_counts_v <- eval_df %>%
    group_by(station_id, month) %>%
    summarise(
      n_obs = sum(is.finite(.data[[v]])),
      .groups = "drop"
    )

  eligible_stations_v <- station_month_counts_v %>%
    complete(
      station_id,
      month = months_to_use,
      fill = list(n_obs = 0)
    ) %>%
    group_by(station_id) %>%
    summarise(
      ok = all(n_obs >= min_obs_per_month),
      min_monthly_obs = min(n_obs, na.rm = TRUE),
      mean_monthly_obs = mean(n_obs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(ok)

  if (nrow(eligible_stations_v) < 3) {
    message("Skipped variable ", v, ": fewer than 3 eligible stations.")
    next
  }

  station_reference_v <- eval_df %>%
    filter(station_id %in% eligible_stations_v$station_id) %>%
    group_by(station_id) %>%
    summarise(
      station_full = first(station_full),
      station_location = first(station_location),
      municipality_key = first(municipality_key),
      x_coord = first(x_coord),
      y_coord = first(y_coord),
      altitude_model = first(altitude_model),
      n_obs_selected_months = sum(is.finite(.data[[v]])),
      .groups = "drop"
    ) %>%
    left_join(
      eligible_stations_v %>% select(station_id, min_monthly_obs, mean_monthly_obs),
      by = "station_id"
    )

  coords_mat <- as.matrix(station_reference_v[, c("x_coord", "y_coord")])
  dist_mat_m <- as.matrix(dist(coords_mat))
  diag(dist_mat_m) <- Inf

  nearest_idx <- apply(dist_mat_m, 1, which.min)

  station_reference_v <- station_reference_v %>%
    mutate(
      variable = v,
      nearest_station_id = station_reference_v$station_id[nearest_idx],
      nearest_station_full = station_reference_v$station_full[nearest_idx],
      nearest_station_distance_km = apply(dist_mat_m, 1, min, na.rm = TRUE) / 1000
    ) %>%
    arrange(nearest_station_distance_km, station_id)

  station_reference_results[[res_counter_ref]] <- station_reference_v
  res_counter_ref <- res_counter_ref + 1L

  chosen_stations_v <- select_representative_stations(
    station_reference_v,
    band_labels = station_band_labels
  ) %>%
    mutate(variable = v) %>%
    select(
      variable,
      distance_band,
      selection_rule,
      target_nearest_distance_km,
      nearest_station_distance_km,
      station_id,
      station_full,
      station_location,
      municipality_key,
      altitude_model,
      min_monthly_obs,
      mean_monthly_obs,
      n_obs_selected_months,
      nearest_station_id,
      nearest_station_full,
      x_coord,
      y_coord
    )

  chosen_stations_results[[res_counter_chosen]] <- chosen_stations_v
  res_counter_chosen <- res_counter_chosen + 1L

  message("Chosen stations for ", v, ":")
  print(chosen_stations_v %>% select(variable, distance_band, station_id, station_full, nearest_station_distance_km))

  # -------------------------
  # Targeted LOOCV for this variable only
  # -------------------------
  for (i in seq_len(nrow(chosen_stations_v))) {
    holdout_station_id <- chosen_stations_v$station_id[i]
    holdout_station_name <- chosen_stations_v$station_full[i]
    holdout_band <- chosen_stations_v$distance_band[i]
    holdout_nnd_km <- chosen_stations_v$nearest_station_distance_km[i]

    message(
      "Holdout station: ", holdout_station_name,
      " | band = ", holdout_band,
      " | nearest neighbour = ", round(holdout_nnd_km, 1), " km"
    )

    for (m in months_to_use) {
      month_name <- unname(month_labels[as.character(m)])
      message("  Month: ", month_name)

      test_df <- eval_df %>%
        filter(
          station_id == holdout_station_id,
          month == m,
          is.finite(.data[[v]]),
          is.finite(x_coord),
          is.finite(y_coord),
          is.finite(time_num),
          is.finite(altitude_model)
        )

      train_df <- eval_df %>%
        filter(
          station_id != holdout_station_id,
          month == m,
          is.finite(.data[[v]]),
          is.finite(x_coord),
          is.finite(y_coord),
          is.finite(time_num),
          is.finite(altitude_model)
        )

      if (nrow(test_df) == 0) {
        message("    skipped: no observed test rows for holdout station")
        next
      }

      if (nrow(train_df) == 0) {
        message("    skipped: no usable training rows")
        next
      }

      fit_obj <- fit_local_homgp(
        train_df = train_df,
        var_name = v,
        covtype = covtype_gp,
        maxit = maxit_gp
      )

      if (is.null(fit_obj)) {
        message("    skipped: model fit is NULL")
        next
      }

      pred_vals <- predict_local_homgp(
        fit_obj,
        test_df[, fit_obj$feature_cols, drop = FALSE]
      )

      out_daily <- test_df %>%
        transmute(
          variable = v,
          station_id = holdout_station_id,
          station_full = holdout_station_name,
          distance_band = holdout_band,
          nearest_station_distance_km = holdout_nnd_km,
          month = m,
          month_name = month_name,
          date = date,
          observed = .data[[v]],
          predicted = pred_vals,
          error = predicted - observed,
          abs_error = abs(error),
          sq_error = error^2,
          model_type = fit_obj$model_type,
          n_train_rows = nrow(train_df),
          n_test_rows = nrow(test_df)
        )

      out_summary <- out_daily %>%
        summarise(
          variable = first(variable),
          station_id = first(station_id),
          station_full = first(station_full),
          distance_band = first(distance_band),
          nearest_station_distance_km = first(nearest_station_distance_km),
          month = first(month),
          month_name = first(month_name),
          model_type = first(model_type),
          n_train_rows = first(n_train_rows),
          n_test_rows = first(n_test_rows),
          rmse = rmse_vec(observed, predicted),
          mae = mae_vec(observed, predicted),
          bias = bias_vec(observed, predicted),
          cor = ifelse(
            sum(is.finite(observed) & is.finite(predicted)) >= 2,
            cor(observed, predicted, use = "complete.obs"),
            NA_real_
          ),
          obs_mean = mean(observed, na.rm = TRUE),
          pred_mean = mean(predicted, na.rm = TRUE)
        )

      daily_results[[res_counter_daily]] <- out_daily
      summary_results[[res_counter_summary]] <- out_summary
      res_counter_daily <- res_counter_daily + 1L
      res_counter_summary <- res_counter_summary + 1L
    }
  }
}

station_reference_all <- bind_rows(station_reference_results)
chosen_stations_all <- bind_rows(chosen_stations_results)
loocv_daily <- bind_rows(daily_results)
loocv_summary <- bind_rows(summary_results) %>%
  arrange(variable, match(distance_band, station_band_labels), month)

if (nrow(loocv_summary) == 0) {
  warning("No LOOCV results were produced. Check station coverage and parameter settings.")
}

# -----------------------------
# Save outputs
# -----------------------------
write.csv(
  station_reference_all,
  file.path(out_dir, paste0("station_reference_by_variable_loocv_", eval_year, ".csv")),
  row.names = FALSE
)

write.csv(
  chosen_stations_all,
  file.path(out_dir, paste0("chosen_stations_by_variable_loocv_", eval_year, ".csv")),
  row.names = FALSE
)

write.csv(
  loocv_daily,
  file.path(out_dir, paste0("loocv_", eval_year, "_daily_predictions_by_variable.csv")),
  row.names = FALSE
)

write.csv(
  loocv_summary,
  file.path(out_dir, paste0("loocv_", eval_year, "_summary_metrics_by_variable.csv")),
  row.names = FALSE
)

# -----------------------------
# Compact console output
# -----------------------------
cat("\nVariable-specific station reference:\n")
print(station_reference_all)

cat("\nChosen stations by variable:\n")
print(chosen_stations_all)

cat("\nSummary metrics:\n")
print(loocv_summary)

cat("\nAverage RMSE by variable and distance band:\n")
print(
  loocv_summary %>%
    group_by(variable, distance_band) %>%
    summarise(mean_rmse = mean(rmse, na.rm = TRUE), .groups = "drop")
)

