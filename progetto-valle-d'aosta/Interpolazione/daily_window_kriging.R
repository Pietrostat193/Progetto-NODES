# ============================================================
# Daily meteo interpolation with rolling-window hetGP::mleHomGP
# Predictors: x, y, time, altitude
# Responses: temperatura, pressione, umidit_relativa, precipitazione
# ============================================================

library(lubridate)
library(tidyverse)
library(sf)
library(terra)
library(hetGP)
library(stringi)
library(stringr)

# -----------------------------
# User parameters
# -----------------------------
work_crs <- 32632
grid_spacing_m <- 2000

vars_to_model <- c(
  "temperatura",
  "pressione",
  "umidit_relativa",
  "precipitazione"
)

covtype_gp <- "Matern5_2"

window_days <- 30              # rolling half-window: uses [date - 30, date + 30]
chunk_size <- 5000             # prediction chunk size
predict_grid <- TRUE           # if TRUE, predict on municipality grid
grid_dates_to_predict <- NULL  # NULL = all dates in full panel

# -----------------------------
# Helpers
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
  
  # No user-defined minimum n.
  # Only skip GP if the data window is truly degenerate.
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

make_points_in_polygon <- function(poly_row, cellsize) {
  raw_grid <- st_make_grid(poly_row, cellsize = cellsize, what = "centers")
  
  if (length(raw_grid) > 0) {
    pts <- st_as_sf(raw_grid)
    inside <- st_intersects(pts, poly_row, sparse = FALSE)[, 1]
    pts <- pts[inside, , drop = FALSE]
  } else {
    pts <- st_sf(geometry = st_sfc(crs = st_crs(poly_row)))
  }
  
  # Fallback: if polygon too small/narrow for the chosen spacing, create 1 point
  if (nrow(pts) == 0) {
    pts <- st_as_sf(st_point_on_surface(poly_row))
  }
  
  pts %>%
    mutate(
      municipality_key = poly_row$municipality_key,
      istat_muni_code  = poly_row$istat_muni_code,
      muni_poly_id     = poly_row$muni_poly_id
    )
}

# -----------------------------
# Read and clean data
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
  mutate(
    municipality_key = normalize_muni_key(as.character(municipality_key))
  )

vda_sf$municipality_key[
  vda_sf$municipality_key == "gressoneylatrinit"
] <- "gressoneylatrinite"

# -----------------------------
# Municipality polygons: KEEP ALL polygons in vda_sf
# -----------------------------
missing_in_map <- setdiff(
  sort(unique(data_meteo_daily$municipality_key)),
  sort(unique(vda_sf$municipality_key))
)

if (length(missing_in_map) > 0) {
  stop(
    paste(
      "These municipality keys are in data_meteo_daily but not in vda_sf:",
      paste(missing_in_map, collapse = ", ")
    )
  )
}

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

if (nrow(muni_polys) == 0) stop("muni_polys has zero rows")
if (!all(is.finite(st_bbox(muni_polys)))) stop("muni_polys has invalid bbox")

# Attach municipality info to observed station data
data_meteo_daily <- data_meteo_daily %>%
  left_join(
    muni_polys %>%
      st_drop_geometry() %>%
      select(municipality_key, istat_muni_code, muni_poly_id),
    by = "municipality_key"
  )

# -----------------------------
# Station sf
# -----------------------------
stations_sf <- data_meteo_daily %>%
  filter(!is.na(station_longitude), !is.na(station_latitude)) %>%
  st_as_sf(
    coords = c("station_longitude", "station_latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(work_crs)

# -----------------------------
# Prediction points from ALL municipalities
# -----------------------------
pred_pts_list <- lapply(seq_len(nrow(muni_polys)), function(i) {
  make_points_in_polygon(muni_polys[i, ], grid_spacing_m)
})

pred_pts_sf <- do.call(rbind, pred_pts_list) %>%
  mutate(point_id = row_number())

# -----------------------------
# DEM altitude
# -----------------------------
dem_dir <- "~/Desktop/Research/UNVDA/data/Tif Altitude"
tif_files <- list.files(dem_dir, pattern = "\\.tif$", full.names = TRUE)

if (length(tif_files) == 0) stop("No tif files found in DEM folder")

dem_vda <- tif_files %>%
  lapply(terra::rast) %>%
  (\(x) do.call(terra::merge, x))()

# Grid altitude
pred_pts_for_dem <- st_transform(pred_pts_sf, crs = terra::crs(dem_vda))
pred_pts_sf$altitude_model <- terra::extract(
  dem_vda,
  terra::vect(pred_pts_for_dem),
  ID = FALSE
)[[1]]

# Station DEM altitude fallback
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

# -----------------------------
# Plain data frames for modeling
# Expand to full daily station panel
# -----------------------------
t0 <- min(data_meteo_daily$date, na.rm = TRUE)

full_dates <- seq(
  min(data_meteo_daily$date, na.rm = TRUE),
  max(data_meteo_daily$date, na.rm = TRUE),
  by = "day"
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

station_static_cols <- c(
  "station_id",
  "station_full",
  "station_location",
  "station_latitude",
  "station_longitude",
  "municipality_key",
  "istat_muni_code",
  "muni_poly_id",
  "altitude_mslm",
  "altitude_dem",
  "altitude_model",
  "x_coord",
  "y_coord"
)

stations_static <- stations_df_obs %>%
  select(any_of(station_static_cols)) %>%
  distinct()

stations_values <- stations_df_obs %>%
  select(station_id, date, all_of(vars_to_model))

# Full panel = every station x every day in the global observed date range
stations_df <- stations_static %>%
  tidyr::crossing(date = full_dates) %>%
  left_join(stations_values, by = c("station_id", "date")) %>%
  mutate(
    time_num = as.numeric(date - t0)
  )

pred_coords <- st_coordinates(pred_pts_sf)

pred_pts_df <- pred_pts_sf %>%
  st_drop_geometry() %>%
  mutate(
    x_coord = pred_coords[, 1],
    y_coord = pred_coords[, 2]
  ) %>%
  select(
    point_id, municipality_key, istat_muni_code, muni_poly_id,
    altitude_model, x_coord, y_coord
  )

all_dates <- full_dates

if (is.null(grid_dates_to_predict)) {
  grid_dates_to_predict <- all_dates
}

# -----------------------------
# Initialize filled outputs
# -----------------------------
stations_filled_df <- stations_df

cat("predict_grid:", predict_grid, "\n")
cat("nrow(pred_pts_df):", nrow(pred_pts_df), "\n")
cat("length(grid_dates_to_predict):", length(grid_dates_to_predict), "\n")
cat("range grid dates:", as.character(min(grid_dates_to_predict)), "to", as.character(max(grid_dates_to_predict)), "\n")


for (v in vars_to_model) {
  stations_filled_df[[paste0(v, "_filled")]] <- stations_filled_df[[v]]
  stations_filled_df[[paste0(v, "_source")]] <- ifelse(
    is.finite(stations_filled_df[[v]]), "observed", NA_character_
  )
}

# -----------------------------
# Rolling-window interpolation
# -----------------------------
grid_pred_long_list <- list()
grid_pred_counter <- 1L


for (v in vars_to_model) {
  message("Processing variable: ", v)
  
  target_dates_for_var <- sort(unique(
    stations_filled_df$date[is.na(stations_filled_df[[v]])]
  ))
  
  if (predict_grid) {
    target_dates_for_var <- sort(unique(c(target_dates_for_var, grid_dates_to_predict)))
  }
  
  if (length(target_dates_for_var) == 0) next
  grid_dates_to_predict <- as.Date(grid_dates_to_predict, origin = "1970-01-01")
  target_dates_for_var <- as.Date(target_dates_for_var, origin = "1970-01-01")
  
  for (ii in seq_along(target_dates_for_var)) {
    d <- target_dates_for_var[ii]
    message("  Date: ", d)
    
    
    usable_train_n <- train_df %>%
      filter(
        is.finite(x_coord),
        is.finite(y_coord),
        is.finite(time_num),
        is.finite(altitude_model),
        is.finite(.data[[v]])
      ) %>%
      nrow()
    
    message("    usable train rows: ", usable_train_n)
    
    train_df <- stations_filled_df %>%
      filter(
        date >= (d - window_days),
        date <= (d + window_days)
      )
    
    fit_obj <- fit_local_homgp(
      train_df = train_df,
      var_name = v,
      covtype = covtype_gp,
      maxit = 80
    )
    
    message("    fit type: ", if (is.null(fit_obj)) "NULL" else fit_obj$model_type)
    
    if (is.null(fit_obj)) {
      message("    Skipped: no usable training data in window")
      gc()
      next
    }
    
    # -------------------------
    # Fill missing station values on date d
    # -------------------------
    miss_idx <- which(
      stations_filled_df$date == d &
        !is.finite(stations_filled_df[[v]]) &
        is.finite(stations_filled_df$x_coord) &
        is.finite(stations_filled_df$y_coord) &
        is.finite(stations_filled_df$time_num) &
        is.finite(stations_filled_df$altitude_model)
    )
    
    if (length(miss_idx) > 0) {
      new_df_station <- stations_filled_df[miss_idx, fit_obj$feature_cols, drop = FALSE]
      pred_station <- predict_local_homgp(fit_obj, new_df_station)
      
      stations_filled_df[[paste0(v, "_filled")]][miss_idx] <- pred_station
      stations_filled_df[[paste0(v, "_source")]][miss_idx] <- ifelse(
        fit_obj$model_type == "homgp",
        "mleHomGP_rw",
        "local_constant_rw"
      )
    }
    
    # -------------------------
    # Predict municipality grid on date d, in chunks
    # -------------------------
    grid_dates_num <- as.numeric(grid_dates_to_predict)
    d_num <- as.numeric(d)
    
    if (predict_grid && d_num %in% grid_dates_num) {
      base_day_df <- pred_pts_df %>%
        mutate(
          date = d,
          time_num = as.numeric(d - t0)
        )
      message("    grid points today: ", nrow(base_day_df))
      idx_chunks <- split(
        seq_len(nrow(base_day_df)),
        ceiling(seq_len(nrow(base_day_df)) / chunk_size)
      )
      message("    predicting grid with ", nrow(base_day_df), " points")
      
      for (ch in seq_along(idx_chunks)) {
        idx <- idx_chunks[[ch]]
        
        new_df_grid <- base_day_df[idx, c(
          "point_id", "municipality_key", "istat_muni_code", "muni_poly_id",
          "altitude_model", "x_coord", "y_coord", "date", "time_num"
        )]
        
        pred_grid <- predict_local_homgp(
          fit_obj,
          new_df_grid[, fit_obj$feature_cols, drop = FALSE]
        )
        
        message("      pred length: ", length(pred_grid))
        
        
        grid_pred_long_list[[grid_pred_counter]] <- tibble(
          point_id = new_df_grid$point_id,
          municipality_key = new_df_grid$municipality_key,
          istat_muni_code = new_df_grid$istat_muni_code,
          muni_poly_id = new_df_grid$muni_poly_id,
          date = new_df_grid$date,
          variable = v,
          pred_value = pred_grid,
          altitude_model = new_df_grid$altitude_model,
          x_coord = new_df_grid$x_coord,
          y_coord = new_df_grid$y_coord,
          pred_source = ifelse(
            fit_obj$model_type == "homgp",
            "mleHomGP_rw",
            "local_constant_rw"
          )
        )
        grid_pred_counter <- grid_pred_counter + 1L
      }
    }
    
    gc()
  }
}

# -----------------------------
# Final completed daily data
# -----------------------------
data_complete <- stations_filled_df

for (v in vars_to_model) {
  data_complete[[v]] <- data_complete[[paste0(v, "_filled")]]
}

source_cols <- paste0(vars_to_model, "_source")

data_complete <- data_complete %>%
  mutate(
    any_homgp = if_any(all_of(source_cols), ~ .x == "mleHomGP_rw"),
    any_local_constant = if_any(all_of(source_cols), ~ .x == "local_constant_rw")
  )

data_complete_light <- data_complete %>%
  select(-ends_with("_filled"))

# -----------------------------
# Spatial versions
# -----------------------------
stations_filled_sf <- data_complete_light %>%
  st_as_sf(
    coords = c("x_coord", "y_coord"),
    crs = work_crs,
    remove = FALSE
  )

pred_pts_sf <- pred_pts_sf %>%
  left_join(
    pred_pts_df %>%
      select(point_id, altitude_model),
    by = "point_id"
  )

if (predict_grid && length(grid_pred_long_list) > 0) {
  predictions_grid_long <- bind_rows(grid_pred_long_list)
  
  predictions_grid_sf <- predictions_grid_long %>%
    st_as_sf(
      coords = c("x_coord", "y_coord"),
      crs = st_crs(pred_pts_sf),
      remove = FALSE
    )
} else {
  predictions_grid_long <- NULL
  predictions_grid_sf <- NULL
}

# -----------------------------
# Useful diagnostics
# -----------------------------
cat("Number of municipality polygons:", nrow(muni_polys), "\n")
cat("Number of station rows in full panel:", nrow(stations_df), "\n")
cat("Number of grid points:", nrow(pred_pts_df), "\n")
cat("Number of dates:", length(all_dates), "\n")

for (v in vars_to_model) {
  cat(
    v,
    "- missing before:", sum(!is.finite(stations_df[[v]])),
    "- missing after:", sum(!is.finite(data_complete_light[[v]])),
    "\n"
  )
}

if (!is.null(predictions_grid_long)) {
  cat("Number of grid predictions:", nrow(predictions_grid_long), "\n")
}

# -----------------------------
# Optional saves
# -----------------------------
write.csv(
  data_complete_light,
  "~/Desktop/Research/UNVDA/data/data_complete_rolling.csv",
  row.names = FALSE
)

if (!is.null(predictions_grid_long)) {
  write.csv(
    predictions_grid_long,
    "~/Desktop/Research/UNVDA/data/predictions_grid_long_rolling.csv",
    row.names = FALSE
  )
}

saveRDS(
  stations_filled_sf,
  "~/Desktop/Research/UNVDA/data/stations_filled_sf_rolling.rds"
)

if (!is.null(predictions_grid_sf)) {
  saveRDS(
    predictions_grid_sf,
    "~/Desktop/Research/UNVDA/data/predictions_grid_sf_rolling.rds"
  )
}

saveRDS(
  pred_pts_sf,
  "~/Desktop/Research/UNVDA/data/prediction_points_sf_rolling.rds"
)

saveRDS(
  muni_polys,
  "~/Desktop/Research/UNVDA/data/municipality_polygons_rolling.rds"
)

