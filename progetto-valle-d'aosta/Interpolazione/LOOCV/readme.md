# Variable-specific LOOCV for meteo interpolation

## Overview

This folder contains two R scripts used to evaluate a Gaussian Process interpolation pipeline for daily environmental variables in Valle d'Aosta.

The validation design is a **targeted leave-one-out cross-validation (LOOCV)** run on a single year and four representative months. The key methodological choice is that **each variable has its own eligible stations and its own selected holdout stations**.

The scripts are:

- `loocv_variable_specific_stations.R`
- `plotting_loocv.R`

## Goal

The purpose of the LOOCV is to evaluate how prediction quality changes when one station is entirely removed from the training set, focusing on:

- **seasonality**: January, April, July, October
- **station isolation**: stations chosen as empirical `near`, `medium`, and `far`
- **variable-specific behaviour**: temperature, pressure, relative humidity, precipitation

This is not a full LOOCV over all stations. Instead, it is a **targeted validation** meant to compare representative stations with different nearest-neighbour distances.

## Main script: `loocv_variable_specific_stations.R`

### What it does

For each environmental variable separately:

1. Loads and cleans daily station data.
2. Harmonises municipality identifiers.
3. Builds station coordinates in projected CRS (`EPSG:32632`).
4. Fills missing station altitude with DEM extraction when needed.
5. Restricts the analysis to one evaluation year and four selected months.
6. Identifies **eligible stations for that variable only**.
7. Computes the station distance matrix.
8. Selects three representative stations:
   - `near`: least isolated station
   - `medium`: station closest to the median nearest-neighbour distance
   - `far`: most isolated station
9. Repeats the following for each selected station and month:
   - remove the whole station from training
   - fit a Gaussian Process on the remaining stations for that month
   - predict the daily series at the held-out station
10. Saves daily predictions and summary performance metrics.

### Model structure

The Gaussian Process uses:

- predictors: `x_coord`, `y_coord`, `time_num`, `altitude_model`
- covariance type: `Matern5_2`
- fitter: `hetGP::mleHomGP`

Special handling:

- `precipitazione` is modelled on `log1p(y)` and back-transformed after prediction.
- if a GP fit is not feasible, the script falls back to a local constant predictor.

### Key parameters to edit

Inside the script you can change:

- `eval_year`: evaluation year
- `months_to_use`: selected months
- `min_obs_per_month`: minimum observed days required in each selected month
- `covtype_gp`: covariance kernel
- `maxit_gp`: GP optimisation iterations

### Output files

The script writes four CSV files:

- `station_reference_by_variable_loocv_<year>.csv`  
  All eligible stations for each variable, with nearest-neighbour information.

- `chosen_stations_by_variable_loocv_<year>.csv`  
  The three selected stations per variable (`near`, `medium`, `far`).

- `loocv_<year>_daily_predictions_by_variable.csv`  
  Daily observed and predicted values for each validation scenario.

- `loocv_<year>_summary_metrics_by_variable.csv`  
  Aggregated metrics by variable, station, and month.

## Plot script: `plotting_loocv.R`

### What it does

This script visualises the selected LOOCV stations on a 2x2 grid, one panel for each variable.

- background: Valle d'Aosta outline
- points: selected `near`, `medium`, `far` stations
- labels: station location names

The script reads:

- `chosen_stations_by_variable_loocv_<year>.csv`
- `station_reference_by_variable_loocv_<year>.csv`

and saves:

- `loocv_station_locations_by_variable_<year>.png`

## Recommended workflow

1. Run `loocv_variable_specific_stations.R`.
2. Inspect:
   - chosen stations by variable
   - nearest-neighbour distances
   - summary metrics (`rmse`, `mae`, `bias`, `cor`)
3. Run `plotting_loocv.R` to verify the spatial configuration.
4. Produce additional diagnostic plots from the saved CSV outputs.

## Interpretation notes

- `near`, `medium`, and `far` are **empirical labels** based on the available station network for each variable. They are not fixed absolute distance classes.
- RMSE alone can be misleading. For variables such as pressure, a large RMSE may be driven mostly by **systematic bias** rather than poor temporal tracking.
- For precipitation, high RMSE may reflect the intermittency and locality of rainfall events.
- For pressure, strong altitude dependence may require a baseline correction or residual-based modelling.

## Suggested checks after running

Useful tables to inspect:

- mean RMSE by variable and distance band
- mean MAE by variable and distance band
- bias by variable and station
- correlation by variable and station
- observed mean vs predicted mean

A particularly important diagnostic is to compare:

- `RMSE`
- `MAE`
- `bias`
- `cor`
- `obs_mean`
- `pred_mean`

If `RMSE` is large and `|bias|` is also large, while `cor` remains high, then the model is tracking the temporal pattern but missing the station-specific level.

## Minimal run commands

```r
source("~/Desktop/Research/UNVDA/data/loocv_variable_specific_stations.R")
source("~/Desktop/Research/UNVDA/data/plotting_loocv.R")
```

## Package dependencies

The scripts use:

- `tidyverse`
- `lubridate`
- `sf`
- `terra`
- `hetGP`
- `stringi`
- `stringr`
- `ggrepel`

## Possible extensions

- Add boxplots of absolute error by variable and month.
- Plot RMSE and MAE by month to compare seasonal behaviour.
- Plot bias versus altitude to diagnose pressure issues.
- Add observed vs predicted scatterplots by variable.
- Add time-series panels for held-out stations.
