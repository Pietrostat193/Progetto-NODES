# Scenarios Module

This module integrates q95 tourism-load scenario outputs with municipality tail-risk dependence metrics.

## Inputs
- ../Forecast/results/scenario_forecast/tourism_shock_forecast_town_2025.csv
- ../Tail Risk/report_assets/municipality_tail_risk_enriched.csv
- ../../data/vda_shapefile/vda_sf.RData

## Outputs (results)
- scenario_tail_impact_by_town_month.csv
- scenario_tail_impact_by_town_annual.csv
- scenario_tail_impact_regional_2025.csv
- scenario_tail_top20_weighted.csv

## Outputs (report_assets)
- scenario_tail_impact_weighted_map.png
- scenario_tail_impact_kwh_map.png
- scenario_shock_delta_kwh_map.png
- scenario_tail_top20_weighted.png

## Run
Rscript scripts/scenario_tail_risk_integration.R
