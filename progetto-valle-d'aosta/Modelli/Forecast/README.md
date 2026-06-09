# Forecast

Cartella dedicata al workflow di forecasting turismo -> energia.

## Struttura aggiornata

- `scripts/`: script operativi e di diagnostica.
- `results/intermediate/model_selection/`: ranking e assegnazioni per comune.
- `results/intermediate/anomalies/`: comuni esclusi o anomali.
- `results/intermediate/assignment_eval/`: performance del rerun con assegnazione modello-per-comune.
- `results/scenario_forecast/`: output finali di scenario (tourism shock).
- `archive/municipality_plots_legacy_20260604/`: storico completo di vecchi plot e output.
- `report_assets/`: output del blocco forecast turismo originario.

## Script principali (nuovo workflow)

- `scripts/rerun_models_with_assignment.R`
  - rifitta Mixed, GAMM, Prophet;
  - applica l'assegnazione modello-per-comune (`town_model_assignment_mape_le20_2025.csv`);
  - produce confronto `Assigned_Strategy` vs modelli singoli.

- `scripts/tourism_shock_scenario_forecast.R`
  - usa le previsioni baseline assegnate per comune;
  - stima elasticita' turismo->energia per comune (con fallback globale);
  - genera scenari shock turismo (down_15, baseline, up_15, stress_up_30);
  - salva forecast comunali e regionali di scenario.

- `scripts/format_municipality_performance_table.R`
  - converte la tabella performance in formato leggibile (modelli in colonna, metriche in riga).

## Dove trovare i file chiave

### Model selection

- `results/intermediate/model_selection/municipality_model_performance_2025.csv`
- `results/intermediate/model_selection/municipality_model_performance_2025_readable.csv`
- `results/intermediate/model_selection/municipality_mape_ranking_wide_2025.csv`
- `results/intermediate/model_selection/town_model_assignment_mape_le20_2025.csv`
- `results/intermediate/model_selection/town_assignment_summary_mape_threshold_2025.csv`

### Anomalie

- `results/intermediate/anomalies/towns_anomalous_mape_gt20_2025.csv`

### Valutazione strategia assegnata

- `results/intermediate/assignment_eval/assignment_strategy_vs_single_models_2025.csv`
- `results/intermediate/assignment_eval/assignment_strategy_town_metrics_2025.csv`
- `results/intermediate/assignment_eval/assignment_strategy_model_counts_2025.csv`
- `results/intermediate/assignment_eval/assignment_strategy_predictions_2025.csv`

### Scenario forecast (tourism shock)

- `results/scenario_forecast/tourism_shock_scenarios_def.csv`
- `results/scenario_forecast/tourism_shock_elasticity_by_town.csv`
- `results/scenario_forecast/tourism_shock_forecast_town_2025.csv`
- `results/scenario_forecast/tourism_shock_forecast_regional_2025.csv`
- `results/scenario_forecast/tourism_shock_anomalous_excluded_towns.csv` (se disponibile)

## Esecuzione rapida

Dalla root del repository `progetto-valle-d'aosta`:

```r
Rscript "Modelli/Forecast/scripts/rerun_models_with_assignment.R"
Rscript "Modelli/Forecast/scripts/tourism_shock_scenario_forecast.R"
```

## Note

- I vecchi output non sono stati eliminati: sono archiviati in `archive/municipality_plots_legacy_20260604/`.
- Il filtro anomalie corrente e': `Best_MAPE_pct > 20`.
