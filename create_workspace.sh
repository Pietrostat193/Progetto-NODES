#!/usr/bin/env bash
set -euo pipefail

# Workspace root (di default: cartella corrente)
ROOT_DIR="${1:-.}"

# Funzione: crea cartelle in modo idempotente
mk() { mkdir -p "$ROOT_DIR/$1"; }

# ============================================================
# PROGETTO 1 — Valle d’Aosta (software a scelta)
# ============================================================
mk "progetto_1_valle_daosta"
mk "progetto_1_valle_daosta/0_admin"
mk "progetto_1_valle_daosta/0_admin/meeting_notes"
mk "progetto_1_valle_daosta/0_admin/timeline"
mk "progetto_1_valle_daosta/0_admin/requirements"

mk "progetto_1_valle_daosta/1_data"
mk "progetto_1_valle_daosta/1_data/0_raw"
mk "progetto_1_valle_daosta/1_data/1_external"
mk "progetto_1_valle_daosta/1_data/2_interim"
mk "progetto_1_valle_daosta/1_data/3_processed"
mk "progetto_1_valle_daosta/1_data/4_features"
mk "progetto_1_valle_daosta/1_data/5_quality_checks"

mk "progetto_1_valle_daosta/2_notebooks"
mk "progetto_1_valle_daosta/2_notebooks/step_a_ambientale"
mk "progetto_1_valle_daosta/2_notebooks/step_b_consumo_base"
mk "progetto_1_valle_daosta/2_notebooks/step_c_turismo"
mk "progetto_1_valle_daosta/2_notebooks/exploration"
mk "progetto_1_valle_daosta/2_notebooks/figures_quicklook"

mk "progetto_1_valle_daosta/3_src"
mk "progetto_1_valle_daosta/3_src/common"
mk "progetto_1_valle_daosta/3_src/common/config"
mk "progetto_1_valle_daosta/3_src/common/io"
mk "progetto_1_valle_daosta/3_src/common/geo"
mk "progetto_1_valle_daosta/3_src/common/features"
mk "progetto_1_valle_daosta/3_src/common/modeling"
mk "progetto_1_valle_daosta/3_src/common/evaluation"
mk "progetto_1_valle_daosta/3_src/common/visualization"
mk "progetto_1_valle_daosta/3_src/common/utils"

mk "progetto_1_valle_daosta/3_src/step_a_preparazione_ambientale"
mk "progetto_1_valle_daosta/3_src/step_a_preparazione_ambientale/ingestion"
mk "progetto_1_valle_daosta/3_src/step_a_preparazione_ambientale/cleaning"
mk "progetto_1_valle_daosta/3_src/step_a_preparazione_ambientale/alignment"
mk "progetto_1_valle_daosta/3_src/step_a_preparazione_ambientale/feature_engineering"

mk "progetto_1_valle_daosta/3_src/step_b_modellazione_consumo"
mk "progetto_1_valle_daosta/3_src/step_b_modellazione_consumo/baselines"
mk "progetto_1_valle_daosta/3_src/step_b_modellazione_consumo/ml_models"
mk "progetto_1_valle_daosta/3_src/step_b_modellazione_consumo/per_utenza"
mk "progetto_1_valle_daosta/3_src/step_b_modellazione_consumo/validation"

mk "progetto_1_valle_daosta/3_src/step_c_integrazione_turismo"
mk "progetto_1_valle_daosta/3_src/step_c_integrazione_turismo/joint_models"
mk "progetto_1_valle_daosta/3_src/step_c_integrazione_turismo/marginal_impact"
mk "progetto_1_valle_daosta/3_src/step_c_integrazione_turismo/nonlinear_seasonal"
mk "progetto_1_valle_daosta/3_src/step_c_integrazione_turismo/asymmetry"

mk "progetto_1_valle_daosta/4_models"
mk "progetto_1_valle_daosta/4_models/checkpoints"
mk "progetto_1_valle_daosta/4_models/production_candidates"
mk "progetto_1_valle_daosta/4_models/metadata"

mk "progetto_1_valle_daosta/5_reports"
mk "progetto_1_valle_daosta/5_reports/figures"
mk "progetto_1_valle_daosta/5_reports/tables"
mk "progetto_1_valle_daosta/5_reports/drafts"
mk "progetto_1_valle_daosta/5_reports/paper"
mk "progetto_1_valle_daosta/5_reports/presentations"

mk "progetto_1_valle_daosta/6_experiments"
mk "progetto_1_valle_daosta/6_experiments/tracking"
mk "progetto_1_valle_daosta/6_experiments/logs"

mk "progetto_1_valle_daosta/7_tests"
mk "progetto_1_valle_daosta/7_tests/unit"
mk "progetto_1_valle_daosta/7_tests/integration"
mk "progetto_1_valle_daosta/7_tests/data_validation"

mk "progetto_1_valle_daosta/8_docs"
mk "progetto_1_valle_daosta/8_docs/data_dictionary"
mk "progetto_1_valle_daosta/8_docs/methodology"
mk "progetto_1_valle_daosta/8_docs/reproducibility"
mk "progetto_1_valle_daosta/8_docs/references"

# ============================================================
# PROGETTO 2 — Italia (Python) — 7 bidding zone
# ============================================================
mk "progetto_2_italia_bidding_zones"
mk "progetto_2_italia_bidding_zones/0_admin"
mk "progetto_2_italia_bidding_zones/0_admin/meeting_notes"
mk "progetto_2_italia_bidding_zones/0_admin/timeline"
mk "progetto_2_italia_bidding_zones/0_admin/requirements"

mk "progetto_2_italia_bidding_zones/1_data"
mk "progetto_2_italia_bidding_zones/1_data/0_raw"
mk "progetto_2_italia_bidding_zones/1_data/1_external"
mk "progetto_2_italia_bidding_zones/1_data/2_interim"
mk "progetto_2_italia_bidding_zones/1_data/3_processed"
mk "progetto_2_italia_bidding_zones/1_data/4_features"
mk "progetto_2_italia_bidding_zones/1_data/5_quality_checks"

mk "progetto_2_italia_bidding_zones/2_notebooks"
mk "progetto_2_italia_bidding_zones/2_notebooks/fase_1_preparazione_dati"
mk "progetto_2_italia_bidding_zones/2_notebooks/fase_2_previsione_load"
mk "progetto_2_italia_bidding_zones/2_notebooks/fase_3_codipendenze"
mk "progetto_2_italia_bidding_zones/2_notebooks/fase_4_valutazione_finanziaria"
mk "progetto_2_italia_bidding_zones/2_notebooks/exploration"
mk "progetto_2_italia_bidding_zones/2_notebooks/figures_quicklook"

mk "progetto_2_italia_bidding_zones/3_src"
mk "progetto_2_italia_bidding_zones/3_src/common"
mk "progetto_2_italia_bidding_zones/3_src/common/config"
mk "progetto_2_italia_bidding_zones/3_src/common/io"
mk "progetto_2_italia_bidding_zones/3_src/common/features"
mk "progetto_2_italia_bidding_zones/3_src/common/modeling"
mk "progetto_2_italia_bidding_zones/3_src/common/evaluation"
mk "progetto_2_italia_bidding_zones/3_src/common/visualization"
mk "progetto_2_italia_bidding_zones/3_src/common/utils"

mk "progetto_2_italia_bidding_zones/3_src/fase_1_preparazione_dati"
mk "progetto_2_italia_bidding_zones/3_src/fase_1_preparazione_dati/calendar_features"
mk "progetto_2_italia_bidding_zones/3_src/fase_1_preparazione_dati/cleaning"
mk "progetto_2_italia_bidding_zones/3_src/fase_1_preparazione_dati/validation"
mk "progetto_2_italia_bidding_zones/3_src/fase_1_preparazione_dati/feature_store"

mk "progetto_2_italia_bidding_zones/3_src/fase_2_previsione_load"
mk "progetto_2_italia_bidding_zones/3_src/fase_2_previsione_load/baselines"
mk "progetto_2_italia_bidding_zones/3_src/fase_2_previsione_load/transformers"
mk "progetto_2_italia_bidding_zones/3_src/fase_2_previsione_load/statistical_models"
mk "progetto_2_italia_bidding_zones/3_src/fase_2_previsione_load/probabilistic_forecasts"
mk "progetto_2_italia_bidding_zones/3_src/fase_2_previsione_load/backtesting"
mk "progetto_2_italia_bidding_zones/3_src/fase_2_previsione_load/terna_benchmark"

mk "progetto_2_italia_bidding_zones/3_src/fase_3_codipendenze"
mk "progetto_2_italia_bidding_zones/3_src/fase_3_codipendenze/residuals"
mk "progetto_2_italia_bidding_zones/3_src/fase_3_codipendenze/mean_dependence"
mk "progetto_2_italia_bidding_zones/3_src/fase_3_codipendenze/regime_dependence"
mk "progetto_2_italia_bidding_zones/3_src/fase_3_codipendenze/tail_dependence"
mk "progetto_2_italia_bidding_zones/3_src/fase_3_codipendenze/asymmetric_dependence"
mk "progetto_2_italia_bidding_zones/3_src/fase_3_codipendenze/benchmarks"

mk "progetto_2_italia_bidding_zones/3_src/fase_4_valutazione_finanziaria"
mk "progetto_2_italia_bidding_zones/3_src/fase_4_valutazione_finanziaria/strategies"
mk "progetto_2_italia_bidding_zones/3_src/fase_4_valutazione_finanziaria/risk_metrics"
mk "progetto_2_italia_bidding_zones/3_src/fase_4_valutazione_finanziaria/pnl"
mk "progetto_2_italia_bidding_zones/3_src/fase_4_valutazione_finanziaria/reporting"

mk "progetto_2_italia_bidding_zones/4_models"
mk "progetto_2_italia_bidding_zones/4_models/checkpoints"
mk "progetto_2_italia_bidding_zones/4_models/production_candidates"
mk "progetto_2_italia_bidding_zones/4_models/metadata"

mk "progetto_2_italia_bidding_zones/5_reports"
mk "progetto_2_italia_bidding_zones/5_reports/figures"
mk "progetto_2_italia_bidding_zones/5_reports/tables"
mk "progetto_2_italia_bidding_zones/5_reports/drafts"
mk "progetto_2_italia_bidding_zones/5_reports/paper"
mk "progetto_2_italia_bidding_zones/5_reports/presentations"

mk "progetto_2_italia_bidding_zones/6_experiments"
mk "progetto_2_italia_bidding_zones/6_experiments/tracking"
mk "progetto_2_italia_bidding_zones/6_experiments/logs"

mk "progetto_2_italia_bidding_zones/7_tests"
mk "progetto_2_italia_bidding_zones/7_tests/unit"
mk "progetto_2_italia_bidding_zones/7_tests/integration"
mk "progetto_2_italia_bidding_zones/7_tests/data_validation"

mk "progetto_2_italia_bidding_zones/8_docs"
mk "progetto_2_italia_bidding_zones/8_docs/data_dictionary"
mk "progetto_2_italia_bidding_zones/8_docs/methodology"
mk "progetto_2_italia_bidding_zones/8_docs/reproducibility"
mk "progetto_2_italia_bidding_zones/8_docs/references"

echo "✅ Cartelle create sotto: $ROOT_DIR"
