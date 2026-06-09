library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# ==========================================================
# 1. SETUP PERCORSI E CARICAMENTO SCENARI TURISTICI 2025
# ==========================================================
base_dir <- "C:/Users/2692812C/OneDrive - University of Glasgow/Desktop/Progetto-NODES-main/Progetto-NODES-main/progetto-valle-d'aosta/Modelli/Forecast"
scenari_path <- file.path(base_dir, "report_assets", "tourism_arrivals_bootstrap_quantiles_municipal.csv")

cat("Caricamento scenari turistici da:", scenari_path, "\n")
scenari_turismo <- read_csv(scenari_path) %>%
  mutate(
    date = as.Date(date),
    comune_key = tolower(trimws(as.character(comune_key)))
  ) %>%
  filter(lubridate::year(date) == 2025)

# Nota: Assicurati che nel tuo CSV la colonna con gli arrivi predetti si chiami 'q50' o 'mean_arrivals'.
# Supponiamo si chiami 'q50' (la mediana del bootstrap). Se si chiama in un altro modo, cambiala qui sotto:
scenari_turismo <- scenari_turismo %>%
  rename(arrivi_scenario = q50) %>% 
  mutate(log_arrivi_scenario = log1p(arrivi_scenario))

# ==========================================================
# 2. PREPARAZIONE DEL DATASET DI TEST (2025) PER IL CORRENTE LOAD
# ==========================================================
# Isoliamo il paniere di feature del 2025 dal tuo model_df originale
test_2025_base <- model_df %>%
  filter(year == 2025) %>%
  mutate(comune_key = tolower(trimws(as.character(comune_key))))

# ==========================================================
# 3. APPLICAZIONE DEGLI SCENARI AL MODELLO MULTILIVELLO
# ==========================================================
# Uniamo il dataset elettrico del 2025 con i diversi scenari turistici ricevuti
scenari_load <- scenari_turismo %>%
  left_join(test_2025_base, by = c("date", "comune_key")) %>%
  filter(!is.na(log_kwh)) # tiene solo i record allineati

# Sostituiamo la variabile turistica originale (log_arrivi) con quella dello specifico scenario
scenari_load <- scenari_load %>%
  mutate(log_arrivi = log_arrivi_scenario)

# Predizione del consumo elettrico sotto ogni scenario usando il tuo Mixed Model fungente (es. chiamato 'mixed_fit')
# Il modello userà i coefficienti corretti e i random effects del comune per calcolare il nuovo log_kwh
scenari_load$pred_log_kwh <- predict(mixed_fit, newdata = scenari_load)
scenari_load$pred_kwh     <- expm1(scenari_load$pred_log_kwh)

# ==========================================================
# 4. ANALISI DEGLI IMPATTI: DOVE CAMBIA DI PIÙ?
# ==========================================================
# Calcoliamo il consumo totale annuo per comune sotto ogni scenario
impatto_comunale <- scenari_load %>%
  group_by(comune_key, scenario) %>%
  summarise(Total_kWh = sum(pred_kwh, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = scenario, values_from = Total_kWh)

# Calcoliamo la variazione assoluta e percentuale rispetto allo scenario 'baseline'
# Nota: Cambia i nomi delle colonne ("high_tourism", "low_tourism") se differiscono nel tuo CSV
nomi_scenari <- setdiff(names(impatto_comunale), "comune_key")
cat("\nScenari rilevati nel file turistico:", paste(nomi_scenari, collapse = ", "), "\n")

# Classifica dei primi 10 comuni per sensibilità energetica al turismo
classifica_impatto <- impatto_comunale %>%
  mutate(
    Delta_Assoluto = `high_tourism` - `baseline`,
    Delta_Percentuale = (Delta_Assoluto / `baseline`) * 100
  ) %>%
  arrange(desc(abs(Delta_Assoluto)))

cat("\n==========================================================\n")
cat("TOP 10 COMUNI CON MAGGIORE IMPATTO SUL CONSUMO (kWh) NEL 2025")
cat("\n==========================================================\n")
print(head(classifica_impatto, 10))

# ==========================================================
# 5. PLOT DELLE TRAIETTORIE AGGREGATE GLOBALI (2025)
# ==========================================================
plot_globale_scenari <- scenari_load %>%
  group_by(date, scenario) %>%
  summarise(Global_kWh = sum(pred_kwh, na.rm = TRUE), .groups = "drop")

ggplot(plot_globale_scenari, aes(x = date, y = Global_kWh, color = scenario)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  labs(
    title = "Impatto del Turismo sul Carico Elettrico Globale (2025)",
    subtitle = "Simulazione basata sul Modello Multilivello (Mixed Model)",
    x = "Data",
    y = "Consumo Totale Regione (kWh)",
    color = "Scenario Turistico"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

# Salva i risultati dell'impatto in formato CSV
write_csv(classifica_impatto, file.path(base_dir, "Municipality_Plots", "impatto_scenari_turismo_2025.csv"))