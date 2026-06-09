# Indice dei risultati salvati

Questo file riassume i risultati ottenuti fin qui nel blocco mean effect, indica dove sono documentati e specifica quali script e quali file contengono le statistiche salvate.

## Struttura logica del lavoro

Il materiale disponibile in questa cartella copre il blocco:

[Introduzione] -> [Modelli M1-M4] -> [Copule (Residui)] -> [Forecasting]

Al momento, i risultati effettivamente prodotti riguardano soprattutto il blocco mean effect:

- sequenza dei modelli M0-M4 con popolazione between/within;
- contributo degli arrivi turistici nel modello finale;
- confronto tra arrivi osservati e arrivi residualizzati rispetto a struttura, meteo e calendario;
- test di interazione arrivi x temperatura;
- test di eterogeneita' tra comuni turistici e non turistici;
- lettura territoriale con mappa comunale.

## Report principali

### 1. Report storico sui modelli con popolazione

File:

- [README_report_modelli_popolazione.md](README_report_modelli_popolazione.md)

Contenuto:

- descrizione della sequenza M0-M4;
- interpretazione della decomposizione tra `log_residenti_mean_z` e `log_residenti_dev_z`;
- ruolo di meteo, calendario, dinamica e arrivi turistici;
- confronto annidato tra modelli;
- identificazione degli arrivi rispetto a stagionalita' e meteo.

Statistiche principali richiamate in questo report:

- AIC, BIC, logLik, $R^2$ marginale, $R^2$ condizionale;
- coefficienti, standard error, t-stat e p-value dei modelli;
- test annidati M0 -> M4;
- confronto `m3`, `m4_arrivi`, `m4_arrivi_destag`.

### 2. Report aggiornato mean effect

File:

- [README_mean_effect.md](README_mean_effect.md)

Contenuto:

- confronto tra `M3_base`, `M4_arrivi_turistici`, `M4_arrivi_x_temperatura`, `M4_arrivi_eterogeneo`;
- effetto medio degli arrivi turistici;
- interazione arrivi x temperatura;
- eterogeneita' tra comuni turistici e non turistici;
- mappa dei comuni e lettura territoriale;
- coefficienti chiave dei nuovi modelli.

Statistiche principali richiamate in questo report:

- confronto AIC/BIC dei modelli mean effect;
- coefficienti chiave con p-value;
- effetto degli arrivi nei comuni turistici e non turistici;
- soglia usata per classificare i comuni turistici;
- top 10 comuni per arrivi per residente.

### 3. Report storico precedente

File:

- [README_report_modelli.md](README_report_modelli.md)

Contenuto:

- versione precedente dell'analisi M0-M4;
- utile solo come archivio storico;
- superseduto dai due report sopra.

## Script che generano i risultati

### 1. Script principale del blocco mean effect

File:

- [modelling.R](modelling.R)

Produce:

- dataset di analisi comune ai modelli;
- modelli `m0`, `m0_pop`, `m1`, `m2`, `m3`, `m4`;
- modello con arrivi residualizzati `m4_arrivi_destag`;
- tabelle `comparison_table` e `fixed_effects_table`;
- random intercept del modello finale.

Risultati sostantivi ottenuti qui:

- la popolazione between/within spiega parte importante dell'eterogeneita' comunale;
- la dinamica del consumo domina il fit;
- gli arrivi turistici migliorano M4 oltre M3;
- il contributo degli arrivi sopravvive alla residualizzazione rispetto a struttura, meteo e calendario.

### 2. Generazione degli asset standard del report M0-M4

File:

- [generate_report_assets.R](generate_report_assets.R)

Scrive in [report_assets](report_assets):

- [comparison_table.csv](report_assets/comparison_table.csv)
- [fixed_effects_table.csv](report_assets/fixed_effects_table.csv)
- [nested_anova.csv](report_assets/nested_anova.csv)
- [kwh_quality.csv](report_assets/kwh_quality.csv)
- [arrivi_compare.csv](report_assets/arrivi_compare.csv)
- [arrivi_identification.csv](report_assets/arrivi_identification.csv)
- [coef_m0_popolazione.png](report_assets/coef_m0_popolazione.png)
- [coef_m1_meteo.png](report_assets/coef_m1_meteo.png)
- [coef_m2_calendario.png](report_assets/coef_m2_calendario.png)
- [coef_m3_dinamica.png](report_assets/coef_m3_dinamica.png)
- [coef_m4_arrivi_turistici.png](report_assets/coef_m4_arrivi_turistici.png)
- [model_comparison_metrics.png](report_assets/model_comparison_metrics.png)
- [arrivi_model_compare.png](report_assets/arrivi_model_compare.png)
- [final_model_residuals.png](report_assets/final_model_residuals.png)

### 3. Analisi dell'overlap tra arrivi, stagione e dinamica

File:

- [analyze_tourism_overlap.R](analyze_tourism_overlap.R)
- [extract_tourism_overlap_summary.R](extract_tourism_overlap_summary.R)

Scrivono in [report_assets](report_assets):

- [arrivi_overlap_comparison.csv](report_assets/arrivi_overlap_comparison.csv)
- [arrivi_overlap_summary.csv](report_assets/arrivi_overlap_summary.csv)

Risultati sostantivi ottenuti qui:

- gli arrivi spiegano molto il consumo gia' in M2;
- la parte degli arrivi non spiegata da struttura, meteo e calendario continua a spiegare consumo;
- la sovrapposizione piu' forte degli arrivi non e' con la pura stagionalita', ma con i lag del consumo;
- i lag possono assorbire parte del segnale turistico, ma non tutto.

### 4. Report esteso sul mean effect con mappa

File:

- [generate_mean_effect_report.R](generate_mean_effect_report.R)

Scrive in [report_assets](report_assets):

- [mean_effect_model_compare.csv](report_assets/mean_effect_model_compare.csv)
- [mean_effect_fixed_effects.csv](report_assets/mean_effect_fixed_effects.csv)
- [mean_effect_key_effects.csv](report_assets/mean_effect_key_effects.csv)
- [mean_effect_group_slopes.csv](report_assets/mean_effect_group_slopes.csv)
- [municipality_profile.csv](report_assets/municipality_profile.csv)
- [mean_effect_model_compare.png](report_assets/mean_effect_model_compare.png)
- [mean_effect_key_coefficients.png](report_assets/mean_effect_key_coefficients.png)
- [municipality_maps.png](report_assets/municipality_maps.png)
- [README_mean_effect.md](README_mean_effect.md)

Risultati sostantivi ottenuti qui:

- l'interazione `arrivi x temperatura` non e' statisticamente robusta;
- l'effetto degli arrivi e' eterogeneo nello spazio;
- i comuni classificati come turistici mostrano una pendenza piu' alta degli arrivi;
- la geografia dei comuni turistici e dei random intercept residui e' visualizzata nella mappa.

## Dove trovare ogni statistica salvata

### Confronto generale tra modelli M0-M4

File:

- [comparison_table.csv](report_assets/comparison_table.csv)
- [model_comparison_metrics.png](report_assets/model_comparison_metrics.png)

Statistiche salvate:

- `model`
- `engine`
- `nobs`
- `AIC`
- `BIC`
- `logLik`
- `R2_marginal`
- `R2_conditional`
- `singular`

### Coefficienti completi dei modelli standard

File:

- [fixed_effects_table.csv](report_assets/fixed_effects_table.csv)

Statistiche salvate:

- `model`
- `engine`
- `term`
- `estimate`
- `std.error`
- `df`
- `statistic`
- `p.value`

### Test annidati tra i modelli standard

File:

- [nested_anova.csv](report_assets/nested_anova.csv)

Statistiche salvate:

- `model`
- `npar`
- `AIC`
- `BIC`
- `logLik`
- `-2*log(L)`
- `Chisq`
- `Df`
- `Pr(>Chisq)`

### Qualita' del dato kWh

File:

- [kwh_quality.csv](report_assets/kwh_quality.csv)

Statistiche salvate:

- numero osservazioni;
- missing `kwh`;
- `kwh` negativi;
- `kwh` negativi trattati come `NA`;
- `kwh` uguali a zero.

### Contributo degli arrivi rispetto a M3

File:

- [arrivi_compare.csv](report_assets/arrivi_compare.csv)
- [arrivi_model_compare.png](report_assets/arrivi_model_compare.png)

Statistiche salvate:

- confronto `m3`, `m4`, `m4_arrivi_destag`;
- `AIC`, `BIC`, `logLik`, `Chisq`, `Df`, `Pr(>Chisq)`.

### Identificazione degli arrivi osservati vs residualizzati

File:

- [arrivi_identification.csv](report_assets/arrivi_identification.csv)
- [arrivi_overlap_summary.csv](report_assets/arrivi_overlap_summary.csv)
- [arrivi_overlap_comparison.csv](report_assets/arrivi_overlap_comparison.csv)

Statistiche salvate:

- coefficiente degli arrivi osservati in M4;
- coefficiente degli arrivi residualizzati su struttura + meteo + calendario;
- AIC dei modelli con e senza arrivi residualizzati;
- correlazioni tra arrivi, stagione e lag del consumo.

### Nuovi modelli mean effect

File:

- [mean_effect_model_compare.csv](report_assets/mean_effect_model_compare.csv)
- [mean_effect_fixed_effects.csv](report_assets/mean_effect_fixed_effects.csv)
- [mean_effect_key_effects.csv](report_assets/mean_effect_key_effects.csv)
- [mean_effect_group_slopes.csv](report_assets/mean_effect_group_slopes.csv)

Statistiche salvate:

- confronto tra `M3_base`, `M4_arrivi_turistici`, `M4_arrivi_x_temperatura`, `M4_arrivi_eterogeneo`;
- coefficienti completi dei modelli nuovi;
- coefficienti chiave di arrivi, interazione termica e interazione con comune turistico;
- slope degli arrivi nei comuni `touristico` e `non_turistico`.

### Lettura territoriale

File:

- [municipality_profile.csv](report_assets/municipality_profile.csv)
- [municipality_maps.png](report_assets/municipality_maps.png)

Statistiche salvate:

- `istat_muni_code`
- `comune_nome`
- `arrivi_per_residente`
- `letti_per_residente`
- classificazione `touristico / non_turistico`
- `random_intercept` del modello finale

## Risultati sostantivi principali ottenuti fin qui

1. La popolazione comunale va trattata in forma between/within; il random intercept del modello nullo assorbiva molta scala strutturale.
2. La dinamica del consumo, tramite `lag1` e `lag12`, resta il blocco dominante del fit.
3. Gli arrivi turistici migliorano il modello finale oltre la dinamica pura.
4. Gli arrivi residualizzati rispetto a struttura, meteo e calendario mantengono lo stesso contenuto informativo sostanziale del segnale osservato.
5. L'interazione `arrivi x temperatura` non mostra evidenza robusta.
6. L'effetto degli arrivi e' piu' forte nei comuni turistici che nei comuni non turistici.
7. La mappa comunale mostra dove si concentra l'intensita' turistica e dove rimane eterogeneita' residua nel modello finale.

## Risultati chiave in 10 righe

1. La popolazione spiega gran parte dell'eterogeneita' strutturale tra comuni, soprattutto nella componente between.
2. La componente within della popolazione e' debole e non resta robusta nei modelli piu' ricchi.
3. Meteo e stagionalita' migliorano molto il fit, ma non sono il blocco dominante.
4. Il vero salto predittivo arriva con la dinamica del consumo, tramite `lag1` e `lag12`.
5. Anche dopo i lag, gli arrivi turistici aggiungono informazione statisticamente significativa.
6. Il contributo degli arrivi non sparisce quando li residualizzi rispetto a struttura, meteo e calendario.
7. Questo implica che il segnale degli arrivi non e' riducibile a semplice stagionalita' o meteo.
8. L'overlap piu' forte degli arrivi e' con la dinamica del consumo, non con le armoniche stagionali pure.
9. L'interazione `arrivi x temperatura` non mostra evidenza robusta.
10. L'effetto degli arrivi e' invece eterogeneo: risulta piu' forte nei comuni turistici.

## Cosa non e' ancora stato prodotto

Nelle cartelle sorelle [Tail Risk](../Tail%20Risk/README.md) e [Forecast](../Forecast/README.md) esiste solo la struttura iniziale. Non sono ancora stati generati output quantitativi finali per:

- copule sui residui;
- dipendenza in coda;
- forecasting operativo;
- scenari di pianificazione.
