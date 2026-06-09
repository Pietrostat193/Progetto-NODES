# Summary di `data_full`

## Panoramica

Il dataset finale `data_full` è un pannello comune-mese per la Valle d’Aosta. Contiene **9.546 righe** e **71 variabili**, coprendo **74 comuni** nel periodo **da gennaio 2015 a settembre 2025**. La struttura temporale è mensile, con una riga per ogni combinazione `comune × mese`.

## Dimensioni del dataset

- **Righe:** 9.546
- **Colonne:** 71
- **Unità spaziali:** 74 comuni
- **Intervallo temporale:** dal 2015-01-01 al 2025-09-01
- **Frequenza:** mensile

La distribuzione annuale è coerente con un pannello comune-mese bilanciato:
- **2015–2024:** 888 righe per anno, corrispondenti a `74 comuni × 12 mesi`
- **2025:** 666 righe, corrispondenti a `74 comuni × 9 mesi`

## Tipi di dati

Il dataset include una combinazione di tipi di variabili:

- **Variabili character** per identificativi e descrizioni testuali, come codice e nome del comune
- **Variabili Date** per l’indice temporale mensile
- **Variabili integer** per conteggi discreti e campi di calendario
- **Variabili numeric** per indicatori continui, variabili turistiche, meteorologiche e socio-demografiche

In particolare:
- `istat_muni_code`, `comune_nome`, `comune_key`, `mese_anno`, `unita_territoriale`, `mese_label` e diversi descrittori territoriali sono memorizzati come **character**
- `date` è memorizzata come **Date**
- `year` e `month_num` sono memorizzati come **integer**
- la maggior parte delle variabili analitiche sostanziali è memorizzata come **numeric** oppure **integer**

## Gruppi di variabili

Le 71 variabili possono essere suddivise in sei blocchi principali.

### 1. Variabili identificative e temporali (7)

Queste variabili definiscono l’unità di osservazione e la struttura temporale mensile:

- `istat_muni_code`
- `comune_nome`
- `comune_key`
- `date`
- `year`
- `month_num`
- `mese_anno`

### 2. Variabili di turismo ed energia (8)

Queste variabili descrivono i flussi turistici mensili, la capacità ricettiva, la popolazione residente e il consumo elettrico:

- `unita_territoriale`
- `mese_label`
- `totale_presenze`
- `totale_arrivi`
- `numero_alloggi`
- `numero_letti`
- `residenti`
- `kwh`

### 3. Variabili meteorologiche (6)

Si tratta di aggregati meteorologici mensili a livello comunale:

- `muni_poly_id`
- `n_points`
- `precipitazione`
- `pressione`
- `temperatura`
- `umidit_relativa`

### 4. Variabili demografiche annuali da `demo_data` (19)

Queste variabili rappresentano stock e flussi demografici annuali, replicati su tutti i mesi dell’anno corrispondente:

- `demo_popolazione_1_gennaio`
- `demo_nati_vivi`
- `demo_morti`
- `demo_saldo_naturale`
- `demo_immigrati_da_altro_comune`
- `demo_emigrati_per_altro_comune`
- `demo_saldo_migratorio_interno`
- `demo_immigrati_dall_estero`
- `demo_emigrati_per_estero`
- `demo_saldo_migratorio_estero`
- `demo_variazioni_territoriali`
- `demo_aggiustamento_statistico`
- `demo_saldo_totale`
- `demo_popolazione_31_dicembre`
- `demo_numero_famiglie_31_dicembre`
- `demo_popolazione_residente_famiglia_31_dicembre`
- `demo_componenti_medi_famiglia_31_dicembre`
- `demo_numero_convivenze_31_dicembre`
- `demo_popolazione_residente_convivenza_31_dicembre`

### 5. Metadati territoriali da `df_vda` (5)

Queste variabili descrivono la classificazione territoriale più ampia di ciascun comune:

- `ripartizione_geografica`
- `regione_codice`
- `regione_nome`
- `provincia_nome`
- `comune_capoluogo`

### 6. Indicatori comunali annuali da `df_vda` (26)

Questi includono indicatori socio-economici, occupazionali, di mobilità, cultura, reddito, servizi sociali e uso del suolo:

- `vda_autovetture`
- `vda_bassa_intensita_lavorativa`
- `vda_comune_infanzia`
- `vda_edu_secondaria`
- `vda_edu_terziaria`
- `vda_famiglie_monoreddito`
- `vda_giovani_disoccupati`
- `vda_incidenti`
- `vda_incidenti_lesivita`
- `vda_incidenti_mortalita`
- `vda_moto_autovetture`
- `vda_moto_motocicli`
- `vda_nbiblioteca`
- `vda_nmusei_etc_100`
- `vda_nvisitatori_100`
- `vda_occupati_non_stabili`
- `vda_raccolta_differenziata`
- `vda_reddito`
- `vda_reddito_sub10`
- `vda_ricerca`
- `vda_servizi_sociali_tot`
- `vda_suolo`
- `vda_tasso_disoccupazione`
- `vda_tasso_inattivita`
- `vda_tasso_occupazione`
- `vda_test`

## Struttura dei dati mancanti

La presenza di valori mancanti è dovuta soprattutto alle diverse coperture temporali delle fonti, più che a errori di merge.

### Copertura delle variabili meteorologiche

Le variabili meteorologiche (`muni_poly_id`, `n_points`, `precipitazione`, `pressione`, `temperatura`, `umidit_relativa`) hanno ciascuna **3.552 valori mancanti**. Questo è coerente con il fatto che i dati meteo iniziano nel **2019**, mentre il pannello finale parte dal **2015**. Di conseguenza, le righe dal 2015 al 2018 non hanno informazioni meteorologiche.

### Copertura delle variabili demografiche

Tutte le variabili `demo_` hanno **4.218 valori mancanti**, indicando che i dati demografici annuali sono disponibili solo dal **2019 in poi**. Di conseguenza, gli anni 2015–2018 sono strutturalmente mancanti per questo blocco.

### Copertura delle variabili `df_vda`

Il blocco `df_vda` presenta in generale **2.442 valori mancanti** per le variabili disponibili fino al **2022**, riflettendo il fatto che questi indicatori annuali non si estendono al 2023–2025. Alcune variabili `vda_` hanno molti più valori mancanti, il che suggerisce che siano disponibili solo per un sottoinsieme di anni anche all’interno del periodo 2014–2022. Per esempio:

- `vda_bassa_intensita_lavorativa`: 9.474 valori mancanti
- `vda_giovani_disoccupati`: 9.474 valori mancanti
- `vda_occupati_non_stabili`: 9.474 valori mancanti
- `vda_raccolta_differenziata`: 8.718 valori mancanti
- `vda_tasso_disoccupazione`: 6.882 valori mancanti

## Interpretazione

Nel complesso, `data_full` è un **dataset panel integrato ricco e strutturato**, che combina:

- dati mensili di turismo ed energia,
- dati meteorologici mensili a livello comunale,
- variabili demografiche annuali,
- indicatori annuali strutturali e socio-economici a livello comunale.

Il dataset è adatto ad analisi panel, esplorazione descrittiva e modellazione predittiva a livello comune-mese. Il suo limite principale non è la struttura del pannello, che è coerente e bilanciata, ma il fatto che i diversi blocchi informativi abbiano coperture temporali differenti. Qualsiasi strategia di modellazione dovrebbe quindi tenere esplicitamente conto delle finestre temporali di disponibilità delle diverse fonti.
