#  Dati di flusso turisto  e consumo

Flussi_consumi_finoSETTEMBRE2025.xlsx

# Dati ambientali

-https://cf.regione.vda.it/it/mappa-dati-stazioni-periferiche
-https://cf.regione.vda.it/it/temperature

# Dati Altitude
https://tinitaly.pi.ingv.it/

# Potenziali variabili di interesse

# Dataset Variabili – Consumo Energetico & Turismo

## 📊 Tabella Variabili

| Categoria | Variabile | Descrizione | Frequenza | Source | Presente |
|----------|----------|------------|-----------|--------|----------|
| 🔥 Consumo | Consumo totale energia | Consumo energetico totale comunale | Giornaliera/Mensile |  | ☐ |
| 🔥 Consumo | Consumo residenziale | Consumo settore domestico | Giornaliera/Mensile |  | ☐ |
| 🔥 Consumo | Consumo commerciale | Consumo attività commerciali | Giornaliera/Mensile |  | ☐ |
| 🔥 Consumo | Consumo industriale | Consumo settore industriale | Giornaliera/Mensile |  | ☐ |
| 🌡️ Meteo | Temperatura media | Temperatura media giornaliera | Giornaliera |  | ☐ |
| 🌡️ Meteo | Temperatura minima | Temperatura minima | Giornaliera |  | ☐ |
| 🌡️ Meteo | Temperatura massima | Temperatura massima | Giornaliera |  | ☐ |
| 🌡️ Meteo | Precipitazioni | Pioggia (mm) | Giornaliera |  | ☐ |
| 🌡️ Meteo | Neve | Altezza neve | Giornaliera |  | ☐ |
| 🌡️ Meteo | Umidità | Umidità relativa (%) | Giornaliera |  | ☐ |
| 🌡️ Meteo | Velocità vento | Intensità vento | Giornaliera |  | ☐ |
| 🌡️ Meteo | HDD (Heating Degree Days) | Domanda riscaldamento | Giornaliera |  | ☐ |
| 🌡️ Meteo | CDD (Cooling Degree Days) | Domanda raffrescamento | Giornaliera |  | ☐ |
| 📅 Tempo | Giorno della settimana | Lunedì–Domenica | Giornaliera |  | ☐ |
| 📅 Tempo | Weekend | Flag weekend | Giornaliera |  | ☐ |
| 📅 Tempo | Festività | Giorni festivi | Giornaliera |  | ☐ |
| 📅 Tempo | Mese | Numero mese | Mensile |  | ☐ |
| 📅 Tempo | Stagione | Inverno/Primavera/Estate/Autunno | Mensile |  | ☐ |
| 📅 Tempo | Giorno dell’anno | Progressivo 1–365 | Giornaliera |  | ☐ |
| 📉 Lag | Consumo t-1 | Consumo giorno precedente | Giornaliera |  | ☐ |
| 📉 Lag | Consumo t-7 | Consumo settimana precedente | Giornaliera |  | ☐ |
| 📉 Lag | Consumo t-30 | Consumo mese precedente | Giornaliera |  | ☐ |
| 📉 Lag | Media mobile 7gg | Media consumo ultimi 7 giorni | Giornaliera |  | ☐ |
| 📉 Lag | Media mobile 30gg | Media consumo ultimi 30 giorni | Giornaliera |  | ☐ |
| 🏨 Turismo | Presenze turistiche | Numero notti turistiche | Giornaliera/Mensile |  | ☐ |
| 🏨 Turismo | Arrivi turistici | Numero arrivi | Giornaliera/Mensile |  | ☐ |
| 🏨 Turismo | Occupazione alberghiera | % occupazione hotel | Giornaliera/Mensile |  | ☐ |
| 🏨 Turismo | Permanenza media | Notti medie per turista | Mensile |  | ☐ |
| 🏨 Turismo | Numero strutture ricettive | Hotel, B&B, ecc. | Annuale |  | ☐ |
| 🏨 Turismo | Eventi locali | Eventi/festival rilevanti | Giornaliera |  | ☐ |
| 🏨 Turismo | Alta stagione (flag) | Periodi turistici | Giornaliera |  | ☐ |
| 🚗 Proxy Turismo | Traffico veicolare | Volume traffico | Giornaliera |  | ☐ |
| 🚗 Proxy Turismo | Dati mobile | Presenze da celle telefoniche | Giornaliera |  | ☐ |
| 🚗 Proxy Turismo | Ricerca Google Trends | Interesse turistico | Settimanale |  | ☐ |
| 🏙️ Territorio | Popolazione residente | Numero abitanti | Annuale |  | ☐ |
| 🏙️ Territorio | Densità abitativa | Ab/km² | Annuale |  | ☐ |
| 🏙️ Territorio | Superficie comune | Area geografica | Annuale |  | ☐ |
| ⚡ Energia | Prezzo energia | Prezzo medio energia | Giornaliera/Mensile |  | ☐ |
| ⚡ Energia | Tipologia energia | Elettrica / Gas | Statico |  | ☐ |
| 🏢 Economia | Numero imprese | Attività economiche | Annuale |  | ☐ |
| 🏢 Economia | Indicatori economici | PIL locale / proxy | Annuale |  | ☐ |

---

## ✅ Note
- Colonna **Source** → inserisci la fonte (ISTAT, Terna, ARPA, ecc.)
- Colonna **Presente** → usa `☑` quando la variabile è disponibile
- Frequenze diverse → richiedono allineamento temporale (Step A)

---

## 🚀 Consiglio
Inizia con:
- Consumi
- Meteo
- Calendario  
Poi aggiungi turismo → per misurare il vero impatto
