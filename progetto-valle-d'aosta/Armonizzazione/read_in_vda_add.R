# Read-in altre variabili

library(readxl)
library(dplyr)
library(purrr)
library(janitor)
library(readr)

cartella <- path.expand("~/Desktop/VDA")
files_excel <- list.files(cartella, pattern = "\\.xlsx$", full.names = TRUE)

files_excel

# Primo file /Users/camillandreozzi/Desktop/VDA/10-Territorio-e-ambiente.xlsx"  

excel_sheets(files_excel[1])

df_raccolta_differenziata <- read_excel(files_excel[1], sheet = "Tav. 6.1 Comuni", skip = 2)

df_autovetture <- read_excel(files_excel[1], sheet = "Tav. 8.1 Comuni", skip = 2)

df_suolo <- read_excel(files_excel[1], sheet = "Tav. 9.1 Comuni", skip = 2)


# Secondo file "/Users/camillandreozzi/Desktop/VDA/12-Ricerca-innovazione.xlsx"              

excel_sheets(files_excel[2])

df_ricerca <- read_excel(files_excel[2], sheet = "Tav. 1.1 Comuni", skip = 2)

# Terzo file "/Users/camillandreozzi/Desktop/VDA/13a-Infrastrutture-e-mobilita-incidenti-stradali.xlsx" 

excel_sheets(files_excel[3])

df_incidenti <- read_excel(files_excel[3], sheet = "Tav. 1.1 Comuni", skip = 2)

df_incidenti_mortalita <- read_excel(files_excel[3], sheet = "Tav. 3.1 Comuni", skip = 2)

df_incidenti_lesivita <- read_excel(files_excel[3], sheet = "Tav. 4.1 Comuni", skip = 2)

# Quarto file "/Users/camillandreozzi/Desktop/VDA/13b-Infrastrutture-e-mobilita-per-tipologia-di-servizi.xlsx"   

excel_sheets(files_excel[4])

# None

# Quinto file "/Users/camillandreozzi/Desktop/VDA/13c-Infrastrutture-e-mobilita-per-tassi-di-motorizzazione-e-proventi-dalle-sanzioni.xlsx"

excel_sheets(files_excel[5])

df_moto_autovetture <- read_excel(files_excel[5], sheet = "Tav. 9.1 Comuni", skip = 2)

df_moto_motocicli <- read_excel(files_excel[5], sheet = "Tav. 10.1 Comuni", skip = 2)

# Sesto file "/Users/camillandreozzi/Desktop/VDA/3-Istruzione.xlsx"    

excel_sheets(files_excel[6])

df_comune_infanzia <- read_excel(files_excel[6], sheet = "Tav. 1.1 Comuni", skip = 2)

df_edu_secondaria <- read_excel(files_excel[6], sheet = "Tav. 4.1 Comuni", skip = 2)

df_edu_terziaria <- read_excel(files_excel[6], sheet = "Tav. 5.1 Comuni", skip = 2)

df_giovani_disoccupati <- read_excel(files_excel[6], sheet = "Tav. 6.1 Comuni", skip = 2)

# Settimo file "/Users/camillandreozzi/Desktop/VDA/4-Lavoro.xlsx" 

excel_sheets(files_excel[7])

df_tasso_occupazione <- read_excel(files_excel[7], sheet = "Tav. 1.1 Comuni", skip = 2)

df_tasso_disoccupazione <- read_excel(files_excel[7], sheet = "Tav. 2.1 Comuni", skip = 2)

df_tasso_inattività <- read_excel(files_excel[7], sheet = "Tav. 3.1 Comuni", skip = 2)

df_occupati_non_stabili <- read_excel(files_excel[7], sheet = "Tav. 4.1 Comuni", skip = 2)


# Ottavo file "/Users/camillandreozzi/Desktop/VDA/5-Benessere-economico.xlsx" 

excel_sheets(files_excel[8])

df_reddito_sub10 <- read_excel(files_excel[8], sheet = "Tav. 1.1 Comuni", skip = 2)

df_reddito <- read_excel(files_excel[8], sheet = "Tav. 2.1 Comuni", skip = 2)

df_famiglie_monoreddito <- read_excel(files_excel[8], sheet = "Tav. 3.1 Comuni", skip = 2)

df_bassa_intensita_lavorativa <- read_excel(files_excel[8], sheet = "Tav. 4.1 Comuni", skip = 2)

# Nono file "/Users/camillandreozzi/Desktop/VDA/7-Cultura.xlsx"           

excel_sheets(files_excel[9])

df_nbiblioteca <- read_excel(files_excel[9], sheet = "Tav. 1.1 Comuni", skip = 2)

df_nmusei_etc_100 <- read_excel(files_excel[9], sheet = "Tav. 2.1 Comuni", skip = 2)

df_nvisitatori_100 <- read_excel(files_excel[9], sheet = "Tav. 3.1 Comuni", skip = 2)


# Undicesimo file "/Users/camillandreozzi/Desktop/VDA/9b-Servizi-sociali-per-abitante.xlsx"  

excel_sheets(files_excel[11])

df_servizi_sociali_tot <- read_excel(files_excel[11], sheet = "Tav. 2.1 Comuni", skip = 2)


# prendo tutti i df_ ma escludo quello dettagliato con sottocategorie
nomi_df <- setdiff(ls(pattern = "^df_"), "df_servizi_sociali")

lista_df <- mget(nomi_df, envir = .GlobalEnv)

converti_numero <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", ".", "..", "-", "NA")] <- NA
  
  # se c'è la virgola, assumo formato italiano
  if (any(str_detect(x, ","), na.rm = TRUE)) {
    x <- gsub("\\.", "", x)         # toglie eventuali separatori migliaia
    x <- gsub(",", ".", x, fixed = TRUE)
  }
  
  as.numeric(x)
}

prepara_df <- function(df, nome_oggetto) {
  
  nome_variabile <- sub("^df_", "", nome_oggetto)
  
  df <- df %>%
    rename_with(janitor::make_clean_names) %>%
    mutate(codice_comune_istat = as.character(codice_comune_istat))
  
  cols_anno <- names(df)[str_detect(names(df), "^x?\\d{4}$")]
  
  if (length(cols_anno) == 0) {
    stop(paste("Nessuna colonna anno trovata in", nome_oggetto))
  }
  
  df <- df %>%
    mutate(
      across(all_of(cols_anno), converti_numero)
    )
  
  df %>%
    pivot_longer(
      cols = all_of(cols_anno),
      names_to = "anno",
      values_to = nome_variabile
    ) %>%
    mutate(
      anno = as.integer(str_remove(anno, "^x"))
    )
}
# 2. preparo tutti i df

lista_long <- imap(lista_df, prepara_df)

# 4. merge di tutti i dataframe
df_merged <- reduce(
  lista_long,
  full_join,
  by = c(
    "ripartizione",
    "codice_regione",
    "denominazione_regione",
    "provincia",
    "capoluogo",
    "denominazione_comune",
    "codice_comune_istat",
    "anno"
  )
)

# 5. tengo solo Valle d'Aosta
df_vda <- df_merged %>%
  filter(codice_regione == "02")

# risultato finale
glimpse(df_vda)
head(df_vda)

write.csv(df_vda, "df_vda.csv")


