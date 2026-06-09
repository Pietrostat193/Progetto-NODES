# Municipality Shapefile

library(sf)
library(dplyr)
library(stringi)
library(janitor)

italy_com <- st_read("~/Desktop/Research/UNVDA/data/Com01012025_g_WGS84.shp", quiet = FALSE) %>%
  janitor::clean_names()

# 2) Build the SAME municipality_key logic you use in your panel (lowercase, no accents, no punctuation/spaces)
make_key <- function(x) {
  x %>%
    as.character() %>%
    stri_trans_general("Latin-ASCII") %>%  # é -> e, ù -> u, etc.
    tolower() %>%
    gsub("[^a-z0-9]+", "", .)              # drop spaces, hyphens, apostrophes, etc.
}

# 3) Subset Valle d'Aosta (cod_reg == 2) and create keys + keep a stable ISTAT municipality code
vda_sf <- italy_com %>%
  filter(as.integer(cod_reg) == 2) %>%
  mutate(
    municipality_name = comune,
    municipality_key  = make_key(comune),
    istat_muni_code   = pro_com_t          # 6-digit municipality code as character
  ) %>%
  select(istat_muni_code, municipality_name, municipality_key, geometry)

save(vda_sf, file = "vda_sf.RData")
