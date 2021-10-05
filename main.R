

library(lubridate)
library(dplyr)
library(DBI)
library(tidyr)
library(glue)
library(futile.logger)


devtools::load_all()

brp_path <- "c:/repos/ede/DATA/dd-data/brp"
inst_path <- "c:/repos/ede/DATA/dd-data/institutionele_adressen"

.peil_datum <- today()-1

adressen_inst <- read_institutionele_adressen(inst_path)

# stambestand (241k rijen)
brpcur <- read_brpcur(brp_path, 
                      .peil_datum)


# Koppeltabel geboorte/overlijden
levenstabel <- brpcur %>%
  select(anr, anrouder1, anrouder2, datum_geboorte, datum_overlijden) %>%
  distinct


# Verblijfshistorie ()
historie <- read_historie(brp_path, levenstabel)


# Huwelijk historie
huwelijk <- read_huwelijk(brp_path)

# Kind tabel
kind <- read_kind(brp_path)


# 
brp_huishoudens_huidig <- bepaal_huishoudens(brpcur, historie, huwelijk, kind, adressen_inst, .peil_datum,
                                             
                                             verhuis_wezen = TRUE,
                                             leeftijd_delta_koppel = 8,
                                             datum_adres_koppel = 15
                                             )


# dit hoeft niet van te voren, kan ook achteraf voor een dataset met kolommen:
# geboorte_land_oud1_code, geboorte_land_oud2_code, geslacht_ouder1, geslacht_ouder2
# geboorte_land_code
brp_huishoudens_huidig2 <- add_ethniciteit_columns(brp_huishoudens_huidig)


# idem, alleen aan het eind 
brp_huishoudens_huidig3 <- add_buurt_wijk_columns(brp_huishoudens_huidig2)




