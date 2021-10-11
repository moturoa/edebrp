
# 
# library(lubridate)
# library(dplyr)
# library(DBI)
# library(tidyr)
# library(glue)
# library(futile.logger)
# library(dbplyr)
# 
# 
# devtools::load_all()

library(edebrp)


.peil_datum <- today() - 1

con <- shintobag::shinto_db_connection("ede_dd_data", 
                                       file = "c:/repos/ede/ede_datadienst_dashboard/conf/config.yaml")


# Voor Ede kant
if(FALSE){
  brp_path <- "c:/repos/ede/DATA/dd-data/brp"
  inst_path <- "c:/repos/ede/DATA/dd-data/institutionele_adressen"
  
  bzsprs2 <- read_bzsprs(brp_path = brp_path)
  adressen_inst2 <- read_institutionele_adressen(inst_path = inst_path)
  bzsc2 <- read_bzsc58(brp_path = brp_path)
  huwelijk2 <- read_huwelijk(brp_path = brp_path)
  kind2 <- read_kind(brp_path = brp_path)
  
}



# Uit DB
tictoc::tic("Data DB read")

bzsprs <- read_bzsprs(con = con)
adressen_inst <- read_institutionele_adressen(con = con)
bzsc <- read_bzsc58(con = con)
huwelijk <- read_huwelijk(con = con)
kind <- read_kind(con = con)

# stambestand (241k rijen)
brpstam <- read_brpstam(bzsprs,
                        adressen_inst,
                        .peil_datum)

# Verblijfshistorie (626k rijen)
historie <- read_historie(bzsc, brpstam)

tictoc::toc()

# 
tictoc::tic("Huishoudens")
brp_huishoudens_huidig <- bepaal_huishoudens(.peil_datum, 
                                             brpstam, 
                                             historie, 
                                             huwelijk, 
                                             kind, 
                                             adressen_inst, 
                                             verhuis_wezen = TRUE,
                                             leeftijd_delta_koppel = 8,
                                             datum_adres_koppel = 15,
                                             ethniciteit = TRUE,
                                             buurt_wijk_codes = TRUE
                                             )

brp_summary(brp_huishoudens_huidig)
tictoc::toc()




