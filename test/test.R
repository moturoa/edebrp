
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
 devtools::load_all()

#library(edebrp)


.peil_datum <- today() - 1

con <- shintobag::shinto_db_connection("ede_dd_data2", 
                                       file = "d:/repos/ede_datadienst_dataportal/conf/config.yaml")


# Voor Ede kant
if(FALSE){
  brp_path <- "d:/repos/ede/DATA/datadienst/dd-data/brp"
  inst_path <- "d:/repos/ede/DATA/datadienst/dd-data/institutionele_adressen"
  
  bzsprs2 <- read_bzsprs(brp_path = brp_path)
  adressen_inst2 <- read_institutionele_adressen(inst_path = inst_path)
  bzsc2 <- read_bzsc58(brp_path = brp_path)
  huwelijk2 <- read_huwelijk(brp_path = brp_path)
  kind2 <- read_kind(brp_path = brp_path)
  
}


# pink
if(FALSE){
  
  brp_path <- "test"
  inst_path <- "test"
  
  
  bzsc2 <- read_bzsc58(brp_path = brp_path, basename = "BZSC58Q00_pink.csv") %>%
    mutate(datum_inschrijving = ymd(datum_inschrijving),
           datum_adres = ymd(datum_adres)
    )
  
  
  bzsprs2 <- read_bzsprs(brp_path = brp_path, basename = "bzsprsq00_pink.csv") %>%
    mutate(datum_adres = ymd(datum_adres),
           datum_geboorte = ymd(datum_geboorte),
           datum_overlijden = ymd(datum_overlijden),
           datum_inschrijving = ymd(datum_inschrijving),
           datum_inschrijving_vws = ymd(datum_inschrijving)
           )
  
    
  adressen_inst2 <- read_institutionele_adressen(inst_path = inst_path)
  
  huwelijk2 <- read_huwelijk(brp_path = brp_path, basename = "BZSHUWQ00_pink.csv")
  kind2 <- read_kind(brp_path = brp_path, basename = "BZSKINQ00_pink.csv")
  
  brpstam2 <- read_brpstam(bzsprs2,
                          adressen_inst2,
                          .peil_datum)
  
  historie2 <- read_historie(bzsc2, brpstam2)
  
  adres_historie2 <- bind_rows(
    select(brpstam2, anr, adres, datum_adres, datum_inschrijving, gemeente_inschrijving,
           gemeente_deel,woonplaats,postcode,huisnummer,huisletter,huisnummertoevoeging,wijk_code,
           wijk_naam,buurt_code_cipers,buurt_naam,soort_pand_code,soort_pand_omschrijving
    ),
    select(historie2, anr, adres, datum_adres, datum_inschrijving, gemeente_inschrijving, 
           gemeente_deel,woonplaats,postcode,huisnummer,huisletter,huisnummertoevoeging,wijk_code,
           wijk_naam,buurt_code_cipers,buurt_naam,soort_pand_code,soort_pand_omschrijving
    )
  )
  
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

# optioneel.
#brpstam2 <- add_ethniciteit_columns(brpstam)

# Verblijfshistorie (626k rijen)
historie <- read_historie(bzsc, brpstam)


adres_historie <- bind_rows(
  select(brpstam, anr, adres, datum_adres, datum_inschrijving, gemeente_inschrijving,gemeente_inschrijving_vws,datum_inschrijving_vws,
         gemeente_deel,woonplaats,postcode,huisnummer,huisletter,huisnummertoevoeging,wijk_code,
         wijk_naam,buurt_code_cipers,buurt_naam,soort_pand_code,soort_pand_omschrijving
  ),
  select(historie, anr, adres, datum_adres, datum_inschrijving, gemeente_inschrijving, 
         gemeente_deel,woonplaats,postcode,huisnummer,huisletter,huisnummertoevoeging,wijk_code,
         wijk_naam,buurt_code_cipers,buurt_naam,soort_pand_code,soort_pand_omschrijving
  )
)


tictoc::toc()

# 
tictoc::tic("Huishoudens")
hh <- bepaal_huishoudens(.peil_datum, 
                         brpstam, 
                         historie, 
                         huwelijk, 
                         kind, 
                         adressen_inst, 
                         verhuis_wezen = TRUE,
                         leeftijd_delta_koppel = 8,
                         datum_adres_koppel = 15,
                         ethniciteit = FALSE,
                         buurt_wijk_codes = TRUE
                         )

hh2 <- add_ethniciteit_columns(hh)

brp_summary(brp_huishoudens_huidig)
tictoc::toc()




