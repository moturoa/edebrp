


.peil_datum <- as.Date("2019-1-1")

devtools::load_all()

brp_path <- "d:/repos/ede/DATA/datadienst/dd-data/brp_cipers//"
inst_path <- "d:/repos/ede/DATA/datadienst/dd-data/institutionele_adressen/"


bzsc <- read_bzsc58(brp_path = brp_path, basename = "BZSC58Q00")

bzsprs <- read_bzsprs(brp_path = brp_path, basename = "BZSPRSQ00") 

adressen_inst <- read_institutionele_adressen(inst_path = inst_path)

huwelijk <- read_huwelijk(brp_path = brp_path, basename = "BZSHUWQ00")
kind <- read_kind(brp_path = brp_path, basename = "BZSKINQ00")


brpstam <- read_brpstam(bzsprs, adressen_inst, .peil_datum, date_format = "old")

historie <- read_historie(bzsc, brpstam, date_format = "old")


o <- brp_tijdmachine_cipers(historie, brpstam, .peil_datum)


hh <- bepaal_huishoudens(.peil_datum, brpstam, historie, huwelijk, kind, adressen_inst,
                         format = "old")





check1 <- function(id = NULL){
  
  if(is.null(id))id <- sample(historie$bsn,1)
  
  one <- filter(brpstam, bsn == !!id) %>%
    select(bsn,adres, datum_geboorte, datum_overlijden, datum_inschrijving, datum_adres, gemeente_inschrijving_vws) %>%
    as_tibble
  
  two <- filter(historie, bsn == !!id) %>%
    select(bsn, adres,gemeente_inschrijving, 
           datum_geboorte, datum_overlijden, datum_inschrijving, datum_adres) %>%
    arrange(desc(datum_adres)) %>%
    as_tibble
  
  bind_rows(one,two)
  
}





# alleen te maken met oude levering
make_buurt_koppel_pink <- function(hh){
  select(hh, buurt_code_cipers, buurt_code_cbs, buurt_naam) %>% distinct  
}

# buurt_koppel_fix_pink <- make_buurt_koppel_pink(hh)
# usethis::use_data(buurt_koppel_fix_pink, overwrite = TRUE)






adres_historie <- bind_rows(
  select(brpstam, anr, adres, datum_adres, datum_inschrijving, gemeente_inschrijving,
         gemeente_deel,woonplaats,postcode,huisnummer,huisletter,huisnummertoevoeging,wijk_code,
         wijk_naam,buurt_code_cipers,buurt_naam,soort_pand_code,soort_pand_omschrijving
  ),
  select(historie, anr, adres, datum_adres, datum_inschrijving, gemeente_inschrijving, 
         gemeente_deel,woonplaats,postcode,huisnummer,huisletter,huisnummertoevoeging,wijk_code,
         wijk_naam,buurt_code_cipers,buurt_naam,soort_pand_code,soort_pand_omschrijving
  )
) %>% 
  filter(gemeente_inschrijving == "Ede",
         adres != "NA_NA_NA_NA")
         