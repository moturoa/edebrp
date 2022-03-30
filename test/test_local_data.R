
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
