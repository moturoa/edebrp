


.peil_datum <- as.Date("2022-1-19")

devtools::load_all()



from_disk <- FALSE


if(from_disk){
  
  brp_path <- "d:/repos/ede/DATA/datadienst/dd-data/brp/"
  inst_path <- "d:/repos/ede/DATA/datadienst/dd-data/institutionele_adressen/"
  
  bzsc <- read_bzsc58(brp_path = brp_path)
  
  bzsprs <- read_bzsprs(brp_path = brp_path) 
  
  adressen_inst <- read_institutionele_adressen(inst_path = inst_path)
  
  huwelijk <- read_huwelijk(brp_path = brp_path)
  kind <- read_kind(brp_path = brp_path)
  
  brpstam <- read_brpstam(bzsprs, adressen_inst, .peil_datum)
  
  dim(bzsc)
  dim(bzsprs)
  dim(huwelijk)
  dim(kind)
  dim(brpstam)
  
  historie <- read_historie(bzsc, brpstam)
  
} else {
  
  
  bzsc <- read_bzsc58(con = con)
  
  bzsprs <- read_bzsprs(con = con) 
  
  adressen_inst <- read_institutionele_adressen(con = con)
  
  huwelijk <- read_huwelijk(con = con)
  kind <- read_kind(con = con)
  
  brpstam <- read_brpstam(bzsprs, adressen_inst, .peil_datum)
  
  dim(bzsc)
  dim(bzsprs)
  dim(huwelijk)
  dim(kind)
  dim(brpstam)
  
  historie <- read_historie(bzsc, brpstam)
  
}

.peil_datum <- as.Date("2022-1-18")
hh <- bepaal_huishoudens(.peil_datum, brpstam, historie, huwelijk, kind, adressen_inst,
                         buurt_wijk_codes = FALSE)

hh2 <- add_buurt_wijk_columns(hh)






check1 <- function(id = NULL){
  
  if(is.null(id))id <- sample(historie$bsn,1)
  
  one <- filter(brpstam, bsn == !!id) %>%
    select(bsn,adres, datum_geboorte, datum_overlijden, datum_inschrijving, datum_adres, gemeente_inschrijving) %>%
    as_tibble
  
  two <- filter(historie, bsn == !!id) %>%
    select(bsn, adres, datum_geboorte, datum_overlijden, datum_inschrijving, datum_adres, gemeente_inschrijving) %>%
    arrange(desc(datum_adres)) %>%
    as_tibble
  
  bind_rows(one,two)
}


check1("dMXhtPS8B")
check1("PoMucgeUV")



