


.peil_datum <- as.Date("2022-1-19")

devtools::load_all()

brp_path <- "d:/repos/ede/DATA/datadienst/dd-data/brp/"
inst_path <- "d:/repos/ede/DATA/datadienst/dd-data/institutionele_adressen/"


bzsc <- read_bzsc58(brp_path = brp_path, basename = "BZSC58Q00_pink.csv")


bzsc2 <- read_csv_source(brp_path, "BZSC58Q00_pink.csv")

bzsprs <- read_bzsprs(brp_path = brp_path, basename = "bzsprsq00_pink.csv") 

adressen_inst <- read_institutionele_adressen(inst_path = inst_path)

huwelijk <- read_huwelijk(brp_path = brp_path, basename = "BZSHUWQ00_pink.csv")
kind <- read_kind(brp_path = brp_path, basename = "BZSKINQ00_pink.csv")

brpstam <- read_brpstam(bzsprs, adressen_inst, .peil_datum)

historie <- read_historie(bzsc, brpstam)


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



