


devtools::load_all()

.peil_datum <- as.Date("2020-1-1")  #today() - 1

con <- shintobag::shinto_db_connection("ede_dd_data2", 
                                       file = "c:/repos/ede_datadienst_dataportal/conf/config.yaml")


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
                         buurt_wijk_codes = FALSE
                         )


hh2 <- add_ethniciteit_columns(hh)

brp_summary(hh)

tictoc::toc()




