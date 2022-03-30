

anrs <- unique(adres_historie$anr)






stam1 <- brp_tijdmachine(historie, brpstam, as.Date("2020-1-1"))


stam2 <- adres_historie2 %>% 
  filter(gemeente_inschrijving == "Ede",
         datum_adres <= as.Date("2020-1-1")) %>%
  group_by(anr) %>% 
  filter(datum_adres == max(datum_adres)) %>%
  ungroup



stam1$anr[!stam1$anr %in% stam2$anr]



test <- function(anr_ = NULL, printcase=TRUE){
  
  if(is.null(anr_))anr_ <- sample(anrs,1)
  
  f1 <- filter(adres_historie, anr == !!anr_) %>%
    select(anr, datum_adres, datum_inschrijving, gemeente_inschrijving, datum_inschrijving_vws, gemeente_inschrijving_vws) %>%
    arrange(desc(datum_adres))
  
  if(!is.na(f1$gemeente_inschrijving_vws[1])){
    f1 <- bind_rows(
      tibble(anr = anr_, datum_adres = NA, datum_inschrijving = f1$datum_inschrijving_vws[1], gemeente_inschrijving = f1$gemeente_inschrijving_vws[1]),
      f1[-1,]
    )
  }
  
  f1 <- select(f1, -datum_inschrijving_vws, -gemeente_inschrijving_vws)
  
  f2 <- filter(adres_historie2, anr == !!anr_) %>%
    select(anr, datum_adres, datum_inschrijving, gemeente_inschrijving) %>%
    arrange(desc(datum_adres))
  
  if(isTRUE(f1$gemeente_inschrijving[1] != f2$gemeente_inschrijving[1])){
    print("---------------------------------------------------------------------")
    print("Burgerzaken")
    print(f1)
    print("Pink")
    print(f2)  
  }

  if(printcase){
    print("Burgerzaken")
    print(f1)
    print("Pink")
    print(f2)  
  }
  
  
}


.peil_datum <- as.Date("2020-1-1")

data_bz <- filter(adres_historie, gemeente_inschrijving == "Ede",
                  datum_adres < .peil_datum) %>%
  pull(anr) %>% unique

data_pi <- filter(adres_historie2, gemeente_inschrijving == "Ede",
                  datum_adres < .peil_datum) %>%
  pull(anr) %>% unique

mis <- data_bz[!data_bz %in% data_pi]


# test gevallen
# cL3VjGA71  = 2983915963   (RNI)
# 2T6OlFdLA  = 9730458431   (1 verhuisbeweging ontbreekt)
# WE42Y1e3o = 3147693056


