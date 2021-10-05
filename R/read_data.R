
read_institutionele_adressen <- function(inst_path){
  read.csv2(file.path(inst_path, "adressen_lijst.csv"),
            na.strings = "") %>%
    mutate(adres = paste0(Postcode, "_", Huisnummer, "_",Huisnummerletter))
}


read_csv_source <- function(path, filename){
  
  data.table::fread(file.path(path, filename), 
                    na.strings = "",
                    colClasses = "character",
                    showProgress = FALSE)
  
}

#' Lees stambestand (BZSPRSQ00)
#' @export
read_brpcur <- function(brp_path, peil_datum){
  
  data(buurt_key)
  data(geboorte_land_code_key)
  
  # 1. Lees ruwe data, select/rename kolommen
  raw_data <- read_csv_source(brp_path, "BZSPRSQ00") %>%
    mutate(adres = paste0(VBLPOSTCODE, "_", 
                          VBLHUISNUMMER, "_", 
                          VBLHUISLETTER, "_", 
                          VBLHUISNUMMERTOEVOEGING))
  
  data <- dplyr::select(raw_data,
                        adres, 
                        
                        bsn = PRSBURGERSERVICENUMMER,
                        anr = PRSANUMMER,
                        
                        anrouder1 = OU1ANUMMER,  
                        anrouder2 = OU2ANUMMER,   
                        geslacht_ouder1 = OU1GESLACHTSAANDUIDINGCODE,
                        geslacht_ouder2 = OU2GESLACHTSAANDUIDINGCODE,
                        
                        geslacht = PRSGESLACHTSAANDUIDINGCODE,
                        gezinsverhouding = PRSGEZINSVERHOUDING,
                        burgerlijke_staat = PRSBURGERLIJKESTAAT,
                        geboorte_land = PRSGEBOORTELANDOMSCHRIJVING,
                        geboorte_land_code = PRSGEBOORTELANDCODE,
                        
                        geboorte_land_oud1_code = OU1GEBOORTELANDCODE,
                        geboorte_land_oud2_code = OU2GEBOORTELANDCODE,
                        geboorte_land_oud1_omschrijving = OU1GEBOORTELANDOMSCHRIJVING,
                        geboorte_land_oud2_omschrijving = OU2GEBOORTELANDOMSCHRIJVING,
                        
                        datum_geboorte = PRSGEBOORTEDATUM,
                        datum_overlijden = OVLDATUMOVERLIJDEN,
                        datum_inschrijving = VBLDATUMINSCHRIJVING,
                        datum_inschrijving_vws = VWSDATUMINSCHRIJVING,
                        datum_adres = VBLDATUMAANVANGADRESHOUDING,
                        
                        gemeente_inschrijving = VBLGEMEENTEVANINSCHRIJVINGOMSCHRIJVING,
                        gemeente_inschrijving_vws = VWSGEMEENTEVANINSCHRIJVINGOMSCHRIJVING,
                        
                        gemeente_deel = VBLGEMEENTEDEEL,
                        woonplaats = VBLWOONPLAATSNAAM, 
                        postcode = VBLPOSTCODE,
                        huisnummer = VBLHUISNUMMER,
                        huisletter = VBLHUISLETTER,
                        huisnummertoevoeging = VBLHUISNUMMERTOEVOEGING,
                        
                        wijk_code = VBLWIJKCODE,
                        wijk_naam = VBLWIJKOMSCHRIJVING,
                        buurt_code_cipers = VBLPLANALOGISCHEWIJKCODE,
                        buurt_naam = VBLPLANALOGISCHEWIJKOMSCHRIJVING,
                        
                        soort_pand_code = VBLSOORTPANDCODE,
                        soort_pand_omschrijving = VBLSOORTPANDOMSCHRIJVING,
                        
                        aanduidingverblijfstitelcode = VBTAANDUIDINGVERBLIJFSTITELCODE,
                        verblijfstitelvervallen = VBTVERBLIJFSTITELVERVALLEN,
                        aanduidingverblijfstitelomschrijving = VBTAANDUIDINGVERBLIJFSTITELOMSCHRIJVING,
                        ingangsdatumverblijfstitelindicator = VBTINGANGSDATUMVERBLIJFSTITELINDICATOR,
                        ingangsdatumverblijfstitel = VBTINGANGSDATUMVERBLIJFSTITEL,
                        datumeindeverblijfstitelindicator = VBTDATUMEINDEVERBLIJFSTITELINDICATOR,
                        datumeindeverblijfstitel = VBTDATUMEINDEVERBLIJFSTITEL,
                        
                        gezag_minderjarige_indicatie = GZVINDICATIEGEZAGMINDERJARIGE,
                        indicatie_curatele_register = GZVINDICATIECURATELEREGISTER,
                        prs_document_datum = PRSDATUMDOCUMENT,
                        prs_document_datum_indicator = PRSDATUMDOCUMENTINDICATOR,
                        prs_document_beschrijving = PRSBESCHRIJVINGDOCUMENT,
                        gzv_document_beschrijving = GZVBESCHRIJVINGDOCUMENT,
                        gzv_document_datum_indicator = GZVDATUMDOCUMENTINDICATOR,
                        gzv_document_datum = GZVDATUMDOCUMENT,
                        ou1_document_datum = OU1DATUMDOCUMENT,
                        ou1_document_datum_indicator = OU1DATUMDOCUMENTINDICATOR,
                        ou1_document_beschrijving = OU1BESCHRIJVINGDOCUMENT,
                        ou2_document_datum = OU2DATUMDOCUMENT,
                        ou2_document_datum_indicator = OU2DATUMDOCUMENTINDICATOR,
                        ou2_document_beschrijving = OU2BESCHRIJVINGDOCUMENT)
  
  # 4. Leeftijden, datums.
  data <- data %>%
    mutate(
      leeftijd = as.numeric(difftime(peil_datum, datum_geboorte, units = "weeks")) / 52,
      minder18 = leeftijd < 18, 
      minder23 = leeftijd < 23,
      
      datum_adres = lubridate::ymd(datum_adres),
      datum_geboorte = lubridate::ymd(datum_geboorte),
      datum_overlijden = lubridate::ymd(datum_overlijden),
      datum_inschrijving = lubridate::ymd(datum_inschrijving),
      datum_inschrijving_vws = lubridate::ymd(datum_inschrijving_vws),
    )
  
  data$datum_inschrijving[data$datum_inschrijving == lubridate::ymd("1001-01-01")] <- NA
  data$datum_adres[data$datum_adres == lubridate::ymd("1001-01-01")] <- NA
  
  data <- mutate(data, 
                 datum_adres = coalesce(datum_adres, datum_inschrijving)
  )
  

  # 6b.
  data <- as_tibble(data)
  
  
  # 7. Burgerlijke staat labels
  burgstaat_key <- tibble::tribble(
    ~burgerlijke_staat, ~burgerlijke_staat_omschrijving,
    "A", "Achtergebleven gereg. partner",
    "B", "Gescheiden of beëindigd geregistreerd partnerschap",
    "H", "Gehuwd",
    "O", "Ongehuwd, Geen huwelijk/gereg. partnerschap",
    "P", "Geregistreerd partner",
    "S", "Gescheiden",
    "W", "Weduwe/weduwenaar")
  
  # join, en zet kolom naast de code.
  data <- left_join(data, burgstaat_key, by = "burgerlijke_staat") %>% 
    select(adres:burgerlijke_staat, burgerlijke_staat_omschrijving, everything())
  
  
  # 8. Gezinsverhouding label.
  gezinsverh_key <- tibble::tribble(
    ~gezinsverhouding, ~gezinsverhouding_omschrijving,
    "1", "Man of vrouw met inwonende echtgenoot",
    "2", "Man of vrouw met inwonende echtgenoot en kinderen" ,
    "3", "Man of vrouw met kind",
    "4", "Inwonende echtgeno(o)t(e)",
    "5", "Inwonend ongehuwd kind",
    "6", "Niet in gezinsverband levend/alleenstaand")
  
  data <- left_join(data, gezinsverh_key, by = "gezinsverhouding") %>% 
    select(adres:gezinsverhouding, gezinsverhouding_omschrijving, everything())
  
  
  # 9. 
  if("gezag_minderjarige_indicatie" %in% names(data)){
    
    gezag_key <- tibble::tribble(
      ~gezag_minderjarige_indicatie, ~gezag_minderjarige_omschrijving,
      "1", "ouder1 heeft het gezag",
      "12", "ouder1 + ouder2 hebben het gezag",
      "1D", "ouder1 + een derde hebben het gezag",
      "2", "ouder2 heeft het gezag", 
      "2D", "ouder2 + een derde hebben het gezag",
      "D", "een of meer derden hebben het gezag")
    
    data <- left_join(data, gezag_key, by = "gezag_minderjarige_indicatie") %>% 
      relocate(gezag_minderjarige_omschrijving, .after = gezag_minderjarige_indicatie)
  }
  
  # 10. Voeg id kolom toe om volgorde te behouden
  data <- data %>% 
    mutate(id = 1:nrow(.)) %>%
    dplyr::relocate(id)
  
  
  data
}


add_buurt_wijk_columns <- function(data){
  
  data(buurt_key)
  
  data <- left_join(data, buurt_key, by = "buurt_code_cipers") %>%
    relocate(buurt_code, .after = buurt_code_cipers) %>%
    mutate(buurt_code_cbs  = paste0("BU0228", buurt_code)) %>%
    relocate(buurt_code_cbs, .after = buurt_code) %>%
    mutate(wijk_code_cbs  = paste0("WK0228", wijk_code)) %>%
    relocate(wijk_code_cbs, .after = wijk_code)
  
  data$wijk_code_cbs[data$wijk_code_cbs == "WK0228"] <- NA
  data$buurt_code_cbs[data$buurt_code_cbs == "BU0228NA"] <- NA
  
data
}


add_institutioneel_adres <- function(data, adressen_inst){
  
  # 5. Institutionele adressen : zoals in de syntax van Ilse, join
  # op postcode+huisnummer als geen huisletter, en op 
  # postcode+huisnummer+huisletter als wel huisletter.
  # (omslachtige maar veilige join)
  inst1 <- filter(adressen_inst, !is.na(Huisnummerletter)) %>%
    mutate(adres = paste0(Postcode, "_", Huisnummer, "_", Huisnummerletter))
  inst2 <- filter(adressen_inst, is.na(Huisnummerletter)) %>%
    mutate(adres = paste0(Postcode, "_", Huisnummer))
  
  data <- mutate(data, 
                 adres1 = paste0(postcode, "_", 
                                 huisnummer, "_",
                                 huisletter),
                 adres2 = paste0(postcode, "_", 
                                 huisnummer
                 )
  )
  data$institutioneel_adres <- data$adres1 %in% inst1$adres |
    data$adres2 %in% inst2$adres
  
  data <- select(data, -adres1, -adres2)
  
data
}



read_huwelijk <- function(brp_path){
  
  data <- read_csv_source(brp_path, "BZSHUWQ00") %>%
    select(bsn = PRSBURGERSERVICENUMMER,
           anr = PRSANUMMER,
           bsn_partner = HUWBURGERSERVICENUMMER,
           anr_partner = HUWANUMMER,
           datum_huwelijk = HUWDATUMSLUITINGHUWELIJKPARTNERSCHAP,
           datum_huwelijk_ind = HUWDATUMSLUITINGHUWELIJKPARTNERSCHAPINDICATOR,
           datum_scheiding = HUWDATUMONTBINDINGHUWELIJKPARTNERSCHAP,
           datum_scheiding_ind = HUWDATUMONTBINDINGHUWELIJKPARTNERSCHAPINDICATOR,
           datum_omzetting = HUWDATUMOMZETTINGHUWELIJKPARTNERSCHAP,
           datum_omzetting_ind = HUWDATUMOMZETTINGHUWELIJKPARTNERSCHAPINDICATOR) %>%
    remove_identical() %>%
    mutate(datum_scheiding = lubridate::ymd(datum_scheiding),
           datum_omzetting = lubridate::ymd(datum_omzetting),
           datum_huwelijk = lubridate::ymd(datum_huwelijk),
           datum_huwelijk = replace_na(datum_huwelijk, lubridate::ymd("1001-1-1")))
  
  return(data)  
}


# Filter huwelijk tabel op huidige huwelijken, die nog niet beeindigd zijn.
current_huwelijk <- function(data, peildatum){
  
  data %>%
    filter(datum_huwelijk < peildatum,
           is.na(datum_scheiding) | datum_scheiding > peildatum
    ) %>%
    #select(anr, anr_partner) %>%
    filter(!is.na(anr_partner))
  
}


read_historie <- function(brp_path, levenstabel){
  
  read_csv_source(brp_path, "BZSC58Q00") %>%
    mutate(adres = paste0(VBLHSTPOSTCODE, "_", 
                          VBLHSTHUISNUMMER, "_", 
                          VBLHSTHUISLETTER, "_", 
                          VBLHSTHUISNUMMERTOEVOEGING)) %>%
    select(adres, 
           bsn = PRSBURGERSERVICENUMMER,
           anr = PRSANUMMER,
           gemeente_inschrijving = VBLHSTGEMEENTEVANINSCHRIJVINGOMSCHRIJVING,
           adres_functie = VBLHSTFUNCTIEADRES,
           datum_inschrijving = VBLHSTDATUMINSCHRIJVING,
           datum_inschrijving_ind = VBLHSTDATUMINSCHRIJVINGINDICATOR,
           datum_adres = VBLHSTDATUMAANVANGADRESHOUDING,
           datum_adres_ind = VBLHSTDATUMAANVANGADRESHOUDINGINDICATOR,
           adres_buitenland = VBLHSTLANDADRESBUITENLANDOMSCHRIJVING,
           datum_adres_buitenland = VBLHSTDATUMAANVANGADRESBUITENLAND,
           datum_nederland = VBLHSTDATUMVESTIGINGINNEDERLAND,
           datum_nederland_ind = VBLHSTDATUMVESTIGINGINNEDERLANDINDICATOR,
           land_ingeschreven = VBLHSTLANDVANWAARINGESCHREVENOMSCHRIJVING
           
           
    )%>%
    mutate(
      datum_inschrijving = lubridate::ymd(datum_inschrijving),
      datum_adres = lubridate::ymd(datum_adres),
      datum_adres_buitenland = lubridate::ymd(datum_adres_buitenland),
      datum_adres = coalesce(datum_adres, datum_adres_buitenland)
    ) %>%
    remove_identical() %>%
    left_join(levenstabel, by = "anr")
  
}


read_kind <- function(brp_path){
  
  read_csv_source(brp_path, "BZSKINQ00") %>%
    mutate(KNDGEBOORTEDATUM = lubridate::ymd(KNDGEBOORTEDATUM))
  
}


current_kinderen <- function(data, peil_datum){
  
  data %>%
    filter(KNDGEBOORTEDATUM < peil_datum) %>%
    count(PRSANUMMER, name = "aantal_kinderen") %>%
    rename(anr = PRSANUMMER)
  
}

