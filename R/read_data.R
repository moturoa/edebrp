



read_csv_source <- function(path, filename){
  
  data.table::fread(file.path(path, filename), 
                    na.strings = "",
                    colClasses = "character",
                    showProgress = FALSE)
  
}

#' Lees institutionele adressen data
#' @export
read_institutionele_adressen <- function(con = NULL, inst_path = NULL, basename = "adressen_lijst.csv"){
  tictoc::tic("inst_adr")
  if(!is.null(inst_path)){
    out <- read.csv2(file.path(inst_path, basename),
              na.strings = "") %>%
      janitor::clean_names() %>%
      mutate(adres = paste0(postcode, "_", huisnummer, "_",huisnummerletter))  
  } else {
    out <- tbl(con, in_schema("pseudo","brp_institutionele_adressen")) %>%
      collect %>%
      mutate(adres = paste0(postcode, "_", huisnummer, "_",huisnummerletter))
    
    out[out == ""] <- NA
  }
  tictoc::toc()
  out
}

#' Read huwelijk data
#' @export
read_huwelijk <- function(con = NULL, brp_path = NULL, basename = "BZSHUWQ00_pink.csv"){
  
  tictoc::tic("bzshuw")
  
  if(!is.null(brp_path)){
    
    out <- read_csv_source(brp_path, basename) %>%
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
  } else {
    
    out <- tbl(con, in_schema("pseudo", "brp_bzshuw")) %>% 
      select(bsn = prsburgerservicenummer,
             anr = prsanummer,
             bsn_partner = huwburgerservicenummer,
             anr_partner = huwanummer,
             datum_huwelijk = huwdatumsluitinghuwelijkpartnerschap,
             datum_huwelijk_ind = huwdatumsluitinghuwelijkpartnerschapindicator,
             datum_scheiding = huwdatumontbindinghuwelijkpartnerschap,
             datum_scheiding_ind = huwdatumontbindinghuwelijkpartnerschapindicator,
             datum_omzetting = huwdatumomzettinghuwelijkpartnerschap,
             datum_omzetting_ind = huwdatumomzettinghuwelijkpartnerschapindicator) %>% 
      collect %>%
      remove_identical() %>%
      mutate(datum_scheiding = lubridate::ymd(datum_scheiding),
             datum_omzetting = lubridate::ymd(datum_omzetting),
             datum_huwelijk = lubridate::ymd(datum_huwelijk),
             datum_huwelijk = replace_na(datum_huwelijk, lubridate::ymd("1001-1-1")))
    
    out[out == ""] <- NA
    
  }
  
  tictoc::toc()
  
  out
}


#' Lees kinderen data
#' @export
read_kind <- function(con = NULL, brp_path = NULL, basename = "BZSKINQ00_pink.csv"){
  
  tictoc::tic("bzskin")
  
  if(!is.null(brp_path)){
    out <- read_csv_source(brp_path, basename) %>%
      janitor::clean_names() %>%
      mutate(kndgeboortedatum = lubridate::ymd(kndgeboortedatum))  
  } else {
    out <- tbl(con, in_schema("pseudo", "brp_bzskin")) %>% 
      collect %>%
      mutate(kndgeboortedatum = lubridate::ymd(kndgeboortedatum))
    
    out[out == ""] <- NA
  }
  
  tictoc::toc()
  
out
}




#' Lees BRP adres historie data
#' @export
read_bzsc58 <- function(con = NULL, brp_path = NULL, basename = "BZSC58Q00_pink.csv"){
  
  tictoc::tic("bzsc58")
  
  if(!is.null(brp_path)){
    
    raw_data <- read_csv_source(brp_path, basename) %>%
      mutate(adres = paste0(VBLHSTPOSTCODE, "_", 
                            VBLHSTHUISNUMMER, "_", 
                            VBLHSTHUISLETTER, "_", 
                            VBLHSTHUISNUMMERTOEVOEGING))
    
    out <- raw_data %>% 
      select(adres, 
             bsn = PRSBURGERSERVICENUMMER,
             anr = PRSANUMMER,
             
             gemeente_deel = VBLHSTGEMEENTEDEEL,
             woonplaats = VBLHSTWOONPLAATSNAAM,
             postcode = VBLHSTPOSTCODE,
             huisnummer = VBLHSTHUISNUMMER,
             huisletter = VBLHSTHUISLETTER,
             huisnummertoevoeging = VBLHSTHUISNUMMERTOEVOEGING,
             wijk_code = VBLHSTWIJKCODE,
             wijk_naam = VBLHSTWIJKOMSCHRIJVING,
             buurt_code_cipers = VBLHSTPLANALOGISCHEWIJKCODE,
             buurt_naam = VBLHSTPLANALOGISCHEWIJKOMSCHRIJVING,
             soort_pand_code = VBLHSTSOORTPANDCODE,
             soort_pand_omschrijving = VBLHSTSOORTPANDOMSCHRIJVING,
             
             gemeente_inschrijving = VBLHSTGEMEENTEVANINSCHRIJVINGOMSCHRIJVING,
             adres_functie = VBLHSTFUNCTIEADRES,
             datum_inschrijving = VBLHSTDATUMINSCHRIJVING,
             datum_inschrijving_ind = VBLHSTDATUMINSCHRIJVINGINDICATOR,
             datum_adres = VBLHSTDATUMAANVANGADRESHOUDING,
             datum_adres_ind = VBLHSTDATUMAANVANGADRESHOUDINGINDICATOR,
             #adres_buitenland = VBLHSTLANDADRESBUITENLANDOMSCHRIJVING,
             #datum_adres_buitenland = VBLHSTDATUMAANVANGADRESBUITENLAND,
             datum_nederland = VBLHSTDATUMVESTIGINGINNEDERLAND,
             datum_nederland_ind = VBLHSTDATUMVESTIGINGINNEDERLANDINDICATOR,
             land_ingeschreven = VBLHSTLANDVANWAARINGESCHREVENOMSCHRIJVING
      )  
    
    
  } else {
    
    out <- tbl(con, in_schema("pseudo", "brp_bzsc58")) %>% 
      select(bsn = prsburgerservicenummer,
             anr = prsanummer,
             
             gemeente_deel = vblhstgemeentedeel,
             woonplaats = vblhstwoonplaatsnaam,
             postcode = vblhstpostcode,
             huisnummer = vblhsthuisnummer,
             huisletter = vblhsthuisletter,
             huisnummertoevoeging = vblhsthuisnummertoevoeging,
             wijk_code = vblhstwijkcode,
             wijk_naam = vblhstwijkomschrijving,
             buurt_code_cipers = vblhstplanalogischewijkcode,
             buurt_naam = vblhstplanalogischewijkomschrijving,
             soort_pand_code = vblhstsoortpandcode,
             soort_pand_omschrijving = vblhstsoortpandomschrijving,
             
             gemeente_inschrijving = vblhstgemeentevaninschrijvingomschrijving,
             adres_functie = vblhstfunctieadres,
             datum_inschrijving = vblhstdatuminschrijving,
             datum_inschrijving_ind = vblhstdatuminschrijvingindicator,
             datum_adres = vblhstdatumaanvangadreshouding,
             datum_adres_ind = vblhstdatumaanvangadreshoudingindicator,
             #adres_buitenland = vblhstlandadresbuitenlandomschrijving,
             #datum_adres_buitenland = vblhstdatumaanvangadresbuitenland,
             datum_nederland = vblhstdatumvestiginginnederland,
             datum_nederland_ind = vblhstdatumvestiginginnederlandindicator,
             land_ingeschreven = vblhstlandvanwaaringeschrevenomschrijving
      ) %>%
      collect
    
    out <- mutate(out, adres = paste0(postcode, "_", 
                                      huisnummer, "_", 
                                      huisletter, "_", 
                                      huisnummertoevoeging)) %>%
      dplyr::relocate(adres)
    
    out[out == ""] <- NA
    
  }
  
  tictoc::toc()
  
  out
  
}



#' Lees ruwe BZSPRS data
#' @export
read_bzsprs <- function(con = NULL, brp_path = NULL, basename = "bzsprsq00_pink.csv"){
  
  tictoc::tic("bzsprs")
  
  if(!is.null(brp_path)){
    
    raw_data <- read_csv_source(brp_path, basename) %>%
      mutate(adres = paste0(VBLPOSTCODE, "_", 
                            VBLHUISNUMMER, "_", 
                            VBLHUISLETTER, "_", 
                            VBLHUISNUMMERTOEVOEGING))
    
    out <- dplyr::select(raw_data,
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
                          
                          # adres kolommen
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
    
  } else {
    
    out <- tbl(con, in_schema("pseudo", "brp_bzsprs")) %>% 
      select(bsn = prsburgerservicenummer,
             anr = prsanummer,
             anrouder1 = ou1anummer,  
             anrouder2 = ou2anummer,   
             geslacht_ouder1 = ou1geslachtsaanduidingcode,
             geslacht_ouder2 = ou2geslachtsaanduidingcode,
             geslacht = prsgeslachtsaanduidingcode,
             gezinsverhouding = prsgezinsverhouding,
             burgerlijke_staat = prsburgerlijkestaat,
             geboorte_land = prsgeboortelandomschrijving,
             geboorte_land_code = prsgeboortelandcode,
             geboorte_land_oud1_code = ou1geboortelandcode,
             geboorte_land_oud2_code = ou2geboortelandcode,
             geboorte_land_oud1_omschrijving = ou1geboortelandomschrijving,
             geboorte_land_oud2_omschrijving = ou2geboortelandomschrijving,
             datum_geboorte = prsgeboortedatum,
             datum_overlijden = ovldatumoverlijden,
             datum_inschrijving = vbldatuminschrijving,
             datum_inschrijving_vws = vwsdatuminschrijving,
             datum_adres = vbldatumaanvangadreshouding,
             gemeente_inschrijving = vblgemeentevaninschrijvingomschrijving,
             gemeente_inschrijving_vws = vwsgemeentevaninschrijvingomschrijving,
             gemeente_deel = vblgemeentedeel,
             woonplaats = vblwoonplaatsnaam, 
             postcode = vblpostcode,
             huisnummer = vblhuisnummer,
             huisletter = vblhuisletter,
             huisnummertoevoeging = vblhuisnummertoevoeging,
             wijk_code = vblwijkcode,
             wijk_naam = vblwijkomschrijving,
             buurt_code_cipers = vblplanalogischewijkcode,
             buurt_naam = vblplanalogischewijkomschrijving,
             soort_pand_code = vblsoortpandcode,
             soort_pand_omschrijving = vblsoortpandomschrijving,
             aanduidingverblijfstitelcode = vbtaanduidingverblijfstitelcode,
             verblijfstitelvervallen = vbtverblijfstitelvervallen,
             aanduidingverblijfstitelomschrijving = vbtaanduidingverblijfstitelomschrijving,
             ingangsdatumverblijfstitelindicator = vbtingangsdatumverblijfstitelindicator,
             ingangsdatumverblijfstitel = vbtingangsdatumverblijfstitel,
             datumeindeverblijfstitelindicator = vbtdatumeindeverblijfstitelindicator,
             datumeindeverblijfstitel = vbtdatumeindeverblijfstitel,
             gezag_minderjarige_indicatie = gzvindicatiegezagminderjarige,
             indicatie_curatele_register = gzvindicatiecurateleregister,
             prs_document_datum = prsdatumdocument,
             prs_document_datum_indicator = prsdatumdocumentindicator,
             prs_document_beschrijving = prsbeschrijvingdocument,
             gzv_document_beschrijving = gzvbeschrijvingdocument,
             gzv_document_datum_indicator = gzvdatumdocumentindicator,
             gzv_document_datum = gzvdatumdocument,
             ou1_document_datum = ou1datumdocument,
             ou1_document_datum_indicator = ou1datumdocumentindicator,
             ou1_document_beschrijving = ou1beschrijvingdocument,
             ou2_document_datum = ou2datumdocument,
             ou2_document_datum_indicator = ou2datumdocumentindicator,
             ou2_document_beschrijving = ou2beschrijvingdocument) %>%
      collect
    
    out <- mutate(out, adres = paste0(postcode, "_", 
                                      huisnummer, "_", 
                                      huisletter, "_", 
                                      huisnummertoevoeging)) %>%
      dplyr::relocate(adres)
    
    out[out == ""] <- NA
  }
  
  tictoc::toc()
  
  out
}


#' Lees stambestand (BZSPRSQ00)
#' @export
read_brpstam <- function(brp_bzsprs, adressen_inst, peil_datum, date_format = c("new","old")){
  
  data(buurt_key)
  data(geboorte_land_code_key)
  
  date_format <- match.arg(date_format)
  
  # 1. Leeftijden, datums.
  if(date_format == "old"){
    data <- brp_bzsprs %>%
      mutate(
        datum_adres = as.Date(datum_adres),
        datum_geboorte = as.Date(datum_geboorte),
        datum_overlijden = as.Date(datum_overlijden),
        datum_inschrijving = as.Date(datum_inschrijving),
        datum_inschrijving_vws = as.Date(datum_inschrijving_vws)
      )
  } else {
    data <- brp_bzsprs %>%
      mutate(
        datum_adres = ymd(datum_adres),
        datum_geboorte = ymd(datum_geboorte),
        datum_overlijden = as.Date(ymd_hms(datum_overlijden)),
        datum_inschrijving = ymd(datum_inschrijving),
        datum_inschrijving_vws = ymd(datum_inschrijving_vws)
      )
    
  }
  
  # extra kolommen
  data$leeftijd <- as.numeric(difftime(peil_datum, data$datum_geboorte, units = "weeks")) / 52
  data$minder18 <- data$leeftijd < 18
  data$minder23 <- data$leeftijd < 23
  
  # datum fixes
  data$datum_inschrijving[data$datum_inschrijving == as.Date("1001-01-01")] <- NA
  data$datum_adres[data$datum_adres == as.Date("1001-01-01")] <- NA
  
  # nog nodig?
  data <- mutate(data, 
                 datum_adres = coalesce(datum_adres, datum_inschrijving)
  )
  
  # 2. Institutioneel adres TRUE/FALSE
  data <- add_institutioneel_adres(data, adressen_inst)
  

  # 
  data <- as_tibble(data)
  
  # 3. Status houder categorieen
  data <- mutate(data,
                 statushouder_code = ifelse(aanduidingverblijfstitelcode %in% c(21,26), 
                                       1,2),
                 statushouder_omschrijving = ifelse(statushouder_code == 1, "Statushouder", "Geen statushouder"),
                 statushouderplus_code = ifelse(aanduidingverblijfstitelcode %in% c(21,25,26,27), 
                                           1,2),
                 statushouderplus_omschrijving = ifelse(statushouderplus_code == 1, "Statushouderplus", "Geen statushouderplus"),
                 ) %>%
    relocate(statushouder_code, .after = datumeindeverblijfstitel) %>%
    relocate(statushouder_omschrijving, .after = statushouder_code) %>%
    relocate(statushouderplus_code, .after = statushouder_omschrijving) %>%
    relocate(statushouderplus_omschrijving, .after = statushouderplus_code)
  
  # 4. Burgerlijke staat labels
  burgstaat_key <- tibble::tribble(
    ~burgerlijke_staat, ~burgerlijke_staat_omschrijving,
    "0","Onbekend",
    "1","Ongehuwd en nooit gehuwd geweest",
    "2","Gehuwd",
    "3","Gescheiden",
    "4","Weduwe/weduwnaar",
    "5","Partnerschap",
    "6","Partnerschap beëindigd",
    "7","Achtergebleven partner")
  
  # join, en zet kolom naast de code.
  data <- left_join(data, burgstaat_key, by = "burgerlijke_staat") %>% 
    relocate(burgerlijke_staat_omschrijving, .after = burgerlijke_staat)
  
  
  # 5. Gezinsverhouding label.
  gezinsverh_key <- tibble::tribble(
    ~gezinsverhouding, ~gezinsverhouding_omschrijving,
    "1", "Man of vrouw met inwonende echtgenoot",
    "2", "Man of vrouw met inwonende echtgenoot en kinderen" ,
    "3", "Man of vrouw met kind",
    "4", "Inwonende echtgeno(o)t(e)",
    "5", "Inwonend ongehuwd kind",
    "6", "Niet in gezinsverband levend/alleenstaand")
  
  data <- left_join(data, gezinsverh_key, by = "gezinsverhouding") %>% 
    relocate(gezinsverhouding_omschrijving, .after = gezinsverhouding)
  
  
  # 6. 
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
  
  # 7. Voeg id kolom toe om volgorde te behouden
  data <- data %>% 
    mutate(id = 1:nrow(.)) %>%
    dplyr::relocate(id)
  
  
  data
}

#' Buurt/wijk codes toevoegen
#' @export
add_buurt_wijk_columns <- function(data){
  
  # foute levering 2022-01-20
  if(length(unique(data$buurt_code_cipers)) < 30){
    
    data(buurt_koppel_fix_pink)
    data$buurt_code_cipers <- NULL
    
    data <- left_join(data, select(buurt_koppel_fix_pink, -buurt_code_cbs), by = "buurt_naam")
    
  }
  
  data(buurt_key)
  
  data <- left_join(data, buurt_key, by = "buurt_code_cipers") %>%
    mutate(buurt_code = na_if(buurt_code,"0"),
           wijk_code=  na_if(wijk_code,"000")) %>%
    relocate(buurt_code, .after = buurt_naam) %>%
    mutate(buurt_code_cbs  = paste0("BU0228", buurt_code)) %>%
    relocate(buurt_code_cbs, .after = buurt_code) %>%
    mutate(wijk_code_cbs  = paste0("WK0228", substr(wijk_code,2,3))) %>%
    relocate(wijk_code_cbs, .after = wijk_code) %>%
    relocate(buurt_code_cipers, .after = buurt_code)
  
  data$wijk_code_cbs[data$wijk_code_cbs == "WK0228"] <- NA
  data$wijk_code_cbs[data$wijk_code_cbs == "WK0228NA"] <- NA
  data$buurt_code_cbs[data$buurt_code_cbs == "BU0228NA"] <- NA
  
data
}



#' Historie data, bijgewerkt
#' @export
read_historie <- function(brp_bzsc58, brpstam, date_format = c("new","old")){
  
  date_format <- match.arg(date_format)
  
  # Koppeltabel geboorte/overlijden
  levenstabel <- brpstam %>%
    select(anr, anrouder1, anrouder2, datum_geboorte, datum_overlijden) %>%
    distinct
  
  if(date_format == "old"){
    out <- brp_bzsc58 %>%
      mutate(
        datum_inschrijving = as.Date(datum_inschrijving),
        datum_adres = as.Date(datum_adres)
        #datum_adres_buitenland = lubridate::ymd(datum_adres_buitenland)
        #datum_adres = coalesce(datum_adres, datum_adres_buitenland)
      )  
  } else {
    out <- brp_bzsc58 %>%
      mutate(
        datum_inschrijving = ymd(datum_inschrijving),
        datum_adres = ymd(datum_adres)
      )
  }
   
  out %>%
    remove_identical() %>%
    left_join(levenstabel, by = "anr")
  
}



#-------------------------------------------------------------------------------#
#utils


add_institutioneel_adres <- function(data, adressen_inst){
  
  if(is.null(adressen_inst))return(data)
  
  # 5. Institutionele adressen : zoals in de syntax van Ilse, join
  # op postcode+huisnummer als geen huisletter, en op 
  # postcode+huisnummer+huisletter als wel huisletter.
  # (omslachtige maar veilige join)
  inst1 <- filter(adressen_inst, !is.na(huisnummerletter)) %>%
    mutate(adres = paste0(postcode, "_", huisnummer, "_", huisnummerletter))
  inst2 <- filter(adressen_inst, is.na(huisnummerletter)) %>%
    mutate(adres = paste0(postcode, "_", huisnummer))
  
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






# Filter huwelijk tabel op huidige huwelijken, die nog niet beeindigd zijn.
current_huwelijk <- function(data, peildatum){
  
  data %>%
    filter(datum_huwelijk < peildatum,
           is.na(datum_scheiding) | datum_scheiding > peildatum
    ) %>%
    #select(anr, anr_partner) %>%
    filter(!is.na(anr_partner))
  
}








