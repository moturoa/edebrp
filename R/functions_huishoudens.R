



#' Huishouden data samenvatting
#' @details Werkt met output van bepaal_huishoudens
#' @export
brp_summary <- function(data){
  tibble::tibble(
    peil_datum = unique(data$peil_datum)[1],
    n_inwoners = length(unique(data$anr)),
    n_adressen = length(unique(data$adres)),
    n_huishoudens = length(unique(data$huishouden)),
    n_verhuisde_wezen = sum(data$adres != data$adres_huishouden),
    ave_n_huwelijken_persoon = mean(data$aantal_huwelijken),
    ave_n_kinderen_persoon = mean(data$aantal_kinderen),
    p_getrouwd = table_frac("getrouwd", data),
    p_getrouwd_zelfde_adres = table_frac("getrouwd_zelfde_adres", data),
    ave_hh_n_personen = nrow(data)/length(unique(data$huishouden)),
    ave_persoon_ouders_adres = table_frac("kind", data),
    ave_persoon_kinderen_adres = table_frac("ouder", data)
  )
}


leeftijd_categorie_5jr <- function(leeftijd){
  
  v <- seq(0,100, by= 5)
  
  d_lab <- as.data.frame(cbind(v, c(v[2:length(v)],200))) %>%
    setNames(c("x","y")) %>%
    mutate(y = y-1)
  labs <- with(d_lab, paste(x,"t/m", y,"jaar"))
  labs[length(labs)] <- "100 jaar en ouder"
  
  labs[findInterval(leeftijd,v)]
  
}

leeftijdcategorie_senior <- function(leeftijd){
  
  case_when(
    
    leeftijd >= 80 ~ "80 jaar en ouder (dubbele vergrijzing)",
    leeftijd >= 75 ~ "75 jaar en ouder (vergrijzing2)",
    leeftijd >= 65 ~ "65 jaar en ouder (vergrijzing1)",
    TRUE ~ "0 - 64 jaar"
  )
  
}




#------BRP Tijdmachine -----

#' BRP tijdmachine, ouder CIPERS levering
#' @param historie Dataframe ingelezen met read_historie
#' @export
brp_tijdmachine_cipers <- function(historie, brpstam, peil_datum){
  
  stopifnot(is.Date(peil_datum))
  
  # maak tabel met datum eerste inschrijving persoon
  tbl_datum_inschrijving <- filter(historie, datum_inschrijving < peil_datum) %>%
    mutate(datum_inschrijving = lubridate::ymd(datum_inschrijving)) %>%
    group_by(anr) %>%
    mutate(datum_inschrijving_eerst = filter_last_leading(gemeente_inschrijving, 
                                                          datum_inschrijving, 
                                                          val = "Ede")) %>%
    ungroup %>%
    select(anr, datum_inschrijving_eerst) %>%
    distinct
  
  data <- left_join(brpstam, tbl_datum_inschrijving, by = "anr")
  
  data <- data %>%
    mutate(datum_brp_tijdmachine = peil_datum,
           datum_geboorte = lubridate::ymd(datum_geboorte),
           datum_overlijden = lubridate::ymd(datum_overlijden),
           datum_inschrijving = lubridate::ymd(datum_inschrijving),
           datum_inschrijving_vws = lubridate::ymd(datum_inschrijving_vws),
           datum_inschrijving_eerst = dplyr::coalesce(datum_inschrijving_eerst, datum_inschrijving))
  
  data <- data %>%
    mutate(
      verhuisd = !is.na(datum_inschrijving_vws) & datum_inschrijving_vws <= !!peil_datum,
      overleden = !is.na(datum_overlijden) & datum_overlijden <= !!peil_datum,
      geboren = datum_geboorte <= !!peil_datum,
      ingeschreven = datum_inschrijving_eerst <= !!peil_datum) %>% 
    filter(!verhuisd,
           !overleden,
           ingeschreven,
           geboren,
           gemeente_inschrijving != "Onbekend")
  
  
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
             adres != "NA_NA_NA_NA",
             datum_adres < peil_datum) %>%
      group_by(anr) %>%
      filter(datum_adres == max(datum_adres)) %>%
      select(anr, adres, datum_adres, datum_inschrijving,
             gemeente_deel,woonplaats,postcode,huisnummer,huisletter,huisnummertoevoeging,wijk_code,
             wijk_naam,buurt_code_cipers,buurt_naam,soort_pand_code,soort_pand_omschrijving) %>%
      distinct(anr, .keep_all = TRUE) %>%
    ungroup()
      
    data <- left_join(select(data, -datum_inschrijving, -datum_adres,
                             -gemeente_deel,-woonplaats,-postcode,-huisnummer,-huisletter,
                             -huisnummertoevoeging,-wijk_code,-wijk_naam,-buurt_code_cipers,
                             -buurt_naam,-soort_pand_code,-soort_pand_omschrijving) %>% 
                        rename(adres_cur = adres), 
                      adres_historie, by = "anr") %>%
            mutate(adres = coalesce(adres, adres_cur)) %>%
            select(-adres_cur)
                      
  
  data 
}

#' BRP tijdmachine
#' @export
brp_tijdmachine <- function(historie, brpstam, peil_datum){
  
  stopifnot(is.Date(peil_datum))
  
  data <- brpstam %>%
    mutate(datum_brp_tijdmachine = peil_datum,
      overleden = !is.na(datum_overlijden) & datum_overlijden <= !!peil_datum,
      geboren = is.na(datum_geboorte) | datum_geboorte <= !!peil_datum) %>%
    filter(!overleden, geboren)
  
  # Laatste adres gegevens net voor de peil datum (1 rij per persoon)
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
    filter(anr %in% !!data$anr,
           datum_adres < peil_datum) %>%
    group_by(anr) %>%   # hier laatste adres voor de peildatum vinden per persoon
    filter(datum_adres == max(datum_adres)) %>%
    select(anr, adres, datum_adres, datum_inschrijving,gemeente_inschrijving,
           gemeente_deel,woonplaats,postcode,huisnummer,huisletter,huisnummertoevoeging,wijk_code,
           wijk_naam,buurt_code_cipers,buurt_naam,soort_pand_code,soort_pand_omschrijving) %>%
    distinct(anr, .keep_all = TRUE) %>%
    ungroup()
  
  data <- left_join(select(data, -datum_inschrijving, -datum_adres,-gemeente_inschrijving,
                           -gemeente_deel,-woonplaats,-postcode,-huisnummer,-huisletter,
                           -huisnummertoevoeging,-wijk_code,-wijk_naam,-buurt_code_cipers,
                           -buurt_naam,-soort_pand_code,-soort_pand_omschrijving) %>% 
                      rename(adres_cur = adres), 
                    adres_historie, by = "anr") %>%
    mutate(adres = coalesce(adres, adres_cur)) %>%
    select(-adres_cur) %>%
    filter(gemeente_inschrijving == "Ede",
           adres != "___")   # ontbrekende adressen, we gaan ervan uit dat deze personen niet in Ede wonen (?!)
  
  
  data 
}


# Tel aantal kinderen per persoon op een bepaalde peildatum
current_kinderen <- function(data, peil_datum){
  
  data %>%
    filter(kndgeboortedatum < peil_datum) %>%
    count(prsanummer, name = "aantal_kinderen") %>%
    rename(anr = prsanummer)
  
}


#' Hoofd functie om 'huishouden' kolom toe te voegen.
#' @export
bepaal_huishoudens <- function(peil_datum, 
                               brpstam, historie, huwelijk, kind, adressen_inst,
                               verhuis_wezen = TRUE, 
                               ethniciteit = TRUE,
                               buurt_wijk_codes = TRUE,
                               format = c("new","old"),
                               ...){
  
  format <- match.arg(format)
  
  # Filter op peildatum
  if(format == "old"){
    brp <- brp_tijdmachine_cipers(historie, brpstam, peil_datum)  
  } else {
    brp <- brp_tijdmachine(historie, brpstam, peil_datum)  
  }
  
  
  # Start punt 'adres voor bepaling huishouden': adres_huishouden.
  # Dit kan later afwijken omdat 'wezen' verhuisd worden
  brp <- mutate(brp, adres_huishouden = adres)
  
  # Aantal huwelijken.
  n_huwelijk <- count(huwelijk, anr, name = "aantal_huwelijken")
  
  # Huwelijk data. 
  huwelijk_cur <- huwelijk %>%
    current_huwelijk(peil_datum) %>%
    select(anr, anr_partner, datum_huwelijk)
  
  brp <- left_join(brp, huwelijk_cur, by = "anr")
  #pm_log("Huwelijken gefilterd.")
  
  brp <- left_join(brp, n_huwelijk, by = "anr") %>%
    mutate(aantal_huwelijken = replace_na(aantal_huwelijken, 0))
  
  # Aantal kinderen (alleen voor beschrijvende kolom)
  n_kinderen <- current_kinderen(kind, peil_datum)
  
  brp <- left_join(brp, n_kinderen, by = "anr") %>%
    mutate(aantal_kinderen = replace_na(aantal_kinderen, 0))
  
  # Onafhankelijke kinderen verhuizen naar ouders
  # Dit zijn kinderen van wie de ouders wel in Ede wonen, maar ze wonen bij bv. pleegouders.
  # Deze kinderen horen wel bij de ouders ingedeeld te worden voor het huishouden.
  if(verhuis_wezen){
    brp <- verhuis_wezen(brp)  
  }
  

  # Persoon classificatie
  # Voeg enkele descriptors toe (ouder/kind/broerzus etc.)
  brp <- persoon_classificatie(brp)
  
  # Pas huishouden algorithme toe op elk 'adres_huishouden'.
  # Maakt nieuwe kolom 'huishouden', het anr voor dit huishouden.
  brp <- group_by(brp, adres_huishouden) %>%
    mutate(huishouden = huishouden_functie(
      adres_huishouden,
      anr, 
      n_personen_adres,
      institutioneel_adres, 
      anrouder1, 
      anrouder2,
      anr_partner,
      ouder,
      kind, 
      getrouwd_zelfde_adres,
      broerzus,
      leeftijd, 
      minder18, 
      minder23,
      datum_adres,
      ...)) %>%
    ungroup
  

  # Laatste 'overblijvers' vormen eigen huishouden.
  brp$huishouden_overgebleven_persoon <- brp$huishouden == ""
  brp$huishouden[brp$huishouden == ""] <- brp$anr[brp$huishouden == ""]
  
  
  # Huishouden categorieen, n_personen_huishouden updaten.
  brp <- huishouden_categorieen(brp)
  
  # verwijder kolommen die niet nodig zijn in output.
  # (doen we hier omdat deze functie ook in ssd_join_brp wordt gebruikt)
  brp <- drop_columns(brp,  
                c("woonplaats", "gemeente_inschrijving","gemeente_inschrijving_vws",
                  "datum_inschrijving_vws","ingangsdatumverblijfstitelindicator", 
                  "datumeindeverblijfstitelindicator", "prs_document_datum_indicator", 
                  "gzv_document_datum_indicator", "ou1_document_datum_indicator", 
                  "ou2_document_datum_indicator", "datum_inschrijving_eerst"))
  
  # Laatste bewerkingen
  brp <- mutate(brp,
                leeftijd = floor(leeftijd),
                minder18 = ifelse(minder18, "<18 Jaar", ">18 Jaar"),
                minder23 = ifelse(minder23, "<23 Jaar", ">23 Jaar"),
                institutioneel_adres = ifelse(institutioneel_adres, "Inst. adres", "Geen inst. adres"),
                verhuisde_minderjarige = ifelse(verhuisde_minderjarige, "Verhuisde minderjarige", "Geen verhuisde minderjarige"),
                ouder = ifelse(ouder, "Heeft kinderen op adres", "Heeft geen kinderen op adres"),
                kind = ifelse(kind, "Heeft ouders op adres", "Heeft geen ouders op adres"),
                broerzus = ifelse(broerzus, "Heeft broer/zus op adres", "Heeft geen broer/zus op adres"),
                getrouwd = ifelse(getrouwd, "Getrouwd", "Niet getrouwd"),
                getrouwd_zelfde_adres = ifelse(getrouwd_zelfde_adres, "Getrouwd, partner zelfde adres","Niet getrouwd of ander adres"),
                leeftijd_categorie_5jr = leeftijd_categorie_5jr(leeftijd),
                leeftijdcategorie_senior = leeftijdcategorie_senior(leeftijd),
                beroepsbevolking_64 = ifelse(leeftijd >= 15 & leeftijd <= 64, 
                                             "Potentiele beroepsbevolking (65-)",
                                             "Geen beroepsbevolking"),
                beroepsbevolking_74 = ifelse(leeftijd >= 15 & leeftijd <= 74, 
                                             "Potentiele beroepsbevolking (75-)",
                                             "Geen beroepsbevolking")
  ) %>% 
    drop_columns(c("verhuisd","geboren","overleden","ingeschreven", "huishouden_overgebleven_persoon")) %>%
    rename(peil_datum = datum_brp_tijdmachine) %>%
    relocate(peil_datum, .after = "id")
  
  # Extra kolommen
  if(ethniciteit){
    # dit hoeft niet van te voren, kan ook achteraf voor een dataset met kolommen:
    # geboorte_land_oud1_code, geboorte_land_oud2_code, geslacht_ouder1, geslacht_ouder2
    # geboorte_land_code
    brp <- add_ethniciteit_columns(brp)

  }
  
  # Extra buurt/wijk codes, namen (deels Ede specifiek)
  if(buurt_wijk_codes){
    brp <- add_buurt_wijk_columns(brp)
  }
  
  return(brp)  
}






#----- Wezen verhuizen -----

verhuis_wezen <- function(data){
  
  #if uithuizige_minderjarigen_bij_ouder
  wees <- group_by(data, adres) %>%
    mutate(geenouders = minder18 & (!anrouder1 %in% anr & !anrouder2 %in% anr)) %>%
    ungroup %>%
    filter(!institutioneel_adres, geenouders)
  
  wees_verhuis <- filter(wees, 
                           anrouder1 %in% !!data$anr | 
                           anrouder2 %in% !!data$anr)
  wees_verhuis_anr <- wees_verhuis$anr
  ii <- match(wees_verhuis_anr, data$anr)
  
  for(i in ii){
    
    if(data$anrouder1[i] %in% data$anr){
      ouder_adres <- unique(data$adres_huishouden[data$anr == data$anrouder1[i]])
      data$adres_huishouden[i] <- ouder_adres
    } else if(data$anrouder2[i] %in% data$anr){
      ouder_adres <- unique(data$adres_huishouden[data$anr == data$anrouder2[i]])
      data$adres_huishouden[i] <- ouder_adres
    }
    
  }
  
  mutate(data,
         verhuisde_minderjarige = adres != adres_huishouden)
}




#----- Persoon classificatie ----

persoon_classificatie <- function(data){
  
  group_by(data, adres_huishouden) %>%
    mutate(
      n_personen_adres = n(),
      ouder = anr %in% anrouder1 | anr %in% anrouder2, # heeft deze persoon kinderen op dit adres?
      kind = anrouder1 %in% anr | anrouder2 %in% anr,  # heeft deze persoon ouder(s) op dit adres?
      broerzus = heeft_broerzus(anrouder1, anrouder2), # deelt deze persoon ouders met een andere persoon op dit adres?
      getrouwd = !is.na(anr_partner),                  # a-nummer partner ingevuld?
      getrouwd_zelfde_adres = anr_partner %in% anr     # partner op hetzelfde adres?
    ) %>%
    ungroup
  
}



#----- Bepalen huishoudens op basis van familie/andere links ----

# gegeven vectors als input (voor 1 adres_huishouden), return
# een Anr dat het 'huishouden' wordt.
huishouden_functie <- function(adres_huishouden,
                               anr, 
                               n_personen_adres,
                               institutioneel_adres, 
                               anrouder1, 
                               anrouder2, 
                               anr_partner, 
                               ouder, 
                               kind, 
                               getrouwd_zelfde_adres,
                               broerzus,
                               leeftijd, 
                               minder18, 
                               minder23,
                               datum_adres,
                               
                               leeftijd_delta_koppel = 8,
                               datum_adres_koppel = 15,
                               verbose = FALSE
                               ){
  
  
  print_step <- function(txt){
    if(verbose){
      print(txt)
    }
  }
  
  # 1 persoon.
  if(n_personen_adres[1] == 1){
    return(anr[1])
  }
  
  # institutioneel huishouden: ieder voor zich.
  if(institutioneel_adres[1]){
    return(anr)
  }
  
  # Exact twee personen, geen ouder/kind relaties, niet getrouwd.
  if(n_personen_adres[1] == 2 & !any(kind) & !any(getrouwd_zelfde_adres)){
    
    print_step("n_personen_adres[1] == 2 & !any(kind) & !any(getrouwd_zelfde_adres)")
    
    # absoluut leeftijd verschil
    similar_age <- leeftijd_delta(leeftijd) < leeftijd_delta_koppel
    similar_verhuisdatum <- datum_delta(datum_adres) < datum_adres_koppel
    
    # Koppel op basis van leeftijd of verhuisdatum
    if(similar_age | similar_verhuisdatum){
      
      return(anr[which.max(leeftijd)])
      
    } else {
      
      return(anr)
    }
    
  }
  
  
  # Iedereen is ongekoppeld.
  if(all(!kind & !ouder & !broerzus & !getrouwd_zelfde_adres)){
    
    print_step("all(!kind & !ouder & !broerzus & !getrouwd_zelfde_adres)")
    
    return(anr)
    
  }
  
  
  # Maak koppelingen op basis van ouder/kind en partner relaties.
  huishouden <- vector("character", length = length(anr))
  for(i in seq_along(anr)){
    
     # Elk kind krijgt huishouden oudste ouder.
     if(kind[i]){
       print_step(paste("kind", anr[i], "krijgt anr oudste ouder"))
       
       i_parent <- which(anr %in% c(anrouder1[i], anrouder2[i]))
       i_oldest_parent <- i_parent[which.max(leeftijd[i_parent])]
       huishouden[i] <- anr[i_oldest_parent]
     }
  }
  
  
  # Loop nu door de ouders.
  for(i in seq_along(anr)){
    
    if(ouder[i]){

      i_kind <- which(anrouder1 %in% anr[i] | anrouder2 %in% anr[i])
      i_parent <- which(ouder)
      
      # Enige ouder. Dan is dit de ouder naar wie alle kinderen gaan verwijzen.
      # Dan is de ouder dus het huishouden.
      if(length(i_parent) == 1){
        
        print_step("enige ouder dit adres")
        
        huishouden[i] <- anr[i]
        
      } else if(length(i_parent) == 2){
        
        print_step("beide ouders dit adres")
        
        # Beide ouders wonen op dit adres.
        # Als boven: oudste ouder is het huishouden. De andere ouder krijgt dus hetzelfde huishouden.
        i_oldest_parent <- i_parent[which.max(leeftijd[i_parent])]
        huishouden[i] <- anr[i_oldest_parent]
        
      } else {  
        
        # 3 of meer ouders op dit adres.
        # 3e generatie huishouden / overige situatie / commune / camping / etc.
        
        # Wie zijn de kinderen van deze persoon?
        i_kind <- which(anrouder1 == anr[i] | anrouder2 == anr[i])
        
        # Als 1 kind, geef deze ouder het huishouden van dit kind (vaak: zichzelf)
        if(length(i_kind) == 1){
          print_step("3e generatie, 1 kind")
          huishouden[i] <- huishouden[i_kind]
        } else {
          print_step("3e generatie, meerdere kinderen, zelfde huishouden")
          
          # Anders, als alle kinderen hetzelfde huishouden hebben, gebruik dit.
          hh_kind <- huishouden[i_kind]
          if(length(unique(hh_kind)) == 1){
            huishouden[i] <- hh_kind[1]
          } else {
            # Speciaal geval, wordt beneden verder ingevuld.
          }
          
        }
        
      }

    }
    
  }
  
  # 3e generatie huishoudens
  if(any(ouder & kind)){
    
    print_step("3e generatie huishouden")
    
    huishouden[huishouden == ""] <- anr[huishouden == ""]
    
    # De 'middleman', is de persoon die zowel ouder als kind is.
    i_middleman <- which(ouder & kind)
    anr_middleman <- anr[i_middleman]
    
    # Zeer lastige gevallen: meerdere middlemen.
    # Gebruik de oudste middleman. Dit lost nog niet alles op.
    if(length(anr_middleman) > 1){
      
      age_middlemen <- leeftijd[i_middleman]
      i_middleman <- i_middleman[which.max(age_middlemen)]
      anr_middleman <- anr_middleman[which.max(age_middlemen)]
    }
    
    # Ouders van de middleman
    i_middleman_parents <- which(anr %in% c(anrouder1[i_middleman], anrouder2[i_middleman]))
    
    # partner van de ouders van de middleman
    i_middleman_parent_partner <- which(anr %in% anr_partner[i_middleman_parents])
    
    # Kinderen van de middleman
    i_middleman_kids <- which(anrouder1 == anr_middleman | anrouder2 == anr_middleman)

    # Andere (niet-getrouwde) ouder van een kind van de middleman
    i_middleman_other_parent <- which(anr %in% unique(c(anrouder1[i_middleman_kids],
                                                        anrouder2[i_middleman_kids])))
    
    # partner van middleman
    i_partner <- which(anr == anr_partner[i_middleman])
    
    # kinderen van partner van middleman
    anr_partner_mm <- anr[i_partner]
    i_partner_kids <- which(anrouder1 == anr_partner_mm | anrouder2 == anr_partner_mm)

    # broers/zussen van de middleman
    i_sibl <- which(anrouder1 == anrouder1[i_middleman] |
                      anrouder2 == anrouder2[i_middleman])
    
    i_relatives <- unique(c(i_middleman, 
                            i_partner,
                            i_partner_kids, 
                            i_middleman_parent_partner,
                            i_sibl,
                            i_middleman_parents, 
                            i_middleman_kids, 
                            i_middleman_other_parent))

    huishouden[i_relatives] <- anr_middleman
    
  }


  # getrouwde personen koppelen
  if(any(getrouwd_zelfde_adres)){
    
    for(i in seq_along(anr)){
      
      if(getrouwd_zelfde_adres[i]){
        
        i_partner <- which(anr == anr_partner[i])
        i_koppel <- c(i, i_partner)
        
        # Speciale situatie: 1 ouder, 1 getrouwde persoon, 1 of meer kinderen.
        # In dit geval is de ouder het hoofd van het huishouden, partner wordt gekoppeld.
        if(sum(ouder) == 1){
          huishouden[i] <- huishouden[which(ouder)]
        
          # Huishouden was nog niet gevuld, gebruik anummer.
          if(huishouden[i] == ""){
            huishouden[i] <- anr[which(ouder)]
          }
        } else {
          
          # In andere gevallen (0, 2 ouders) is de oudste persoon hoofd huishouden.
          i_oldest_in_couple <- i_koppel[which.max(leeftijd[i_koppel])]
          huishouden[i] <- huishouden[i_oldest_in_couple]  
          
          # Huishouden was nog niet gevuld, gebruik anummer.
          if(huishouden[i] == ""){
            huishouden[i] <- anr[i_oldest_in_couple]
          }
        }
        
      }
      
    }
    
  }
  
  # re-fix: geef kinderen het huishouden van hun oudste ouder.
  # (dit is nodig vanwege 3e generatie of andere huishouden aanpassing)
  if(any(kind)){
    
    for(i in seq_along(anr)){
      
      if(kind[i]){
        
        print_step("kind herkoppelen")
        
        i_parent <- which(anr %in% c(anrouder1[i], anrouder2[i]))
        i_oldest_parent <- i_parent[which.max(leeftijd[i_parent])]
        
        # kind krijgt huishouden oudste ouder
        huishouden[i] <- huishouden[i_oldest_parent]
        
        # en voor de zekerheid, beide ouders ook 
        huishouden[i_parent] <- huishouden[i_oldest_parent]
      }
      
    }
  }
  
  # Broer/zussen koppelen (ouders wonen elders)
  if(any(broerzus) & !any(kind)){
    
    for(i in seq_along(anr)){
      
      if(broerzus[i]){
        
        i_sibl <- which(anrouder1 == anrouder1[i] |
                          anrouder2 == anrouder2[i])
        
        i_oldest_sibling <- i_sibl[which.max(leeftijd[i_sibl])]
        huishouden[i] <- anr[i_oldest_sibling]
          
      }
      
    }
  }
  
  # Half-broers/half-zussen koppelen, als nog niet gekoppeld.
  if(any(broerzus) & any(huishouden == "")){
    
    for(i in seq_along(anr)){
     
      if(huishouden[i] == "" & broerzus[i]){
        
        i_sibl <- which(anrouder1 == anrouder1[i] |
                          anrouder2 == anrouder2[i] | 
                          anrouder1 == anrouder2[i] | 
                          anrouder2 == anrouder1[i] )
        i_sibl <- setdiff(i_sibl, i)
        huishouden[i] <- huishouden[i_sibl[1]]
        
      }
      
       
    }
    
  }
  
  # Enkele volwassene koppelen aan enkele oudere, als leeftijd of verhuisdatum ca. gelijk.
  if(sum(ouder) == 1){

    if(any(huishouden == "")){
    
      mis_age <- leeftijd[huishouden == ""]
      if(sum(mis_age > 18) == 1){
        
        i_onafh <- which(huishouden == "" & leeftijd > 18)
        i_ouder <- which(ouder)
        i_koppel <- c(i_onafh, i_ouder)
        
        similar_age <- leeftijd_delta(leeftijd[i_koppel]) < 15
        similar_move_date <- datum_delta(datum_adres[i_koppel]) < 15
        
        if(similar_age | similar_move_date){
          
          huishouden[i_onafh] <- huishouden[i_ouder]
          
        } 
        
      }
      
    }
    
  }
  
  # Ongekoppelde minderjarigen koppelen.
  if(any(minder18 & !kind)){
    
    if(any(ouder)){
      
      if(sum(ouder) > 2){
        #print(paste("moeilijke situatie:", adres_huishouden))
      }
      
      i_pleeg <- which(minder18 & !kind)
      i_parent <- which(ouder)
      i_oldest_parent <- i_parent[which.max(leeftijd[i_parent])]
      huishouden[i_pleeg] <- huishouden[i_oldest_parent]
      
    } else {
      #print(paste("moeilijke situatie:", adres_huishouden))
    }
    
  }
  

  return(huishouden)
}





#----- Huishouden categorieen ----



huishouden_categorieen <- function(data){
  
  data <- group_by(data, adres_huishouden) %>%
    mutate(
      n_huishouden_adres = length(unique(huishouden))
    ) %>%
    ungroup
  
  data <- group_by(data, huishouden) %>%
    mutate(n_personen_huishouden = n(),
           huishouden_categorie_1 = huishouden_categorie(n_personen_huishouden, 
                                                         ouder,kind,broerzus, leeftijd, 
                                                         anr, anr_partner,
                                                         institutioneel_adres,level = "18"),
           huishouden_categorie_2 = huishouden_categorie(n_personen_huishouden, 
                                                         ouder,kind,broerzus, leeftijd, 
                                                         anr, anr_partner,
                                                         institutioneel_adres,level = "23")) %>%
    ungroup
  
  
  return(data)
}




# Indeling 1:
#   
#   Alleenstaande
# Paar zonder kind
# Paar met inwonende kinderen onder de 18 jaar
# Paar met inwonende kinderen van 18 jaar en ouder
# Paar met inwonende kinderen zowel onder de 18 als 18 jaar en ouder
# Eenoudergezin met inwonende kinderen onder de 18 jaar
# Eenoudergezin met inwonende kinderen van 18 jaar en ouder
# Eenoudergezin met inwonende kinderen zowel onder de 18 als 18 jaar en ouder
# Persoon in institutionele huishouden
# Huishoudens met meerdere generaties achter één voordeur
# Overig huishouden
# 
# 
# Indeling 2:
#   
#   Alleenstaande
# Paar zonder kind
# Paar met inwonende kinderen onder de 23 jaar
# Paar met inwonende kinderen van 23 jaar en ouder
# Paar met inwonende kinderen zowel onder de 23 als 23 jaar en ouder
# Eenoudergezin met inwonende kinderen onder de 23 jaar
# Eenoudergezin met inwonende kinderen van 23 jaar en ouder
# Eenoudergezin met inwonende kinderen zowel onder de 23 als 23 jaar en ouder
# Persoon in institutionele huishouden
# Huishoudens met meerdere generaties achter één voordeur
# Overig huishouden

huishouden_categorie <- function(n_personen, ouder, kind, broerzus, leeftijd, 
                                 anr, anr_partner,
                                 institutioneel_adres,
                                 level = c("18","23")){
  level <- match.arg(level)
  
  minder18 <- leeftijd < 18
  minder23 <- leeftijd < 23
  
  # Personen onder de 18 als kind bestempelen.
  kind[minder18] <- TRUE
  
  if(all(institutioneel_adres)){
    return("Persoon in institutionele huishouden")
  }
  
  if(n_personen[1] == 1){
    return("Alleenstaande")
  }
  
  if(!any(kind)){
    if(n_personen[1] == 2){
      return("Paar zonder kind")
    } else {
      return("Overig huishouden")
    }
  }
  
  if(any(kind & ouder) |   # 'middleman' in 3e generatie huishouden
     any(kind & (!is.na(anr_partner) & anr_partner %in% anr))){   # dubbel gescheiden gekoppeld
    
    return("Huishoudens met meerdere generaties achter één voordeur")
    
  }
  
  
  # 'kind' label aanpassen. Dit geldt ook voor half-broers/zussen.
  kind[broerzus] <- TRUE
  
  # >2 ouders (of in elk geval, in dit huishouden, de volwassenen)
  ouder_label <- ifelse(sum(!kind) == 1, "Eenoudergezin", "Paar")
  
  
  if(sum(kind) >= 1){
    
    if(level == "18"){
      min18 <- any(kind & minder18)
      plus18 <- any(kind & !minder18)
      
      if(min18 & !plus18){
        return(paste0(ouder_label," met inwonende kinderen onder de 18 jaar"))
      }
      if(!min18 & plus18){
        return(paste0(ouder_label," met inwonende kinderen boven de 18 jaar"))
      }
      if(min18 & plus18){
        return(paste0(ouder_label," met inwonende kinderen zowel onder de 18 als 18 jaar en ouder"))
      }
      
    }
    
    if(level == "23"){
      min23 <- any(kind & minder23)
      plus23 <- any(kind & !minder23)
      
      if(min23 & !plus23){
        return(paste0(ouder_label," met inwonende kinderen onder de 23 jaar"))
      }
      if(!min23 & plus23){
        return(paste0(ouder_label," met inwonende kinderen boven de 23 jaar"))
      }
      if(min23 & plus23){
        return(paste0(ouder_label," met inwonende kinderen zowel onder de 23 als 23 jaar en ouder"))
      }
      
    }
  }
  
  # "else"
  return("Overig huishouden")
}






heeft_broerzus <- function(anrouder1, anrouder2){
  
  a1 <- anrouder1[!is.na(anrouder1)]
  a2 <- anrouder2[!is.na(anrouder2)]
  a <- c(a1, a2)
  a <- a[duplicated(a)]  # Anr van ouder1 of ouder2 die 2 of meer keer voorkomt
  
  anrouder1 %in% a | anrouder2 %in% a
  
}










