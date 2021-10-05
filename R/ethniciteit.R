#' Voeg ethniciteit bepalingen toe
#' @export
add_ethniciteit_columns <- function(data){
  
  # Hulp tabellen
  data(land_code_labels)
  data(geboorte_land_cat_key)
  data(geboorte_land_cat_labels)
  data(geboorte_land_hercodering)
  
  # Stap 1: geboorte land moeder, vader, onbekend.
  data <- mutate(data, 
                 geboorte_land_moeder_code = ifelse(geslacht_ouder1 == "V", geboorte_land_oud1_code, geboorte_land_oud2_code),
                 geboorte_land_vader_code = ifelse(geslacht_ouder1 == "M", geboorte_land_oud1_code, geboorte_land_oud2_code),
                 
                 geboorte_land_code = case_when(
                   geboorte_land_code == "" ~ NA_character_,
                   geboorte_land_code == "9999" ~ NA_character_,
                   geboorte_land_code == "0000" ~ NA_character_,
                   geboorte_land_code == "0" ~ NA_character_,
                   TRUE ~ geboorte_land_code
                 ),
                 
                 geboorte_land_moeder_code = case_when(
                   geboorte_land_moeder_code == "" ~ NA_character_,
                   geboorte_land_moeder_code == "9999" ~ NA_character_,
                   geboorte_land_moeder_code == "0000" ~ NA_character_,
                   TRUE ~ geboorte_land_moeder_code
                 ),
                 geboorte_land_vader_code = case_when(
                   geboorte_land_vader_code == "" ~ NA_character_,
                   geboorte_land_vader_code == "9999" ~ NA_character_,
                   geboorte_land_vader_code == "0000" ~ NA_character_,
                   TRUE ~ geboorte_land_vader_code
                 ))
  
  data <- mutate(data,
                 
                 geboorte_land_vader_code = coalesce(geboorte_land_vader_code, geboorte_land_moeder_code),
                 geboorte_land_moeder_code = coalesce(geboorte_land_moeder_code, geboorte_land_code),
                 geboorte_land_code = coalesce(geboorte_land_code, geboorte_land_moeder_code))
  
  
  
  # Stap 2: geboorte land hercoderen.
  
  replace_values_key <- function(x, key){
    coalesce(key[[2]][match(x, key[[1]])], x)
  }
  
  data$geboorte_land_code_her <- replace_values_key(data$geboorte_land_code, geboorte_land_hercodering)
  data$geboorte_land_vader_code_her <- replace_values_key(data$geboorte_land_vader_code, geboorte_land_hercodering)
  data$geboorte_land_moeder_code_her <- replace_values_key(data$geboorte_land_moeder_code, geboorte_land_hercodering)
  
  # Onbekend
  data <- mutate(data,
                 geboorte_land_code_her = replace_na(geboorte_land_code_her, "0000"),
                 geboorte_land_vader_code_her = replace_na(geboorte_land_vader_code_her, "0000"),
                 geboorte_land_moeder_code_her = replace_na(geboorte_land_moeder_code_her, "0000")
                 )
  
  # Stap 3: allochtoon categorieen.
  
  # Uitleg:
  # all1gen 'Inwoners met migratieachtergrond 1e generatie naar herkomstland'.
  # al2gen1m 'Inwoners met migratieachtergrond 2e generatie (1 ouder, moeder) naar herkomstland'.
  # al2gen1v 'Inwoners met migratieachtergrond 2e generatie (1 ouder, vader) naar herkomstland'.
  # all2gen2 'Inwoners met migratieachtergrond 2e generatie (2 ouders) naar herkomstland'.
  # all2gen1 'Inwoners met migratieachtergrond 2e generatie (1 ouder) naar herkomstland'.
  # all2gen 'Inwoners met migratieachtergrond 2e generatie naar herkomstland'.
  # alltot 'Inwoners met migratieachtergrond naar herkomstland'.
  
  

  #--> omschrijvev
  #data <- mutate(data, )  
  
  
  # "Allochtoon" 1e generatie.
  data$all1gen <- data$geboorte_land_code_her != "6030" & 
    (data$geboorte_land_vader_code_her != "6030" | data$geboorte_land_moeder_code_her != "6030")
  data$all1gen <- ifelse(data$all1gen, data$geboorte_land_code_her, NA_character_)
  
  # "Allochtoon" 2e generatie - vader NL.
  data$al2gen1m <- data$geboorte_land_code_her == "6030" &
    data$geboorte_land_vader_code_her == "6030" & data$geboorte_land_moeder_code_her != "6030"
  data$al2gen1m <- ifelse(data$al2gen1m, data$geboorte_land_moeder_code_her, NA_character_)
  
  # "Allochtoon" 2e generatie - moeder NL.
  data$al2gen1v <- data$geboorte_land_code_her == "6030" &
    data$geboorte_land_vader_code_her != "6030" & data$geboorte_land_moeder_code_her == "6030"
  data$al2gen1v <- ifelse(data$al2gen1v, data$geboorte_land_vader_code_her, NA_character_)
  
  # "Allochtoon" 2e generatie - beide ouders niet NL.
  data$all2gen2 <- data$geboorte_land_code_her == "6030" &
    data$geboorte_land_vader_code_her != "6030" & data$geboorte_land_moeder_code_her != "6030"
  data$all2gen2 <- ifelse(data$all2gen2, data$geboorte_land_moeder_code_her, NA_character_)
  
  # Combinaties
  data$all2gen1 <- coalesce(data$al2gen1m, data$al2gen1v)
  data$all2gen <- coalesce(data$all2gen1, data$all2gen2)
  
  data$alltot <- coalesce(data$all1gen, data$all2gen)
  data$alltot[is.na(data$alltot)] <- "6030"
  
  
  # Stap 4 : Land labels
  data$geboorte_land_her <- replace_values_key(data$geboorte_land_code_her, land_code_labels)
  data$geboorte_land_vader_her <- replace_values_key(data$geboorte_land_vader_code_her, land_code_labels)
  data$geboorte_land_moeder_her <- replace_values_key(data$geboorte_land_moeder_code_her, land_code_labels)
  
  geboorte_land_cat_key <- left_join(geboorte_land_cat_key, geboorte_land_cat_labels, 
                                     by = "land_categorie_code")
  
  data <- left_join(data, 
                    geboorte_land_cat_key %>%
                      rename(all1genc = land_categorie_code,
                             all1genc_omschrijving = land_categorie
                      ),
                    by = c("all1gen" = "geboorte_land_code_her"))
  
  data$all1genc[is.na(data$all1genc) & !is.na(data$all1gen)] <- 5
  data$all1genc_omschrijving[is.na(data$all1genc_omschrijving) & !is.na(data$all1gen)] <- "Overige niet-westerse landen"
  data$all1genc[is.na(data$all1genc)] <- 9
  data$all1genc_omschrijving[is.na(data$all1genc_omschrijving)] <- "Nederland"
  
  
  data <- left_join(data, 
                    geboorte_land_cat_key %>%
                      rename(all2genc = land_categorie_code,
                             all2genc_omschrijving = land_categorie
                      ),
                    by = c("all2gen" = "geboorte_land_code_her"))
  
  data$all2genc[is.na(data$all2genc) & !is.na(data$all1gen)] <- 5
  data$all2genc_omschrijving[is.na(data$all2genc_omschrijving) & !is.na(data$all1gen)] <- "Overige niet-westerse landen"
  data$all2genc[is.na(data$all2genc)] <- 9
  data$all2genc_omschrijving[is.na(data$all2genc_omschrijving)] <- "Nederland"
  
  data <- left_join(data, 
                    geboorte_land_cat_key %>%
                      rename(alltotc = land_categorie_code,
                             alltotc_omschrijving = land_categorie
                      ),
                    by = c("alltot" = "geboorte_land_code_her"))
  
  data$alltotc[is.na(data$alltotc)] <- 5
  data$alltotc_omschrijving[is.na(data$alltotc_omschrijving)] <- "Overige niet-westerse landen"
  
  
  # westers / niet-westers.
  data(geboorte_land_western_labels)
  data <- left_join(data, geboorte_land_western_labels, by = "alltotc_omschrijving")
  
  # MOE categorieen
  data(geboorte_land_code_key)
  
  data <- left_join(data, 
                    select(geboorte_land_code_key, -ethniciteit_cat1_code, -ethniciteit_cat1_omschrijving),
                    by = c("alltot" = "geboorte_land_code")) %>%
    mutate(ethniciteit_cbs_code = replace_na(ethniciteit_cbs_code, "5"),
           ethniciteit_cbs_omschrijving = replace_na(ethniciteit_cbs_omschrijving, "Ov. niet-westerse landen"),
           ethniciteit_moe_code = replace_na(ethniciteit_moe_code, "2222"),
           ethniciteit_moe_omschrijving = replace_na(ethniciteit_moe_omschrijving, "Overige werelddelen")
    )
  
  return(data)
}

