
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
                                                         institutioneel_adres, level = "23")) %>%
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

