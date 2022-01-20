remove_identical <- function(data){
  n_h <- nrow(data)
  data <- distinct(data)
  # print(glue("removed {n_h - nrow(data)} identical rows"))
  
  data
}


filter_last_leading <- function(column_in, column_out, val){
  
  if(column_in[1] != val){
    return(NA)
  }
  
  rl <- data.table::rleid(column_in)
  vals <- column_out[rl == 1]
  
  vals[length(vals)]
  
}


leeftijd_delta <- function(x){
  
  if(length(x) == 1){
    0
  } else {
    abs(x[2] - x[1])
  }
  
}

datum_delta <- function(x){
  
  x <- x[!is.na(x)]
  
  if(length(x) < 2){
    999
  } else {
    as.numeric(difftime(x[1],x[2], "days"))
  }
  
  
}

# alleen voor TRUE/FALSE
table_perc <- function(column, data){
  
  tab <- table(data[[column]])
  unname(round(100 * tab[2] / sum(tab[1:2]),1))
  
}

# alleen voor TRUE/FALSE
table_frac <- function(column, data){
  
  tab <- table(data[[column]])
  unname(tab[2] / sum(tab[1:2]))
}


drop_columns <- function(data, what){
  what <- intersect(names(data),what)
  dplyr::select(data, -any_of(what))
}


