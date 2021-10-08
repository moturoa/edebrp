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

