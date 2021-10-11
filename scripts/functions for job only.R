

#------ Utils ------

pm_read_config <- function(fn){
  
  out <- try(yaml::read_yaml(fn))
  
  if(inherits(out, "try-error")){
    pm_log("Error reading config ({fn})", "fatal")
    return(FALSE)
  } 
  
  for(i in seq_along(out$config)){
    out$config[[i]]$name <- names(out$config)[i]
  }
  
  return(out)  
}



huishouden_write_output <- function(brp){

  # Leeftijd afgerond naar beneden, zoals het hoort
  brp$leeftijd <- floor(brp$leeftijd)

  data.table::fwrite(brp, file.path(.cc$paths$outputdir, "brp_huishoudens.csv"),
                     sep = ";",
                     row.names = FALSE, col.names = TRUE)
  pm_log("Output written ({.cc$paths$outputdir})")
}

pm_open_logfile <- function(path, what){
  
  today_ <- format(Sys.Date(), "%Y%m%d")
  fn <- file.path(path, paste0(today_, "_", what, ".log"))
  
  flog.appender(appender.tee(fn), name = what)
  pm_log("------------- start {what} v. {.version} -------------")
}


pm_log <- function(msg, how = c("info","fatal","warn")){
  how <- match.arg(how)
  
  msg <- glue(msg, .envir = parent.frame(n = 1))
  switch(how, 
         fatal = flog.fatal(msg, name = .jobname),
         info = flog.info(msg, name = .jobname),
         warn = flog.warn(msg, name = .jobname))
}


pm_open_semafoor <- function(){
  fn <- file.path(.cc$paths$outputdir, "locked.sem")
  writeLines("hello from shintolabs", fn)
}

pm_close_semafoor <- function(){
  fn <- file.path(.cc$paths$outputdir, "locked.sem")
  unlink(fn)  
}

