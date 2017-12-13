#source("/snfs2/HOME/gmanny/backups/Documents/Repos/data_validation/validate_survey_years.R")

pacman::p_load(haven, data.table, doParallel, doSNOW)
cores <- 30

j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/snfs1/")
fs <- list.files(paste0(j, "LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/wash_2"), full.names=T)

get_dates <- function(f){
  f <- gsub(".dta|.DTA", "", f)
  s <- strsplit(f, "/") %>% unlist()
  p <- s[length(s)]
  p <- strsplit(p, "_") %>% unlist()
  years <- c(p[length(p)-1], p[length(p)])
  return(years)
}


check_dates <- function(f){
  dta <- read_dta(f)
  has_int_year <- "int_year" %in% names(dta)
  if (!has_int_year){
    return(NULL)
  }
  years <- get_dates(f)
  years <- as.numeric(years)
  
  any_too_big_years <- any(dta$int_year > years[2])
  any_too_small_years <- any(dta$int_year < years[1])
  
  if (any_too_big_years | any_too_small_years){
    bad_nid <- unique(dta$nid)
    return(bad_nid)
  } else{
    return(NULL)
  }
}

message("make cluster")
cl <- makeCluster(cores)
message("register cluster")
registerDoSNOW(cl)
message("start foreach")
bad_nids <- foreach(i=1:length(fs), .packages=c("haven", "magrittr")) %dopar% {
  #check_dates function
  f <- fs[i]
  dta <- read_dta(f)
  has_int_year <- "int_year" %in% names(dta)
  if (!has_int_year){
    return(NULL)
  }
  years <- get_dates(f)
  years <- as.numeric(years)
  is_na_int_year <- all(is.na(dta$int_year))
  any_too_big_years <- any(dta$int_year > years[2])
  any_too_small_years <- any(dta$int_year < years[1])
  
  if (any_too_big_years | any_too_small_years){
    bad_nid <- unique(dta$nid)
    return(bad_nid)
  } else{
    return(NULL)
  }
}
stopCluster(cl)

bad_nids <- unlist(bad_nids)
message(paste("Found", length(bad_nids), "problematic int_year nids."))