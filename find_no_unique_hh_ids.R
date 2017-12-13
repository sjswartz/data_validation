library(data.table)
library(haven)
library(parallel)

#source("/snfs2/HOME/gmanny/backups/Documents/Repos/data_validation/find_no_unique_hh_ids.R")

j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/snfs1/")
folder_in <- paste0(j, "LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/wash_2")


f <- list.files(folder_in, full.names=T)

find_non_unique_hh_ids <- function(path){
  message(path)
  dta <- read_dta(path)
  
  ####### define booleans##########
  has_psu <- "psu" %in% names(dta)
  has_hh_id <- "hh_id" %in% names(dta)
  has_line_id <- "line_id" %in% names(dta)
  
  ##### test for presense of key-making colnames
  if (!has_hh_id){
    return(unique(dta$nid))
  }
  if (has_psu & has_hh_id & has_line_id){
    id <- paste(dta$psu, dta$hh_id, dta$line_id)
  } else if (has_psu & has_hh_id){
    id <- paste(dta$psu, dta$hh_id)
  } else if(has_hh_id & has_line_id){
    id <- paste(dta$hh_id, dta$line_id)
  } else if (has_hh_id){
    id <- dta$hh_id
  }
  
  ##### test keys for uniqueness
  uq_id <- unique(id)
  uq_id_len <- length(uq_id)
  
  if (uq_id_len < nrow(dta)){
    return(unique(dta$nid))
  }
}

bad <- mclapply(f, find_non_unique_hh_ids, mc.cores=30)

iso <- read.csv(paste0(j, "temp/gmanny/geospatial_stages_priority.csv"), stringsAsFactors=F)

r <- read.csv("J:/temp/gmanny/fix_hh_ids.csv", stringsAsFactors=F)

peruf <- codebook[codebook$ihme_loc_id == "PER" & codebook$nid %in% r$x, ]
