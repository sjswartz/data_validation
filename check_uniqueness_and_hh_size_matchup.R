#source("/snfs2/HOME/gmanny/backups/Documents/Repos/data_validation/check_uniqueness_and_hh_size_matchup.R")

# if(!require(pacman)) {
#   install.packages("pacman"); require(pacman)}
# p_load(googlesheets, openxlsx, data.table, plyr, httpuv, magrittr)
# 
# google_id <- "1WCbgCtciUOEwqwqPtkvlWfaVQkCd16i2zggScPukGyg"
# my_sheets <- gs_ls()
# topic <- gs_key(google_id)
# codebook <- gs_read(topic)
# stages <- read.csv("J:/temp/gmanny/geospatial_stages_priority.csv", stringsAsFactors=F)
# stage_1 <- stages[stages$Stage == "1", "alpha.3"] %>% unique %>% sort
# 
# df_w_nids <- subset(codebook, substr(ihme_loc_id, 1, 3) %in% stage_1 & survey_name != "IPUMS_CENSUS")
# nids <- df_w_nids$nid[!is.na(df_w_nids$nid)]
# 
# save(nids, file="J:/temp/gmanny/africa_wash_nids.RData")

.libPaths("/snfs1/temp/geospatial/")
library(haven)
load("/snfs1/temp/gmanny/africa_wash_nids.RData")



nids_to_check_manually <- NULL

nids <- paste0("_", nids, "_")

for (n in nids){
  fi <- list.files(path="/snfs1/LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/wash_2", pattern=as.character(n), full.names = T)
  
  if (length(fi) > 1){
    message(paste("multiple files match", n))
    #print(fi)
    nids_to_check_manually <- c(nids_to_check_manually, n)
    next
  } else if (length(fi) == 0){
    message(paste(n, "is not extracted"))
    next
  }
  d <- read_dta(fi)
  
  has_psu <- "psu" %in% names(d)
  has_line_id <- "line_id" %in% names(d)
  
  if(has_psu & has_line_id){
    k <- paste(d$psu, d$hh_id, d$line_id, sep="_")
  } else if(has_psu){
    k <- paste(d$psu, d$hh_id, sep="_")
  } else if(has_line_id){
    k <- paste(d$hh_id, d$line_id, sep="_")
  } else{
    k <- d$hh_id
  }
  
  k_len <- length(unique(k))
  
  if (k_len != nrow(d)){
    message(paste("psu & hh_id do not uniquely identify", n))
    nids_to_check_manually <- c(nids_to_check_manually, n)
    next
  }
  
  
  
}

nids_to_check_manually <- gsub("_", "", nids_to_check_manually)