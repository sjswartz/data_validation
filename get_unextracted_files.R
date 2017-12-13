extracts <- list.files("J:/LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/wash_2/", full.names=T)

if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}
p_load(googlesheets, openxlsx, data.table, plyr, httpuv, magrittr)

###
#Get unextracted files
###

cutoff <- as.POSIXct("2017-09-22")

unextracted <- vector()
for (file in extracts){
  edit_time <- file.info(file)$mtime
  if (edit_time < cutoff){
    unextracted <- c(unextracted, file)
  }
}

###
#Turn them into NIDs
###

find_nid_in_filename <- function(sep2){
  for (part in sep2){
    nid_test <- !is.na(as.numeric(part))
    if (nid_test == TRUE){
      return(part)
    }
  }
}

nids <- vector()
for (file in unextracted){
  separated <- strsplit(file, "/") %>% unlist
  end <- separated[length(separated)]
  sep2 <- strsplit(end, "_") %>% unlist
  nids <- c(nids, find_nid_in_filename(sep2))
}

nids <- as.numeric(nids)

###
#Match them to ubcov ids
###

#open wash ubcov codebook
google_id <- "1WCbgCtciUOEwqwqPtkvlWfaVQkCd16i2zggScPukGyg"
my_sheets <- gs_ls()
topic <- gs_key(google_id)
codebook <- gs_read(topic)

codebook <- data.table(codebook)

rerun <- codebook[nid %in% nids, ]$ubcov_id %>% as.numeric

write.csv(rerun, "J:/temp/gmanny/UbCov/rerun_wash_9_22_2017.csv", row.names=F)
