#source("/snfs2/HOME/gmanny/backups/Documents/Repos/data_validation/find_unmatched_data.R")

package_lib <- '/snfs1/temp/geospatial/geos_packages'
.libPaths(package_lib)
library(feather)
library(plyr)
library(data.table)
library(magrittr)

message("load points")
pts <- read_feather("/snfs1/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/points_2017_11_03.feather") %>% data.table
message("load polygons")
pol <- read_feather("/snfs1/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly_2017_11_03.feather") %>% data.table

message("rbind them together")
#wash <- rbind.fill(pt_collapse, poly_collapse)
wash <- rbind.fill(pts, pol)

wash_codebook <- read.csv("/snfs1/temp/gmanny/wash_ubcov_codebook.csv", stringsAsFactors = F) %>% as.data.table

pri <- read.csv("/snfs1/temp/gmanny/geospatial_stages_priority.csv", stringsAsFactors = F) %>% as.data.table
af <- pri[Stage==1, alpha.3]

necessary_nids <- wash_codebook[substr(ihme_loc_id, 1, 3) %in% af & year_end >= 1998 & survey_name != "IPUMS_CENSUS", nid]

flagged <- necessary_nids[!(necessary_nids %in% unique(wash$nid))]

issue_count <- length(flagged)

message(paste("There are", issue_count, "nids missing from your final dataset."))

write.csv(flagged, "/snfs1/temp/gmanny/nids_in_af_not_in_final_extract.csv", row.names=F, na="")