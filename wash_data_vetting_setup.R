#source("/snfs2/HOME/gmanny/backups/Documents/Repos/data_validation/wash_data_vetting_setup.R")
package_lib <- '/snfs1/temp/geospatial/geos_packages'
.libPaths(package_lib)
library(feather)
library(plyr)
library(data.table)
library(magrittr)

message("load points")
pts <- read_feather("/snfs1/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/points_2017_12_11.feather") %>% data.table
message("load polygons")
pol <- read_feather("/snfs1/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly_2017_12_11.feather") %>% data.table

message("rbind them together")
wash <- rbind.fill(pts, pol)

message("read in definitions")
#piped_defs <- read.csv("/snfs1/WORK/11_geospatial/wash/definitions/w_source_defined_updated_2017_09_28.csv", stringsAsFactors = FALSE)
t_defs <- read.csv("/snfs1/WORK/11_geospatial/wash/definitions/t_type_defined_updated_2017_09_28.csv", stringsAsFactors = FALSE)
piped_defs <- t_defs

#piped_defs <- piped_defs[, c(1, 2, 4)]
piped_defs <- piped_defs[, c(1, 2)]

piped_defs <- unique(piped_defs)


message("merge in defintions")
#m <- merge(wash, piped_defs, by.x="w_source_drink", by.y="string", all.x=T) %>% data.table
m <- merge(wash, piped_defs, by.x="t_type", by.y="string", all.x=T) %>% data.table

m <- m[!(nid==21970 & survey_module == "HHM"), ]

#add weighted tabulation function
message("add w_prev function")
source("/snfs2/HOME/gmanny/backups/Documents/Repos/data_validation/weighted_tabulations_by_string.R")

message("Sources with shared strings:")
pri <- read.csv("/snfs1/temp/gmanny/geospatial_stages_priority.csv", stringsAsFactors = F) %>% as.data.table
af <- pri[Stage==1, alpha.3]
m[grepl("share|partag|compart", t_type) & substr(iso3, 1, 3) %in% af, file_path] %>% unique