library(data.table)
library(haven)

read_add_name_col <- function(file){
  #FOR GEOGRAPHY CODEBOOKS. READS THEM IN AND ADDS A COLUMN WITH THEIR CORRESPONDING SURVEY_SERIES
  message(file)
  rn <- gsub(".csv", "", file, ignore.case=T)
  spl <- strsplit(rn, "/") %>% unlist()
  svy <- spl[length(spl)]
  #df <- fread(file, data.table=T)
  #get errors in character encoding from geog codebook and fix
  df <- read.csv(file, encoding="windows-1252")
  df <- as.data.table(df)
  df[, survey_series := svy]
  df <- lapply(df, as.character)
  return(df)
}

message("get all geography codebooks")
files <- list.files(paste0(j, "WORK/11_geospatial/05_survey shapefile library/codebooks"), pattern=".csv$", ignore.case = T, full.names = T)
files <- grep("Copy|together|linkage|IPUMS|special", files, value = T, invert = T) #IPUMS is handled separately
message("lapply geographies into list")
geogs <- lapply(files, read_add_name_col)
message("rbind geography codebooks together")
geo <- rbindlist(geogs, fill=T, use.names=T)


#################
#HH with hh_size#
#################

dta <- read_dta("J:/LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/wash_2/PER_DEMOGRAPHIC_AND_FAMILY_HEALTH_SURVEY_ENDES_303663_HH_PER_2015_2015.dta")


geo_keep <- c("nid", "iso3", "geospatial_id", "point", "lat", "long", "shapefile", "location_code", "survey_series", "admin_level")
geo_k <- geo[, geo_keep, with=F]

dta$nid <- as.character(dta$nid)
dta$geospatial_id <- as.character(dta$geospatial_id)
if (class(dta$nid) == "numeric"){
  geo[, nid := as.numeric(nid)]
} else if (class(dta$nid) == "character"){
  geo[, nid := as.character(nid)]
} else{
  message("update code to accomodate topics nid as")
  message(class(dta$nid))
}


all <- merge(geo_k, dta, by.x=c("nid", "iso3", "geospatial_id"), by.y=c("nid", "ihme_loc_id", "geospatial_id"), all.x=F, all.y=T)

pt_collapse <- all[!is.na(lat) & !is.na(long), ]
pt_collapse[is.na(hh_size), hh_size := sum(hh_size, na.rm=T), by=list(nid, geospatial_id, hhweight, year_start, iso3, lat, long, w_source_drink, w_source_other, mins_ws, t_type, shared_san)]
pt_collapse[, hhs := NULL]
key <- c("nid", "geospatial_id", "hhweight", "year_start", "iso3", "lat", "long", 
         "w_source_drink", "w_source_other", "mins_ws", "dist_ws", 
         "dist_ws_unit", "t_type", "shared_san", "shared_san_num", 
         "hw_station", "hw_water", "hw_soap")
key <- key[key %in% names(pt_collapse)]
pt_collapse <- unique(pt_collapse, by=key)

##################
#HHM with hh_size#
##################
dta <- read_dta("J:/LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/wash_2/GHA_CHILD_LABOR_SURVEY_4010_HHM_GHA_2001_2001.dta")
dta <- as.data.table(dta)
if (class(dta$nid) == "numeric"){
  geo[, nid := as.numeric(nid)]
} else if (class(dta$nid) == "character"){
  geo[, nid := as.character(nid)]
} else{
  message("update code to accomodate topics nid as")
  message(class(dta$nid))
}
dta$nid <- as.character(dta$nid)
dta$geospatial_id <- as.character(dta$geospatial_id)
all <- merge(geo_k, dta, by.x=c("nid", "iso3", "geospatial_id"), by.y=c("nid", "ihme_loc_id", "geospatial_id"), all.x=F, all.y=T)

all[, hhweight := pweight]

all[shapefile=="", shapefile:=NA]
all[location_code=="", location_code:=NA]
poly_collapse <- all[!is.na(shapefile) & !is.na(location_code) & (is.na(lat) | is.na(long)),]
#missing hh_size column because not in microdata

poly_collapse[, hhs:=1]
poly_collapse[, hh_size := sum(hhs), by=list(nid, hhweight, year_start, iso3, shapefile, location_code, w_source_drink, t_type)]
poly_collapse[, hhs := NULL]
poly_collapse <- poly_collapse[, clusters_in_polygon := uniqueN(psu), by=list(nid, hhweight, year_start, iso3, shapefile, location_code, w_source_drink, t_type)]
#poly_collapse <- poly_collapse[nid %in% nids_that_need_hh_size_crosswalk, hh_size := NA]
#setkey(poly_collapse, shapefile, location_code, w_source_drink, w_source_other, mins_ws, dist_ws, dist_ws_unit, t_type, shared_san, shared_san_num, hw_station, hw_water, hw_soap)
key <- c("nid", "hh_id", "geospatial_id", "hhweight", "year_start", "iso3", "lat", "long", 
         "w_source_drink", "w_source_other", "mins_ws", "dist_ws", 
         "dist_ws_unit", "t_type", "shared_san", "shared_san_num", 
         "hw_station", "hw_water", "hw_soap")
key <- key[key %in% names(poly_collapse)]

poly_collapse <- unique(poly_collapse, by=key)


#####################
#HHM without hh_size#
#####################




dta <- read_dta("J:/LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/wash_2/JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020_256175_HH_ETH_2014_2014.dta")
dta <- dplyr::distinct(dta, psu, hh_id)
