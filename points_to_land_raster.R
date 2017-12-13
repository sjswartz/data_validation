rm(list=ls())

pacman::p_load(data.table, rgdal, raster, dplyr, seegSDM, sp, rgeos, maptools)
j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/snfs1/")

read_add_name_col <- function(file){
  #FOR GEOGRAPHY CODEBOOKS. READS THEM IN AND ADDS A COLUMN WITH THEIR CORRESPONDING SURVEY_SERIES
  message(file)
  rn <- gsub(".csv", "", file, ignore.case=T)
  spl <- strsplit(rn, "/") %>% unlist()
  svy <- spl[length(spl)]
  #df <- fread(file, data.table=T)
  #get errors in character encoding from geog codebook and fix
  df <- read.csv(file, encoding="windows-1252", stringsAsFactors = F)
  df <- as.data.table(df)
  df[, survey_series := svy]
  df <- lapply(df, as.character)
  return(df)
}

cb_paths <- list.files(paste0(j, "WORK/11_geospatial/05_survey shapefile library/codebooks"), pattern=".csv$", ignore.case = T, full.names = T)
geogs <- lapply(cb_paths, read_add_name_col)
geo <- rbindlist(geogs, use.names=T, fill=T)
geo[, lat := as.numeric(lat)]
geo[, long := as.numeric(long)]
geo_points <- geo[!is.na(lat) & !is.na(long), ]

coordinates(geo_points) <- ~long+lat
proj4string(geo_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

geo_points <- as(geo_points, "SpatialPoints")
fix <- geo_points[is.na(vals),]

grid <- raster(paste0(j, "WORK/11_geospatial/01_covariates/02_Oxford/01_Global_Masks/Land_Sea_Masks/CoastGlobal_5k.tif"))

vals <- extract(grid, geo_points)

corrected <- nearestLand(test_pt_df, grid, 180) #532 points
