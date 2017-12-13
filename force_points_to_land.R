if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}
p_load(data.table, rgdal, rgeos, sp, magrittr, plyr, snow, raster, ggplot2)
devtools::install_github('SEEG-Oxford/seegSDM')
j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/snfs1/")

#prep lat/long data
load(paste0(j, "temp/gmanny/fix_points_outside_of_5km_buffer.RData"))
points <- fix[, c("long", "lat")]
setnames(points, "long", "Longitude")
setnames(points, "lat", "Latitude")

#prep polygon of Africa
#polygon
global_shp <- readOGR(dsn=paste0(j, "DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/inset_maps/noSubs/GBD_WITH_INSETS_NOSUBS.shp"))
global_shp <- spTransform(global_shp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
stages <- read.csv(paste0(j, "temp/gmanny/geospatial_stages_priority.csv"), stringsAsFactors=F)
s1 <- subset(stages, Stage == 1)
af_shp <- subset(global_shp, iso3 %in% s1$alpha.3)
shp <- fortify(af_shp)
shp <- subset(shp, lat > -40) #drop zooms in shapefile (inaccurately geopositioned to allowed a larger-than-actual view)
shp <- shp[, c("long", "lat")]

