rm(list=ls())

africa_only <- T

if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}
p_load(data.table, rgdal, rgeos, sp, magrittr, plyr, ggplot2, raster)

j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/snfs1/")

read_add_name_col <- function(file){
  #FOR GEOGRAPHY CODEBOOKS. READS THEM IN AND ADDS A COLUMN WITH THEIR CORRESPONDING SURVEY_SERIES
  message(file)
  rn <- gsub(".csv", "", file, ignore.case=T)
  spl <- strsplit(rn, "/") %>% unlist()
  svy <- spl[length(spl)]
  df <- read.csv(file, stringsAsFactors = F)
  df <- as.data.table(df)
  df[, survey_series := svy]
  df <- lapply(df, as.character)
  return(df)
}

stages <- read.csv(paste0(j, "temp/gmanny/geospatial_stages_priority.csv"), stringsAsFactors=F)
s1 <- subset(stages, Stage == 1)

message("packaging all codebooks together")
files <- list.files(paste0(j, "WORK/11_geospatial/05_survey shapefile library/codebooks"), pattern=".csv$", ignore.case = T, full.names = T)
files <- grep("Copy|together|linkage|IPUMS|special", files, value = T, invert = T) #IPUMS is handled separately
message("lapply geographies into list")
geogs <- lapply(files, read_add_name_col)
message("rbind geography codebooks together")
geo <- rbindlist(geogs, fill=T, use.names=T)
geo <- geo[, lat := as.numeric(lat)]
geo <- geo[, long := as.numeric(long)]
geo <- geo[, iso3 := substr(iso3, 1, 3)]

if (africa_only){
  geo <- subset(geo, geo$iso3 %in% s1$alpha.3)
}

geo_points <- geo[!is.na(lat) & !is.na(long), ]
geo_points <- geo_points[order(iso3)]

message("geopositioning points")
coordinates(geo_points) <- ~long+lat
proj4string(geo_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#global_shp <- readOGR(dsn=paste0(j, "DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/inset_maps/noSubs/GBD_WITH_INSETS_NOSUBS.shp"))
#global_shp <- spTransform(global_shp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

global_shp$iso3 <- global_shp$iso3 %>% as.character

africa <- subset(global_shp, iso3 %in% s1$alpha.3)
africa$continent <- "af"
#dis_af <- gUnaryUnion(africa, id=africa@data$continent)
#dis_af <- crop(dis_af, extent(-17.51445, 51.41255, -39.83614, 37.53936))

#dis_af <- as(dis_af, "SpatialPolygonsDataFrame")
#writeOGR(dis_af, dsn="H:/backups/Documents/shapefiles", layer="africa_shp_fix_in_arcmap", driver="ESRI Shapefile")

dis_line <- as(dis_af, "SpatialLines")


o <- over(geo_points, dis_af)
in_h2o <- o[is.na(o)] %>% names
geo_points <- data.frame(geo_points)
fix <- geo_points[in_h2o,]

fix_m <- fix[,c("long", "lat")] %>% data.matrix

#ds_pt <- as(dis_af, "SpatialPoints") %>% data.frame %>% data.matrix
ds_pt <- readOGR("H:/backups/Documents/shapefiles/af_pts_win_12km_coast.shp") %>% data.frame %>% data.matrix
ds_pt <- ds_pt[, c("coords.x1", "coords.x2")]

fixed <- data.frame()
for (i in 1:nrow(fix_m)){
  distances <- spDistsN1(ds_pt, fix_m[i,])
  m_dist <- ds_pt[distances == min(distances)]
  if (length(m_dist) > 2){
    if (length(m_dist)%%2 == 0){
      first <- length(m_dist)/2
      last <- first+1
      f <- data.frame(long=m_dist[first], lat=m_dist[last])
    } else{
      message("Contact Manny about an issue with the geography codebooks. Not all points will be properly shifted from sea to land.")
      f <- data.frame(long=m_dist[1], lat=m_dist[2])
    }
  } else{
    f <- data.frame(long=m_dist[1], lat=m_dist[2])
  }
  fixed <- rbind.fill(fixed, f)
}

#Find points that didn't quite make it to land
fixed_pt <- fixed
coordinates(fixed_pt) <- ~long+lat
proj4string(fixed_pt) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

o <- over(fixed_pt, dis_af)
in_h2o <- o[is.na(o)] %>% names
geo_points <- data.frame(geo_points)
fix <- geo_points[in_h2o,]

not_quite <- cbind(data.frame(), fixed_pt[is.na(over(fixed_pt, africa)$continent),] %>% data.frame)

dis_shp <- fortify(dis_af)
dis_shp <- subset(dis_shp, lat > -40)

ggplot() + coord_fixed() + geom_polygon(data=dis_shp, aes(x=long, y=lat, group=group), color="black", fill="black") +
  geom_point(data=fix, aes(x=long, y=lat, color=iso3), size=3) +
  labs(title=paste0("Points in the Ocean"), x="Longitude", y="Latitude")

ggplot() + coord_fixed() + geom_polygon(data=dis_shp, aes(x=long, y=lat, group=group), color="black", fill="black") + 
  geom_point(data=fix, aes(x=long, y=lat, color=iso3), size=1) + geom_point(data=fixed, aes(x=long, y=lat), color="red", size=0) + 
  labs(title=paste0("Points in the Ocean"), x="Longitude", y="Latitude")