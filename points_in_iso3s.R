#SCRIPT TO TEST That lat/longs fit within their corresponding iso3s
#by Manny Garcia
#gmanny@uw.edu

#source("/snfs2/HOME/gmanny/backups/Documents/Repos/data_validation/points_in_iso3s.R")

twelve_km_buffer <- F
#if set to false, default is 5km

if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}
p_load(data.table, rgdal, rgeos, sp, magrittr, plyr, maps, mapdata)

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

geo_points <- geo[!is.na(lat) & !is.na(long), ]
geo_points <- geo_points[order(iso3)]

message("geopositioning points")
coordinates(geo_points) <- ~long+lat
proj4string(geo_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

message("reading in master GBD shapefile")
global_shp <- readOGR(dsn=paste0(j, "DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/inset_maps/noSubs/GBD_WITH_INSETS_NOSUBS.shp"))
global_shp <- spTransform(global_shp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

global_shp$iso3 <- global_shp$iso3 %>% as.character

countries <- unique(global_shp$iso3) %>% as.character()

prob <- data.frame()
for (place in countries){
  message(paste0("Testing points in ", place))
  shp <- global_shp[global_shp$iso3 == place, ]
  country <- geo_points[geo_points$iso3 == place,]
  if (nrow(country) < 1){
    next
  }
  intersect <- over(country, global_shp)
  bad <- country[is.na(intersect$iso3) | !(place %in% intersect$iso3), ] %>% as.data.frame()
  
  prob <- rbind.fill(bad, prob)
}

#save(prob, file="J:/temp/gmanny/spatial_polygon_of_problems.RData")
message(paste("found", nrow(prob), "points outside of national boundaries."))


###
### use smaller group with 12km buffer to see which points are truely way out of their expected iso3
###

prob2 <- prob

if (twelve_km_buffer){
  buffer = "12"
} else{
  buffer = "5"
}

message(paste0("reading ", buffer, "km buffer Africa shapefile"))
# bufferred shapefiles are made in arcmap instead of R due to memory and speed drawbacks to the gBuffer function
if (!twelve_km_buffer){
  af_shp <- readOGR(dsn=paste0(j, "WORK/11_geospatial/05_survey shapefile library/Shapefile directory/AfricaWithA5kmBuffer.shp"))
} else{
  af_shp <- readOGR(dsn=paste0(j, "WORK/11_geospatial/05_survey shapefile library/Shapefile directory/Af12kmBuffer.shp"))
}
af_shp <- spTransform(af_shp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

message("subset out non-stage 1 countries")
stages <- read.csv(paste0(j, "temp/gmanny/geospatial_stages_priority.csv"), stringsAsFactors=F)
s1 <- subset(stages, Stage == 1)
af_shp <- subset(af_shp, iso3 %in% s1$alpha.3)

iso <- unique(af_shp$iso3)

message("geopositioning points outside of master shapefile iso3s")
coordinates(prob2) <- ~long+lat
proj4string(prob2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


fix <- data.frame()
for (place in iso){
  message(paste0("Testing points in ", place))
  shp <- af_shp[af_shp$iso3 == place, ]
  country <- prob2[prob2$iso3 == place,]
  if (nrow(country) < 1){
    next
  }
  intersect <- over(country, af_shp)
  bad <- country[is.na(intersect$iso3) | !(place %in% intersect$iso3), ] %>% as.data.frame()
  
  fix <- rbind.fill(bad, fix)
}

message(paste("Found", nrow(fix), "points outside of ", buffer, "km buffer of national boundaries."))

save(fix, file=paste0(j, "temp/gmanny/fix_points_outside_of_", buffer, "km_buffer.RData"))

#used when running locally in case of package installation issues
if (j == "J:/"){load(paste0("J:/temp/gmanny/fix_points_outside_of_", buffer, "km_buffer.RData"))}
af_shp <- subset(global_shp, iso3 %in% s1$alpha.3)
png(filename="points_outside_borders.png")
fix <- data.frame(fix)
shp <- fortify(af_shp)
shp <- subset(shp, lat > -40) #drop zooms in shapefile (inaccurately geopositioned to allowed a larger-than-actual view)
#af <- merge(shp, af_shp, by.x="id", by.y="ORIG_FID")
#af <- af[order(af$order),]
ggplot(fix) + geom_polygon(data=shp, aes(x=long, y=lat, group=group), color="white") + 
  geom_point(aes(x=long, y=lat, color=nid), size=6) + coord_fixed() + 
  labs(title=paste0("Points falling ", buffer, "km outside of national borders"), x="Longitude", y="Latitude")
dev.off()