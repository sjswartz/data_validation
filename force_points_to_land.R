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

#prep raster of Africa land
#polygon
global_shp <- readOGR(dsn=paste0(j, "DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/inset_maps/noSubs/GBD_WITH_INSETS_NOSUBS.shp"))
global_shp <- spTransform(global_shp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
stages <- read.csv(paste0(j, "temp/gmanny/geospatial_stages_priority.csv"), stringsAsFactors=F)
s1 <- subset(stages, Stage == 1)
af_shp <- subset(global_shp, iso3 %in% s1$alpha.3)
shp <- fortify(af_shp)
shp <- subset(shp, lat > -40) #drop zooms in shapefile (inaccurately geopositioned to allowed a larger-than-actual view)
shp <- shp[, c("long", "lat")]

#raster
r <- raster()
africa <- rasterize(shp, r)

raster <- africa
max_distance <- 0

#load nearestLand function that should have loaded with seegSDM
nearestLand <- function (points, raster, max_distance) {
  # get nearest non_na cells (within a maximum distance) to a set of points
  # points can be anything extract accepts as the y argument
  # max_distance is in the map units if raster is projected
  # or metres otherwise
  
  # function to find nearest of a set of neighbours or return NA
  nearest <- function (lis, raster) {
    neighbours <- matrix(lis[[1]], ncol = 2)
    point <- lis[[2]]
    # neighbours is a two column matrix giving cell numbers and values
    land <- !is.na(neighbours[, 2])
    if (!any(land)) {
      # if there is no land, give up and return NA
      return (c(NA, NA))
    } else{
      # otherwise get the land cell coordinates
      coords <- xyFromCell(raster, neighbours[land, 1])
      
      if (nrow(coords) == 1) {
        # if there's only one, return it
        return (coords[1, ])
      }
      
      # otherwise calculate distances
      dists <- sqrt((coords[, 1] - point[1]) ^ 2 +
                      (coords[, 2] - point[2]) ^ 2)
      
      # and return the coordinates of the closest
      return (coords[which.min(dists), ])
    }
  }
  
  # extract cell values within max_distance of the points
  neighbour_list <- extract(raster, points,
                            buffer = max_distance,
                            cellnumbers = TRUE)
  
  # add the original point in there too
  neighbour_list <- lapply(1:nrow(points),
                           function(i) {
                             list(neighbours = neighbour_list[[i]],
                                  point = as.numeric(points[i, ]))
                           })
  
  return (t(sapply(neighbour_list, nearest, raster)))
}


nearest <- nearestLand(points, africa, 0)
nearest <- nearest %>% data.frame
setnames(nearest, "x", "long")
setnames(nearest, "y", "lat")
nearest <- data.frame(nearest)
