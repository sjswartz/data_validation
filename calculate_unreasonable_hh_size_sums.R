library(data.table)
load("J:/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/points_collapsed_2017_08_01.Rdata")

mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

attention <- pt_collapse[, list(nid, iso3, lat, long, year_start, survey_name, urban, hh_size)]
attention <- attention[, N := sum(hh_size), by=list(nid, iso3, survey_name, lat, long, year_start)]
attention <- unique(attention, by=c("nid", "iso3", "lat", "long", "survey_name", "year_start"))

p3 <- pt_collapse[nid == 303664, list(nid, iso3, lat, long, year_start, survey_name, urban, hh_size)]
p3s <- p3[, N := sum(hh_size), by=list(nid, iso3, lat, long, year_start, survey_name, urban)]
m <- max(p3s$N)

sum(p3[long == mode(p3$long), ]$hh_size)

