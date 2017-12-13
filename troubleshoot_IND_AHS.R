library(dplyr)
library(data.table)
library(magrittr)

load("J:/LIMITED_USE/LU_GEOSPATIAL/collapsed/diarrhea/IND_AHS_collapsed.RData")
View(IND_AHS_collapsed)

nrow(IND_AHS_collapsed)

reduced <- distinct(IND_AHS_collapsed, location_code, shapefile, .keep_all=T)
nrow(reduced)

IND_AHS_collapse <- data.table(IND_AHS_collapsed)


