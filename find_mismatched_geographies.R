rm(list=ls())
library(haven)
library(data.table)
library(magrittr)
dta <- read_dta("J:/LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/wash_2/MACRO_DHS_275090_HH_PER_2003_2008.dta") %>% data.table
dhs <- read.csv("J:/WORK/11_geospatial/05_survey shapefile library/codebooks/MACRO_DHS.csv", encoding="windows-1252", stringsAsFactors=F) %>% data.table

p_dhs <- dhs[nid == 275090,]
geo <- unique(p_dhs$geospatial_id) %>% length
micro <- unique(dta$geospatial_id) %>% length

micro[!(micro %in% geo)]
