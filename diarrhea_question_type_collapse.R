#rm(list=ls())
#source("/snfs2/HOME/gmanny/backups/Documents/Repos/data_validation/diarrhea_question_type_collapse.R")

library(data.table)
library(magrittr)
library(dplyr)

message("load in data")
#load("/snfs1/LIMITED_USE/LU_GEOSPATIAL/geo_matched/diarrhea/2017_11_20.Rdata") #all
diar <- all
diar[, N := 1]

#age
message("round min/max ages")
diar[, max_age:=round(max(age_year,na.rm=T)), by=nid]
diar[, min_age:=round(min(age_year, na.rm=T)), by=nid]
diar[min_age == Inf, max_age := NA]
diar[min_age == Inf, min_age := NA]

#only keep post-1997 surveys
message("subset to years used in the model & people who answered the diarrhea question")
diar <- diar[year_start > 1997 & !is.na(had_diarrhea)]


message("round recall weeks")
diar[, had_diarrhea_recall_period_weeks := round(as.numeric(had_diarrhea_recall_period_weeks))]

#add stages
message("stratify data by stage")
diar[,iso3 := substr(iso3, 1, 3)]
stages <- read.csv("/snfs1/temp/gmanny/geospatial_stages_priority.csv", stringsAsFactors = F) %>% data.table
stages <- stages[, .(alpha.3, Stage)]
stages[, Stage := gsub("[abc]", "", Stage)]
staged_m <- merge(diar, stages, by.x="iso3", by.y="alpha.3", all.x=T)

#disaggregate by world region
message("disaggregate by region")
root <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
repo <- '/share/code/geospatial/ngraetz/mbg/'
setwd(repo)
package_lib <- paste0(root,'/temp/geospatial/geos_packages')
.libPaths(package_lib)
source('mbg_central/mbg_functions.R')                   # Functions to run MBG model.
source('mbg_central/prep_functions.R')                  # Functions to setup MBG run
source('mbg_central/covariate_functions.R')             # Functions to prep and transform 5*5 covariates
source('mbg_central/misc_functions.R')                  # Other computational MBG-related functions.
source('mbg_central/post_estimation_functions.R')
source('mbg_central/gbd_functions.R')
source('mbg_central/shiny_functions.R')
source('mbg_central/holdout_functions.R')
source('mbg_central/polygon_functions.R')
source('mbg_central/collapse_functions.R')
source('mbg_central/seegMBG_transform_functions.R')
gaul_codes <- vector()
se_asia <- get_gaul_codes(c('se_asia', 'PNG', 'VUT', 'MNG'))
south_asia <- get_gaul_codes(c('south_asia', 'MDV'))
latin_america <- get_gaul_codes(c('latin_america', 'MEX'))
middle_east <- get_gaul_codes(c('middle_east', "UZB", "TKM", "TJK", "KGZ", "YEM"))
gaul_codes <- c(se_asia, south_asia, latin_america, middle_east)

se_count <- rep('se_asia', length(se_asia))
sa_count <- rep('south_asia', length(south_asia))
la_count <- rep('latin_america', length(latin_america))
me_count <- rep('middle_east', length(middle_east))

regions <- c(se_count, sa_count, la_count, me_count)

geo <- data.table()
geo <- geo[, .(gaul = gaul_codes, region = regions)]

loc_ids <- read.csv("/snfs1/WORK/11_geospatial/10_mbg/gaul_to_loc_id.csv", stringsAsFactors = F) %>% data.table
loc_ids <- loc_ids[, .(ihme_lc_id, GAUL_CODE)]
geo_loc_ids <- merge(geo, loc_ids, by.x="gaul", by.y="GAUL_CODE", all.x=T)



reg_matchup <- geo_loc_ids[, .(ihme_lc_id, region)]
setnames(reg_matchup, "ihme_lc_id", "ihme_loc_id")

message("merge regions onto main data")
staged_m <- merge(staged_m, reg_matchup, by.x="iso3", by.y="ihme_loc_id", all.x=T)

#question cat
message("get question info from ubcov codebook")
codebook <- read.csv("/snfs1/temp/gmanny/diarrhea_ubcov_codebook.csv", stringsAsFactors = F) %>% data.table
codebook[nchar(had_diarrhea_false) > 4, format := "Which ailment"]
codebook[nchar(had_diarrhea_false) <= 4 | grepl("No", had_diarrhea_false), format := "Standard"]
codebook <- distinct(codebook, ihme_loc_id, survey_module, nid, had_diarrhea_false, format)
m <- merge(staged_m, codebook, by.x=c("iso3", "nid", "survey_module"), by.y=c("ihme_loc_id", "nid", "survey_module"))

#calculate number if participants and surveys
message("collapse by max_age, min_age, stage, format, region, and recall_period")
m[,n_peep := sum(N),by=.(max_age, min_age, Stage, format, region, had_diarrhea_recall_period_weeks)]
m[,n_surv := uniqueN(nid),by=.(max_age, min_age, Stage, format, region, had_diarrhea_recall_period_weeks)]

#dedup
collap <- distinct(m, max_age, min_age, Stage, format, region, had_diarrhea_recall_period_weeks, .keep_all=T)

keep <- c("min_age", "max_age", "format", "had_diarrhea_recall_period_weeks", "region", "Stage", "n_peep", "n_surv")
collap <- collap[, keep, with=F]

#cleanup & bin ages
#0-3
#0-5
#All Ages
#Adults Only
#NA
clean <- collap
clean[max_age == 2, bucket := "0-2"]
clean[max_age == 3, bucket := "0-3"]
clean[max_age == 4, bucket := "0-4"]
clean[max_age > 4, bucket := "0-5"]
clean[min_age <= 1 & max_age >= 20, bucket := "All Ages"]
clean[min_age >=13, bucket := "Adults Only"]
clean[is.na(max_age), bucket := "No Age Info"]

clean[, Participants := sum(n_peep), by=.(format, Stage, bucket, region, had_diarrhea_recall_period_weeks)]
clean[, Surveys := sum(n_surv), by=.(format, Stage, bucket, region, had_diarrhea_recall_period_weeks)]

clean <- distinct(clean, format, Stage, Participants, Surveys, region, had_diarrhea_recall_period_weeks, bucket)
setnames(clean, "had_diarrhea_recall_period_weeks", "Recall")
write.csv(collap, "/snfs1/temp/gmanny/diarrhea_question_type_collapse.csv", row.names = F, na="NA")
write.csv(clean, "/snfs1/temp/gmanny/bucketed_diarrhea_question_type.csv", row.names = F, na="NA")
