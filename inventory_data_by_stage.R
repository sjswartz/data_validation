library(dplyr)
library(magrittr)
library(data.table)

indicator <- "wash"
collapse_data_type <- "Rdata" #can be csv

j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/snfs1/")

#find most recent indicator collapse
path <- paste0(j, "LIMITED_USE/LU_GEOSPATIAL/collapsed/", indicator)
collapse_data_type <- tolower(collapse_data_type)
file_end <- paste0(".", collapse_data_type, "$")
files <- list.files(path, pattern=file_end, full.names = T, ignore.case=T) %>% grep(value=T, pattern="IND_AHS", invert=T)
details <- file.info(files)
d <- details
d$path <- rownames(d)
d <- d %>% data.table
d <- d[order(mtime, decreasing=T),]
most_recent <- d[1, path]

if (collapse_data_type == "rdata"){
  load(most_recent) #loads collapsed
} else if (collapse_data_type == "csv"){
  collapsed <- read.csv(most_recent, stringsAsFactors = F)
} else{
  stop("This script only accepts Rdata and csv as collapse_data_type arguments.")
}

#set common names to ones used in this code
try(setnames(collapsed, "svy_id", "nid"))
try(setnames(collapsed, "iso3", "country"))
try(setnames(collapsed, "ihme_loc_id", "country"))
try(setnames(collapsed, "year_start", "start_year"))
try(setnames(collapsed, "source", "survey_series"))

data_inventory <- collapsed %>% distinct(nid, country, survey_series, start_year)

stages <- read.csv(paste0(j, "temp/gmanny/geospatial_stages_priority.csv"), stringsAsFactors=F)

m <- merge(data_inventory, stages, by.x="country", by.y="alpha.3", all.y=T) %>% data.table

m[, n:= 1]
m[,country := substring(country, 1, 3)]
m[, n_by_country := sum(n), by=country]

uq <- distinct(m, country, name, n_by_country, Stage)

uq[!(country %in% unique(data_inventory$country)), n_by_country := 0]
uq <- uq[order(n_by_country)]

africa <- uq[Stage == "1"]
stage_2 <- uq[Stage == "2a" | Stage == "2b"]

