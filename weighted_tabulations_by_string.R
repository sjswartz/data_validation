#source("/snfs2/HOME/gmanny/backups/Documents/Repos/data_validation/weighted_tabulations_by_string.R")

library(questionr)
library(magrittr)

w_prev <- function(dt = wash, num_strs = "imp", indic = "sdg", wt = "hhweight"){
  #num_strs is the strings that comprise your numerator, usually a combination of strings that match a specific sdg definition
  #indic is the column name that contains the indicator you want to tabulate
  #dt is the data.table (or data.frame) that you're tabulating from
  #wt is the column name that contains weights
  weight <- dt[, wt, with=F]
  denom_vec <- dt[, indic, with=F]
  denom <- wtd.table(x = denom_vec, weights=weight) %>% as.vector %>% sum(na.rm=T)
  num <- 0
  for (string in num_strs){
    col <- dt[, indic, with=F]
    num_part <- wtd.table(x=col, weights=weight)[string] %>% as.numeric
    num <- num + num_part
  }
  
  prevalence <- num/denom

  return(prevalence)
}
