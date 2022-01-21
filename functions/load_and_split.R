## install and load packages ##
libraries = c("data.table")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
## ##

#### settings ####
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

## install and load packages ##
libraries = c("data.table")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
## ##

#### settings ####
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

load_and_split <- function(PATH){
  ### load aggregate dataset ###
  DT_agg_sub <- fread(PATH)
  ## ##
  
  #### evaluate by id ####
  ## split data.table ##
  DT_split_noimpute <- split_by_id(DT_agg_sub, IMPUTATION = FALSE)
  DT_split_impute <- split_by_id(DT_agg_sub, IMPUTATION = TRUE)
  DT_agg_split_noimpute <- rbindlist(DT_split_noimpute)
  DT_agg_split_impute <- rbindlist(DT_split_impute)
  
  return(list("DT_agg_split_noimpute" = DT_agg_split_noimpute, "DT_agg_split_impute" = DT_agg_split_impute))
}