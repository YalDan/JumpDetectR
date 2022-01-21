## install and load packages ##
libraries = c("data.table")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
## ##

#### settings ####
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

#### load functions #####
source("./functions/load_functions.R", echo = F)
#### ##

#### load aggregate dataset ####
list_DT_agg <- load_and_split("./data/raw/DT_agg_sub.csv")

## get LM result ##
DT_LM_result_id <- jump_test(DT_split_noimpute, which_test = "LM_JumpTest")

## get AJL result ##
DT_AJL_result_id <- jump_test(DT_split_impute, which_test = "AJL_JumpTest")

## save results ##
fwrite(DT_LM_result_id, file = "./data/JumpTestResult/DT_LM_result_id.csv")
fwrite(DT_AJL_result_id, file = "./data/JumpTestResult/DT_AJL_result_id.csv")
## ##