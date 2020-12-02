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
source("./functions/make_return_file.R", echo = F)
source("./functions/LM_JumpTest_2012.R", echo = F)
source("./functions/AJ_JumpTest_2012.R", echo = F)
source("./functions/lapply_jump_test.R", echo = F)

#### load data #####
source("./load_data.R", echo = T)

#### evaluate by id ####
## split data.table ##
source("./split_by_id.R", echo = T)

## get result ##
LM_result_id <- jump_test(DT_split, LM_JumpTest)

fwrite(LM_result_id, file = "./data/JumpTestResult/DT_LM_result_id.csv")
## ##

#### evaluate by id_s ####
## split data.table ##
source("./split_by_id_s.R", echo = T)

## get result ##
LM_result_id_s <- jump_test(DT_split, LM_JumpTest)

fwrite(LM_result_id_s, file = "./data/JumpTestResult/DT_LM_result_id_s.csv")
## ##

