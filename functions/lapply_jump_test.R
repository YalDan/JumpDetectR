## install and load packages ##
libraries = c("data.table")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
## ##

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##


jump_test <- function(DATA, which_test) {##
  
  DT_tmp <- DATA
  progress <- round(quantile(1:length(DT_tmp), probs = seq(0,1,0.05)))
  ##
  
  ## get result ##
  Test_result <- rbindlist(lapply(1:length(DT_tmp), function(x) {
    if (x %in% progress) {print(Sys.time()); print(progress[which(progress == x)])}
    which_test(DT_tmp[[x]])
  })
  )
  return(Test_result)
}
