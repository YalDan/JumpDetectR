## install and load packages ##
libraries = c("data.table")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
## ##

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

## split data.table ##

# by id_s
if(! "q" %in% names(DT_agg_sub)) DT_agg_sub[, q := NA] # if no q reported
DT_split <- split(DT_agg_sub, by = c("date", "id", "s"))

# only keep time series that can be aggregated to at least 5 mins intervals
DT_split <- DT_split[which(lapply(DT_split,function(x) nrow(x) >= (0.75*86400/300)) == TRUE)] 
##

##
progress <- round(quantile(1:length(DT_split), probs = seq(0,1,0.05)))
##


## transform tick data files to regularly spaced return series w.r.t. number of observations##
#
delts <- c(1, 5, 10, 15, 20, 45, 60, 120, 300)
#

DT_split <- lapply(1:length(DT_split), function(x) {
  if (x %in% progress) {print(Sys.time()); print(progress[which(progress == x)])}
  DT_tmp <- DT_split[[x]]
  N <- nrow(DT_tmp)
  DT_ret <- data.table()
  
  if (N > 0.75*86400) {DT_ret <- make_return_file(DT_tmp, 1); return(DT_ret)} else {
    for (i in 1:length(delts)){
      d <- delts[i]
      if (N <= 0.75*86400*1/d) { DT_ret <-	make_return_file(DT_tmp, d)}  # if N is greater or equal to 75% of a c(1, 5, 10, 15, 20, 45, 60, 120, 300) interval convert the file to that frequency
    }
    return(DT_ret)
  }
})

rm(delts)
##


##
DT_split <- DT_split[which(lapply(DT_split,function(x) nrow(x) > 0) == TRUE)] # only keep non-empty list entries (yes, this discards all time series that are not "high frequency")
##