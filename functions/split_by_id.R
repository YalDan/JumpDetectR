## install and load packages ##
libraries = c("data.table")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
## ##

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

split_by_id <- function(DATA, IMPUTATION = FALSE, T_large = 1){
  
  ## split data.table ##
  
  # by id
  if(! "h" %in% names(DATA)) DATA[, h := hour(t)] # if no h available
  if(! "q" %in% names(DATA)) DATA[, q := NA] # if no q reported
  
  print(Sys.time());print("Splitting data")
  DT_split <- split(DATA[, .(date, "p" = mean(p), "q" = sum(q)), by = c("t", "s")], by = c("date", "s"))
  
  # only keep time series that can be aggregated to at least 5 mins intervals
  full_day <- 0.95*86400
  DT_split <- DT_split[which(lapply(DT_split,function(x) nrow(x) >= (full_day/15)) == TRUE)] 
  ##
  
  ##
  progress <- round(quantile(1:length(DT_split), probs = seq(0,1,0.05)))
  ##
  
  
  ## transform tick data files to regularly spaced return series w.r.t. number of observations##
  #
  delts <- c(5, 10, 15)
  #
  
  print("Making returns")
  
  DT_split <- lapply(1:length(DT_split), function(x) {
    if (x %in% progress) {print(Sys.time()); print(progress[which(progress == x)])}
    DT_tmp <- DT_split[[x]]
    if(! "h" %in% names(DT_tmp)) DT_tmp[, h := hour(t)] # if no h available
    
    N <- nrow(DT_tmp)
    DT_ret <- data.table()
    
    if (N >= full_day) {
      DT_ret <- make_return_file(DT_tmp, T_large, IMPUTE = IMPUTATION)
      DT_ret[, freq := 1] 
      DT_ret[, T_large := T_large]
      return(DT_ret)
    
      # if N is greater or equal to 75% of a c(5, 10, 15) interval convert the file to that frequency
    } else {
      for (i in 1:length(delts)){
        d <- delts[i]
        if (N >=full_day*1/d) { 
          DT_ret <-	make_return_file(DT_tmp, d, IMPUTE = IMPUTATION)
          DT_ret[, freq := d] 
          DT_ret[, T_large := T_large]
          d <- delts[1]
          return(DT_ret)
        }  
      }
    }
  })
  ##
  
  
  ## only keep non-empty list entries (yes, this discards all time series that are not "high frequency") ##
  ## do this in real analysis. In this minimal example we skip it
  # DT_split <- DT_split[which(lapply(DT_split,function(x) nrow(x) > full_day/15) == TRUE)] 
  ##
  
  print(Sys.time());print("Removing bounceback outliers")

  ## remove bouncebacks ##
  DT_split <- split(remove_bounceback(DT_split, IMPUTE = IMPUTATION), by = c("date", "s"))
  return(DT_split)
}
