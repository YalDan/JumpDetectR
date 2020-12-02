## install and load packages ##
libraries = c("data.table", "xts", "quantmod")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
if (!("qmao" %in% installed.packages())) {install.packages("qmao", repos="http://R-Forge.R-project.org")} 
library("qmao")
## ##

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

make_return_file <- function(DATA, FREQ){
  S <- DATA
  
  #
  start <- paste(as.Date(S[, t][1]), "00:00:00")
  end <- paste(as.Date(S[, t][1]), "23:59:59")
  #
  
  #
  ts_p <- xts(S$p, order.by=S$t, unique = FALSE)
  ts_p <- align.time(ts_p, n=FREQ)
  ts_p <- to.period(ts_p, period = "seconds", k = FREQ)
  
  #
  DT_ts_p <- data.table("index" = seq(from = as.POSIXct(start, tz="UTC"), to = as.POSIXct(end, tz="UTC"), by = paste(FREQ, " sec", sep = "")),
                        "date" = unique(S$date),
                        "id" = unique(S$id),
                        "s" = unique(S$s)
  ) 
  DT_ts_p[, index := as.POSIXct(index, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")]
  DT_ts_p[as.data.table(ts_p), "p" := i.ts_p.Open, on = "index"]
  DT_ts_p[, p := na.locf(p, na.rm = F)]
  DT_ts_p[, p := na.locf(p, na.rm = F, fromLast = T)]
  DT_ts_p[, log_ret := Delt(p, type = "log")]
  DT_ts_p[1, log_ret := 0]
  names(DT_ts_p)[1] <- "t"
  return(DT_ts_p)
}
