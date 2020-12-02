## install and load packages ##
libraries = c("data.table", "fasttime")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
## ##

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

##
files <- list.files(path = "./data/DT_agg_sub", pattern = "*.csv")
##

##
setDTthreads(percent = 100)
DT_agg_sub <- rbindlist(lapply(files, function(x) {
  print(Sys.time())
  print(paste("Reading file ", x))
  cbind(fread(paste("./data/DT_agg_sub/", x, sep = "")))
}),
idcol = FALSE)
DT_agg_sub[, t := fastPOSIXct(t, tz = "UTC")]
DT_agg_sub <- DT_agg_sub[!(id %in% "hitbtc" & s %in% "bchusd")]
setkey(DT_agg_sub, date, t, id, s)
##