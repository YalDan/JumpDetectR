# install and load packages ##
libraries = c("data.table", "ggplot2", "scales", "xtable", "xts", "quantmod", "PerformanceAnalytics", "moments", "Quandl", "stringr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
# ##

#### settings ####
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
Sys.setlocale("LC_TIME", "English") # set timestamp language to English
###

#### Functions ####
source("./functions/remove_consecutive_jumps.R", echo = F)
source("./functions/remove_bounceback.R", echo = F)
source("./functions/split_by_id.R", echo = F)
source("./functions/make_return_file.R", echo = F)
source("./functions/plot_theme.R", echo = F)
source("./functions/prepare_files.R", echo = F)
###


#### Load data ####
DT_LM_result_id <- fread("./data/JumpTestResult/DT_LM_result_id.csv")
setkey(DT_LM_result_id, t)
DT_AJL_result_id_raw <- fread("./data/JumpTestResult/DT_AJL_result_id.csv")
DT_LM_result_id[, hour := hour(t)]

DT_agg_sub <- fread("./data/raw/DT_agg_sub.csv")
DT_split_noimpute <- split_by_id(DT_agg_sub, IMPUTATION = FALSE)
DT_split_impute <- split_by_id(DT_agg_sub, IMPUTATION = TRUE)
DT_agg_split_noimpute <- rbindlist(DT_split_noimpute)
DT_agg_split_impute <- rbindlist(DT_split_impute)
###

## side vars ##
summary_vars <- c("mean", "sd", "min", "p25", "median", "p75", "max")
event_dates <- c("2019-07-12", "2019-10-24", "2020-03-12", "2020-05-11", "2020-09-03", "2020-12-22")
###

#### Descriptive statistics ####
#### on dataset ####
start_date <- DT_agg_sub[!is.na(date), min(date)]
end_date <- DT_agg_sub[!is.na(date), max(date)]
unique_s <- DT_agg_sub[,unique(s)]
unique_s <- unique_s[which(unique_s %in% c("bchusd", "btcusd", "etcusd", "ethusd", "ltcusd", "xrpusd"))]
unique_id <- DT_agg_sub[,unique(id)]

#### N by s/id ####
DT_agg_split <- DT_agg_split_noimpute
N_by_id <- rbindlist(lapply(unique_id, function(x) DT_agg_sub[id %in% x & s %in% unique_s, .N, by = "id"]));setkey(N_by_id,N)
N_by_s_agg <- rbindlist(lapply(unique_s, function(x) DT_agg_sub[s %in% x, .N, by = "s"]));setkey(N_by_s_agg,N)
return_dataset <- DT_agg_sub[s %in% unique_s]
return_dataset <- return_dataset[, log_ret := quantmod::Delt(p, type = "log"), by = c("s", "date", "h")]
return_dataset <- return_dataset[!is.na(log_ret)] 

## Number of observations per symbol
N_by_s <- rbindlist(lapply(unique_s, function(x) return_dataset[s %in% x, .N, by = "s"]));setkey(N_by_s,N)
N <- nrow(return_dataset)

##
N_by_s_agg[, group := "raw"]
N_by_s[, group := "aggregated"]
DT_N <- rbind(N_by_s_agg, N_by_s)
DT_N[, pct_obs := N/sum(N)*100, by  = "group"]
DT_N[group == "aggregated", agg_vs_unagg := (DT_N[group == "aggregated", N]/DT_N[group == "raw", N])*100]
DT_N <- dcast(DT_N, s ~ group, value.var = c("N", "pct_obs", "agg_vs_unagg"))[, -c("agg_vs_unagg_raw")]
setcolorder(DT_N, c("s", "N_raw", "pct_obs_raw", "N_aggregated", "pct_obs_aggregated", "agg_vs_unagg_aggregated"))
setkey(DT_N, N_raw)
names(DT_N) <- c("Symbol", "% N (raw)" , "N (raw)", "N (aggregated)", "% N (agg.)", "% N vs raw")

## LaTeX output ##
print(xtable(DT_N, caption = "N obs. per symbol"), format = "latex",
      format.args = list(decimal.mark = ".", big.mark = ","), include.rownames=FALSE,
      hline.after = c(-1,-1,0,nrow(DT_N)))

## Generate log returns ##
return_dataset[, log_ret := quantmod::Delt(p, type = "log"), by = c("s", "date", "h")]

#### N obs raw data ####
N_obs_per_day_raw <- lapply(unique_s, function(x){
  tmp_plot <- plot(x = DT_agg_sub[s %in% x, .N, by = "date"]$date, y = DT_agg_sub[s %in% x, .N, by = "date"]$N, main = x, type = "h")
})

#### N obs per time series ####
N_obs_per_day <- lapply(unique_s, function(x){
  tmp_plot <- plot(x = DT_agg_split[s %in% x, .N, by = "date"]$date, y = DT_agg_split[s %in% x, .N, by = "date"]$N, main = x, type = "h")
})


#### price of the assets ####
p_ts_list <- lapply(unique_s, function(x)  return_dataset[s %in% x, c("p", "t")])
names(p_ts_list) <- unique_s

## create xts objects
p_xts_list <- lapply(p_ts_list, function(x) {
  xts_tmp <- xts(x[, p], order.by=x[, t], unique = FALSE)
  xts_tmp <- to.period(xts_tmp, period = "seconds", k = 1)
})
names(p_xts_list) <- unique_s


#### returns of the assets ####
ret_ts_list <- lapply(unique_s, function(x) return_dataset[!is.na(log_ret) & s %in% x, c("log_ret", "t")])
names(ret_ts_list) <- unique_s

## create xts objects
ret_xts_list <- lapply(ret_ts_list, function(x) {
  xts_tmp <- xts(x[, log_ret], order.by=x[, t], unique = FALSE)
  xts_tmp <- to.period(xts_tmp, period = "seconds", k = 1)
})
names(ret_xts_list) <- unique_s

# daily returns ##
p_ts_list_daily <- lapply(unique_s, function(x) DT_agg_sub[!is.na(p) & s %in% x, c("p", "t")])
names(p_ts_list_daily) <- unique_s

ret_xts_list_daily <- lapply(p_ts_list_daily, function(x) {
  xts_tmp <- xts(x[, p], order.by=x[, t], unique = FALSE)
  xts_tmp <- to.period(xts_tmp, period = "days", k = 1)
})
names(ret_xts_list_daily) <- unique_s

DT_xts_list_daily <- lapply(ret_xts_list_daily, function(x) {
  DT_tmp <- data.table("date" = as.Date(index(x)), x)
  DT_tmp[,log_ret := Delt(xts_tmp.Close, type = "log")]
  DT_tmp[, diff_date := c(0,diff(date))]
  names(DT_tmp)[2:5] <- c("open", "high", "low", "close")
  return(DT_tmp)
})
DT_xts_list_daily <- rbindlist(DT_xts_list_daily, idcol = "s")


#### summary statistics ####

sum_ret_all <- lapply(seq_along(unique_s), function(x){
  sum_ret_btc <- summary(ret_ts_list[[x]][, log_ret])
  skew_ret_btc <- skewness(ret_ts_list[[x]][, log_ret])
  kurt_ret_btc <- kurtosis(ret_ts_list[[x]][, log_ret])
  
  DT_tmp <- data.table(c(as.numeric(sum_ret_btc), skew_ret_btc, kurt_ret_btc))
  DT_tmp[, statistic := c(names(summary(1)), "Skewness", "Kurtosis")]
  names(DT_tmp)[1] <- unique_s[x]
  DT_tmp <- transpose(DT_tmp, make.names = "statistic", keep.names = "statistic")
  names(DT_tmp)[1] <- "Currency"
  return(DT_tmp)
})
sum_ret_all <- rbindlist(sum_ret_all)
print(xtable(sum_ret_all), wide.table = T)
DT_ret_ts_list <- rbindlist(ret_ts_list,idcol = "s")

## Output extreme returns ##
DT_ret_ts_list[log_ret < -0.05, .N]
DT_ret_ts_list[log_ret < -0.1, .N]
DT_ret_ts_list[log_ret < -0.2, .N]
DT_ret_ts_list[log_ret < -0.3, .N]
DT_ret_ts_list[log_ret > 0.05, .N]
DT_ret_ts_list[log_ret > 0.1, .N]
DT_ret_ts_list[log_ret > 0.2, .N]
DT_ret_ts_list[log_ret > 0.3, .N]



#### Daily basis ####
sum_ret_all_daily <- lapply(seq_along(unique_s), function(x){
  DT_tmp <- DT_xts_list_daily[s %in% unique_s[x] & diff_date == 1]
  sum_ret_btc <- summary(DT_tmp[, log_ret])
  skew_ret_btc <- skewness(DT_tmp[, log_ret])
  kurt_ret_btc <- kurtosis(DT_tmp[, log_ret])
  
  DT_tmp <- data.table(c(as.numeric(sum_ret_btc), skew_ret_btc, kurt_ret_btc))
  DT_tmp[, statistic := c(names(summary(1)), "Skewness", "Kurtosis")]
  names(DT_tmp)[1] <- unique_s[x]
  DT_tmp <- transpose(DT_tmp, make.names = "statistic", keep.names = "statistic")
  names(DT_tmp)[1] <- "Currency"
  return(DT_tmp)
})
sum_ret_all_daily <- rbindlist(sum_ret_all_daily)
print(xtable(sum_ret_all), wide.table = T)

#### Daily extreme returns ####
DT_xts_list_daily[log_ret < -0.05, .N]
DT_xts_list_daily[log_ret < -0.1, .N]
DT_xts_list_daily[log_ret < -0.2, .N]
DT_xts_list_daily[log_ret < -0.3, .N]
DT_xts_list_daily[log_ret > 0.05, .N]
DT_xts_list_daily[log_ret > 0.1, .N]
DT_xts_list_daily[log_ret > 0.2, .N]
DT_xts_list_daily[log_ret > 0.3, .N]

#### PLOT DAILY TIME SERIES ####
plot_all_daily <- ggplot(DT_xts_list_daily, aes(x=date, y=log(close), group = s, color =s))+
  geom_line(size = 1)+
  plot_theme +
  scale_x_date( expand=c(0.01,0.01),  labels = date_format("%b %Y")) +
  labs(x = "Month", y = "Price (log)",
       title = element_blank())
plot_all_daily <- plot_all_daily +
  geom_vline(xintercept=as.Date(event_dates), color = "red")

#### load functions #####
Quandl.api_key("MYKEY")
btc_quandl <- Quandl("BCHAIN/MKPRU", type = "xts")
btc_p_11_13 <- btc_quandl['2011-06-25/2013-11-30']
btc_p_19_21 <- btc_quandl['2019-04-12/2021-02-08']

btc_quandl_volume <- Quandl("BCHAIN/TRVOU", type = "xts")
btc_vol_11_13 <- btc_quandl_volume['2011-06-25/2013-11-30']
btc_vol_19_21 <- btc_quandl_volume['2019-04-12/2021-02-08']

btc_ret_11_13 <- CalculateReturns(btc_p_11_13,method="log")
btc_ret_11_13[1,] <- 0
btc_ret_19_21 <- CalculateReturns(btc_p_19_21,method="log")
btc_ret_19_21[1,] <- 0
btc_realizedvol_11_13 <- xts(apply(btc_ret_11_13,2,runSD,n=30), index(btc_ret_11_13))*sqrt(252)
btc_realizedvol_19_21 <- xts(apply(btc_ret_19_21,2,runSD,n=30), index(btc_ret_19_21))*sqrt(252)

#### daily returns ####
DT_btc_ret_19_21 <- data.table("date" = index(btc_ret_19_21), "log_ret" = as.numeric(btc_ret_19_21))
sum_ret_btc <- summary(DT_btc_ret_19_21[, log_ret])
skew_ret_btc <- skewness(DT_btc_ret_19_21[, log_ret])
kurt_ret_btc <- kurtosis(DT_btc_ret_19_21[, log_ret])

summary_btc_ret_19_21 <- data.table(c(as.numeric(sum_ret_btc), skew_ret_btc, kurt_ret_btc))
summary_btc_ret_19_21[, statistic := c(names(summary(1)), "Skewness", "Kurtosis")]
names(summary_btc_ret_19_21)[1] <- "BTC"
summary_btc_ret_19_21 <- transpose(summary_btc_ret_19_21, make.names = "statistic", keep.names = "statistic")
names(summary_btc_ret_19_21)[1] <- "Currency"


#### Plots ####
plot_btc_quandl <- autoplot(btc_quandl) + plot_theme + labs(x ="Date", y = "Price(USD)", title = "Bitcoin price")
plot_btc_p_11_13 <- autoplot(btc_p_11_13) + plot_theme + labs(x ="Date", y = "Price(USD)", title = "Bitcoin price 2011-2013")
plot_btc_p_19_21 <- autoplot(btc_p_19_21) + plot_theme  + labs(x ="Date", y = "Price(USD)", title = "Bitcoin price 2019-2021")
plot_btc_vol_11_13 <- autoplot(btc_vol_11_13, type = "h") + plot_theme + labs(x ="Date", y = "Volume(USD)", title = "Bitcoin volume 2011-2013")
plot_btc_vol_19_21 <- autoplot(btc_vol_19_21, type = "h") + plot_theme + labs(x ="Date", y = "Volume(USD)", title = "Bitcoin volume 2019-2021")
plot_btc_realizedvol_11_13 <- autoplot(btc_realizedvol_11_13) + plot_theme + labs(x ="Date", y = "Realized Volatility (%)", title = "30 days Bitcoin volatility 2011-2013")
plot_btc_realizedvol_19_21 <- autoplot(btc_realizedvol_19_21) + plot_theme + labs(x ="Date", y = "Realized Volatility (%)", title = "30 days Bitcoin volatility 2019-2021")

## add important events ##
plot_btc_realizedvol_19_21 <- plot_btc_realizedvol_19_21 + geom_vline(xintercept=as.Date(event_dates), color = "red")
plot_btc_vol_19_21 <- plot_btc_vol_19_21 + geom_vline(xintercept=as.Date(event_dates), color = "red")
plot_btc_p_19_21 <- plot_btc_p_19_21 + geom_vline(xintercept=as.Date(event_dates), color = "red")

#### Get jump statistics ####
sign_level <- 0.001
dataset <- prepare_files(DT_LM_result_id[s %in% unique_s], sign.level = sign_level, bonferroni = TRUE, asymptotic_variation = FALSE)
dataset[, weekday := weekdays(as.Date(date))]

## filter out consecutive jumps? ##
dataset <- remove_consecutive_jumps(dataset, RANGE_OBS = 10)
## ##

## bonferroni correction ##
DT_AJL_result_id <- copy(DT_AJL_result_id_raw)
DT_AJL_result_id[DT_AJL_result_id[,.N,by="s"], n_obs_sample := i.N, on = "s"] # add number of observations per sample
DT_AJL_result_id[,Jump_indicator := NA] # add jump indicator
DT_AJL_result_id[, sign_level := sign_level] # add sign.level
DT_AJL_result_id[, sign_level := sign_level/n_obs_sample] # bonferroni correction
DT_AJL_result_id[, cv := qnorm(1-sign_level/2)] # add critical value
DT_AJL_result_id[!S_stdc %between% list(-1*cv,cv), Jump_indicator := 1] # add rejected values
DT_AJL_result_id[is.na(Jump_indicator), Jump_indicator := 0] # add not rejected values

###
DT_AJL_result_id[,date:=as.character(date)]
dataset[DT_AJL_result_id[Jump_indicator == 1], AJL_Jump_indicator := i.Jump_indicator, on = c("s", "date")] # merge LM & AJL datasets
dataset <- dataset[Jump_indicator != 0 & AJL_Jump_indicator == 1] # keep only those jumps detected by both
###

### create jump + return dataset ####
test_dates <- DT_LM_result_id[s %in% unique_s, unique(date), by  = "s"]
names(test_dates)[which(names(test_dates) == "V1")] <- "date"

#### on jumps ####
## Look at jumps per weekday and hour ##
N_jumps_by_s <- dataset[, unique(date), by  = "s"][,.N, by = "s"]; setkey(N_jumps_by_s, N)
N_jumps_by_h <- dataset[,.N, by = "hour"]; setkey(N_jumps_by_h, hour)

## Number of test days per symbol #
N_test_days_by_s <- DT_LM_result_id[s %in% unique_s, unique(date), by  = "s"][,.N, by = "s"]; setkey(N_test_days_by_s, N)
##

days <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
          "Friday", "Saturday", "Sunday")
N_jumps_by_day <- dataset[,.N, by = "weekday"]; setkey(N_jumps_by_day, N)

## Do this if dates are in German ##
if ("Monday" %in% N_jumps_by_day$weekday) {
  N_jumps_by_day[, weekday := ordered(weekday, levels=days)]
  } else if ("Montag" %in% N_jumps_by_day$weekday){
  N_jumps_by_day[, weekday := ordered(weekday, levels=c("Montag", "Dienstag", "Mittwoch", "Donnerstag", 
                                                      "Freitag", "Samstag", "Sonntag"))];setkey(N_jumps_by_day, weekday)
  N_jumps_by_day[, weekday := as.factor(days)]
  }

#### Jump size ####
dataset[L_t_j < -0.025, .N]
dataset[L_t_j < -0.05, .N]
dataset[L_t_j < -0.1, .N]
dataset[L_t_j < -0.2, .N]
dataset[L_t_j > 0.025, .N]
dataset[L_t_j > 0.05, .N]
dataset[L_t_j > 0.1, .N]
dataset[L_t_j > 0.2, .N]

## Percentage of extreme jumps observed ##
N_obs_dataset <- DT_LM_result_id[s %in% unique_s,.N]
dataset[L_t_j < -0.025, .N] / N_obs_dataset * 100
dataset[L_t_j < -0.05, .N] / N_obs_dataset * 100
dataset[L_t_j < -0.1, .N] / N_obs_dataset * 100
dataset[L_t_j < -0.2, .N] / N_obs_dataset * 100
dataset[L_t_j > 0.025, .N] / N_obs_dataset * 100
dataset[L_t_j > 0.05, .N] / N_obs_dataset * 100
dataset[L_t_j > 0.1, .N] / N_obs_dataset * 100
dataset[L_t_j > 0.2, .N] / N_obs_dataset * 100

## 
plot_jumps_by_h <- ggplot(N_jumps_by_h, aes(x = hour, y=N)) +
  geom_line(size=2) +
  labs(x ="Hour", y = "Jumps", title = "Jumps per hour 2019-2021") +
  plot_theme 

plot_jumps_by_day <- ggplot(N_jumps_by_day, aes(x = weekday, y=N, group = 1)) +
  geom_line(size=2) +
  labs(x ="Weekday", y = "Jumps", title = "Jumps per weekday 2019-2021") +
  plot_theme + theme (axis.text.x = element_text(angle = 45))

## Number of jumps by symbol ##
jumps_by_s <- dataset[Jump_indicator != 0, .N, by  = c("s")]; setkey(jumps_by_s, N)

#### jump statistic dataset on CCs ##

jump_stats <- rbindlist(list("test_days" = N_test_days_by_s, "jumps" = jumps_by_s), idcol = "Statistic")
jump_stats <- dcast(jump_stats, s ~ Statistic, value.var = "N")
jump_stats[, pct_jumps := (jumps / test_days)*100]
setkey(jump_stats, jumps)

#### On L_t_j ####

## Histogram of jump sizes ##
jump_ltj <- ggplot(dataset[,c("L_t_j")], aes(x=L_t_j)) + geom_histogram(bins = 200, size = 5) + plot_theme 
jump_ltj <- jump_ltj +
  labs(x = "Frequency", y = "Size of jumps",
       title = "Size of jumps | all time series") 

### preaveraged returns ###
sum_preav_ret_all <- lapply(seq_along(unique_s), function(x){
  sum_ret_btc <- summary(DT_LM_result_id[s %in% unique_s[x], L_t_j])
  skew_ret_btc <- skewness(DT_LM_result_id[s %in% unique_s[x], L_t_j])
  kurt_ret_btc <- kurtosis(DT_LM_result_id[s %in% unique_s[x], L_t_j])
  
  DT_tmp <- data.table(c(as.numeric(sum_ret_btc), skew_ret_btc, kurt_ret_btc))
  DT_tmp[, statistic := c(names(summary(1)), "Skewness", "Kurtosis")]
  names(DT_tmp)[1] <- unique_s[x]
  DT_tmp <- transpose(DT_tmp, make.names = "statistic", keep.names = "statistic")
  names(DT_tmp)[1] <- "Currency"
  return(DT_tmp)
})
sum_preav_ret_all <- rbindlist(sum_preav_ret_all)
print(xtable(sum_preav_ret_all, digits = 4), wide.table = T)
###

### Collect jump sizes ###
jump_size <- dataset[,L_t_j]
jump_size_pos <- dataset[L_t_j >0,L_t_j]
jump_size_neg <- dataset[L_t_j <0,L_t_j]
list_jump_size <- list("All" = jump_size, "Positive" = jump_size_pos, "Negative" = jump_size_neg)

### Create summary ###
sum_jumps <- lapply(seq_along(list_jump_size), function(x){
  sum_ret_btc <- summary(list_jump_size[[x]])
  skew_ret_btc <- skewness(list_jump_size[[x]])
  kurt_ret_btc <- kurtosis(list_jump_size[[x]])
  
  DT_tmp <- data.table(c(length(list_jump_size[[x]]), as.numeric(sum_ret_btc), skew_ret_btc, kurt_ret_btc))
  DT_tmp[, statistic := c("N", names(summary(1)), "Skewness", "Kurtosis")]
  names(DT_tmp)[1] <- names(list_jump_size)[x]
  DT_tmp <- transpose(DT_tmp, make.names = "statistic", keep.names = "statistic")
  names(DT_tmp)[1] <- "Statistic"
  return(DT_tmp)
})
sum_jumps <- rbindlist(sum_jumps)
sum_jumps <- transpose(sum_jumps, make.names = "Statistic", keep.names = "Statistic")
print(xtable(sum_jumps, caption = "Summary of jump sizes"), format = "latex",
      format.args = list(decimal.mark = ".", big.mark = ","), include.rownames=FALSE,
      hline.after = c(-1,-1,0,nrow(DT_N)))


#### Jump plot ####
## do this for integer labels ##
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

## generate plotting dataset ##
LM_jumps <- dataset[, .N, by  = list(month(as.Date(date)), year(as.Date(date)))]
LM_jumps <- dataset[, .N, by  = date]

## generate plot ##
plot_lm_jump <- ggplot(LM_jumps, aes(x=as.Date(date),xend=as.Date(date),y=0,yend=N)) +
  geom_segment(size = 1, color = "black") +
  geom_point(LM_jumps, mapping = aes(x= as.Date(date), y = N), size = 2, color = "black") +
  plot_theme +
  labs(x = "Month", y = "Number of jumps",
       title = element_blank()) +
  scale_y_discrete(limits = factor(1:LM_jumps[,max(N)]), breaks = every_nth(2)) +
  scale_x_date( expand=c(0.01,0.01),  labels = date_format("%b %Y")) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.position = "none")# get rid of legend panel bg

## plot jumps with important events ##
plot_lm_jump + geom_vline(xintercept=as.Date(event_dates), color = "red")
