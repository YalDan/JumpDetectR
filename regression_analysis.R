# install and load packages ##
libraries = c("data.table", "texreg", "plm", "sandwich", "lmtest")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
# ##

#### settings ####
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
Sys.setlocale("LC_TIME", "English") # set timestamp language to English
## ##

#### load data ####
DT_crypto_jumps <- fread("./data/JumpTestResult/dataset.csv")
DT_crypto_jumps[,date := as.character(date)]
DT_crypto_prices_daily <- fread("./data/raw/DT_crypto_prices_daily.csv")[diff_date == 1]
DT_crypto_prices_daily[,date := as.character(date)]
test_dates <- fread("./data/JumpTestResult/test_dates.csv")
test_dates[,date := as.character(date)]
DT_LM_result_id <- fread("./data/JumpTestResult/DT_LM_result_id.csv")
DT_LM_result_id[,date := as.character(date)]

#### join daily data ####
test_dates[DT_crypto_prices_daily, `:=` (p = i.close, log_ret = i.log_ret), on = c("s", "date")]
test_dates[unique(DT_LM_result_id, by = c("s", "date")), `:=` (sigmahat = i.sigmahat,
                                                               Vn = i.Vn,
                                                               plim_Vn = i.plim_Vn,
                                                               qhat = i.qhat), on = c("s", "date")]
test_dates[DT_crypto_jumps, `:=` (Jump_indicator = i.Jump_indicator,
                                  jump_size = i.L_t_j), on = c("s", "date")]
test_dates[is.na(Jump_indicator), Jump_indicator := 0]
test_dates[, weekday := weekdays(as.Date(date))] 
test_dates[, month := months.Date(as.Date(date))]
test_dates[, quarter := lubridate::quarter(test_dates$date, with_year = T)]
test_dates[, jump_dummy := 0]
test_dates[, pos_jump_dummy := 0]
test_dates[, neg_jump_dummy := 0]
test_dates[Jump_indicator != 0, jump_dummy := 1]
test_dates[Jump_indicator == 1, pos_jump_dummy := 1]
test_dates[Jump_indicator == -1, neg_jump_dummy := 1]
test_dates[, c("jump_dummy", "pos_jump_dummy", "neg_jump_dummy") := lapply(.SD, as.factor), .SDcols = c("jump_dummy", "pos_jump_dummy", "neg_jump_dummy")]
test_dates[,date:=as.Date(date)]


### make pdataframe ####
daily_joined <- pdata.frame(test_dates, index = c("s", "date"))

#### plm regressions ####

##jump --> log return
plmreg0_re <- plm(log_ret ~ jump_dummy, data=daily_joined, model = "random", effect = "individual")
plmreg0_fe <- plm(log_ret ~  jump_size, data=daily_joined, model = "within", effect = "individual")
summary(plmreg0_fe)

plmreg0 <- plm(log_ret ~ jump_dummy, data=daily_joined, model = "within", effect = "individual")
summary(plmreg0)
plmreg0_robust <- coeftest(plmreg0,vcov=vcovHC(plmreg0,type="HC3",cluster="group"))

sum_reg0 <- extract(plmreg0)
sum_reg0@pvalues <- plmreg0_robust[, 4]
sum_reg0@se <- plmreg0_robust[, 2]

## pos jump --> log return
plmreg0_re <- plm(log_ret ~ pos_jump_dummy, data=daily_joined, model = "random", effect = "individual")
plmreg0_fe <- plm(log_ret ~ pos_jump_dummy, data=daily_joined, model = "within", effect = "individual")

plmreg_pos <- plm(log_ret ~ pos_jump_dummy, data=daily_joined, model = "within", effect = "individual")
summary(plmreg_pos)
plmreg_pos_robust <- coeftest(plmreg_pos,vcov=vcovHC(plmreg_pos,type="HC3",cluster="group"))

sum_reg_pos <- extract(plmreg_pos)
sum_reg_pos@pvalues <- plmreg_pos_robust[, 4]
sum_reg_pos@se <- plmreg_pos_robust[, 2]

## neg jump --> log return
plmreg_neg_re <- plm(log_ret ~ neg_jump_dummy, data=daily_joined, model = "random", effect = "individual")
plmreg_neg_fe <- plm(log_ret ~ neg_jump_dummy, data=daily_joined, model = "within", effect = "individual")

plmreg_neg <- plm(log_ret ~ neg_jump_dummy, data=daily_joined, model = "within", effect = "individual")
summary(plmreg_neg)
plmreg_neg_robust <- coeftest(plmreg_neg,vcov=vcovHC(plmreg_neg,type="HC3",cluster="group"))

sum_reg_neg <- extract(plmreg_neg)
sum_reg_neg@pvalues <- plmreg_neg_robust[, 4]
sum_reg_neg@se <- plmreg_neg_robust[, 2]

##jump --> shifted log return
plmreg1 <- plm(shift(log_ret, 1, type = "lead") ~ jump_dummy, data=daily_joined, model = "within", effect = "individual")
summary(plmreg1)
plmreg1_robust <- coeftest(plmreg1,vcov=vcovHC(plmreg1,type="HC3",cluster="group"))
sum_reg1 <- extract(plmreg1)
sum_reg1@pvalues <- plmreg1_robust[, 4]
sum_reg1@se <- plmreg1_robust[, 2]

## output table ##
texreg(list(sum_reg0, plmreg1, sum_reg_pos, sum_reg_neg),
       caption="Relationship of jumps and daily returns",
       caption.above = TRUE,
       dcolumn=TRUE,
       digits = 3,
       custom.model.names=c("Jumps (all)","Lagged jumps (all)", "Jumps (pos.)", "Jumps (neg.)"),
       label = "table:reg_jumps")

