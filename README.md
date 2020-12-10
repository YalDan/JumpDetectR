[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **JumpDetectR** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : JumpDetectR

Published in : 'To be published in METIS'

Description : 'Scalable implementation of Lee / Mykland (2012) and Ait-Sahalia / Jacod (2012) Jump tests for noisy high frequency data'

Keywords : Jumps, jump test, high frequency, time series, Ait-Sahalia, Jacod, Lee, Mykland, stochastic processes, cryptocurrencies, cryptocurrency, crypto, spectrogram, microstructure, market microstructure noise, contagion, shocks

See also : 'Lee, S.S. and Mykland, P.A. (2012) Jumps in Equilibrium Prices and Market Microstructure Noise; Ait-Sahalia, Y. and Jacod, J. (2012) Analyzing the Spectrum of Asset Returns: Jump and Volatility Components in High Frequency Data'

Authors : Danial Florian Saef, Odett Nagy

Submitted : December 2 2020 by Danial Saef
```

### R Code
```r

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


```

automatically created on 2020-12-02