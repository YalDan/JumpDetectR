#' to use when p,k different from range [4,6]
#' @importFrom stats rnorm
#' @keywords internal
mukp <- function(p, k, t = 1000000) {
  p <- as.numeric(p)
  k <- as.numeric(k)
  
  U <- rnorm(t)
  Y <- rnorm(t)
  absU <- abs(U)
  mukp <- mean((absU^p)*(abs(U +sqrt(k-1)*Y))^p)
  return(mukp)
}

##fmupk: to use to calculate mupk in the function of the author.
#' @keywords internal
fmupk <- function(p,k){
  mupk <- NULL;
  if (p == 2) {
    mupk <- switch(as.character(k),
                   "2" = 4.00,
                   "3" = 5.00,
                   "4" = 6.00)
  }
  if (p == 3) {
    mupk <- switch(as.character(k),
                   "2" = 24.07,
                   "3" = 33.63,
                   "4" = 43.74)
  }
  if (p == 4) {
    mupk <- switch(as.character(k),
                   "2" = 204.04,
                   "3" = 320.26,
                   "4" = 455.67)
  }
  if (is.null(mupk)) {
    # reduce simulation error by taking large T and large nrep
    nrep <- 100
    vmupk <- rep(NA, times = nrep)
    
    for (i in 1:nrep) {
      vmupk[i] <- mukp(p, k, t = 1000000)
    }
    mupk <- round(mean(vmupk),2)
  }
  return(mupk)
}


calculateNpk <- function(p,k){
  mup= 2^(p/2)*gamma(1/2*(p+1))/gamma(1/2)
  mu2p= 2^((2*p)/2)*gamma(1/2*((2*p)+1))/gamma(1/2)
  npk= (1/mup^2)*(k^(p-2)*(1+k)*mu2p + k^(p-2)*(k-1)*mup^2-2*k^(p/2-1) * fmupk(p,k))
  return(npk)
}

#' @keywords internal
calculateV <- function(rse, p, k, N){
  mup <- 2^(p/2)*gamma(1/2*(p+1))/gamma(1/2)
  mu2p <- 2^(p)*gamma(1/2*(2*p+1))/gamma(1/2)
  Ap <- (1/N)^(1-p/2)/mup*sum(rse^p)
  A2p = (1/N)^(1-p)/mu2p*sum(rse^(2*p))
  V <- calculateNpk(p,k) *A2p/(N*Ap^2)
  return(V)
}

## install and load packages ##
libraries = c("data.table")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
## ##

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

test_AJ_JumpTest <- function(DATA, alpha = 0.1){
  
  # will loop to compute the values of B(p,u,delta) for all those values below
  pvec <- sort(unique(c(seq(from = 0, to = 6, by = 0.25),seq(from = 0, to = 6, by = 0.25)*2)))

  # atrunc is expressed in terms of numbers of stdev of the continuous part;  use 10^10 for no truncation
  atruncvec <- c(2:20, 25, 30, 40, 50, 60, 75, 100, 10^10)
  
  # specify possible gammas
  gammavec <- seq(from = 1, to = 3, by = 0.25)
  
  # specify possible deltas; for simplicity of coding, make sure those are multiples of timeinterval = 5 in the dataset
  deltavec <- c(1, 5, 10, 15, 30, 45, 60, 120, 300, 600, 1800) 
  
  # specify possible ks
  kvec <- 1:3
  
  # read data
  tmp_DT <- DATA
  
  # extract ID if available
  id_dummy <- NA
  if ("id" %in% names(tmp_DT)) {id_dummy <- tmp_DT[,id][1]}
  
  
  if (nrow(tmp_DT) == 86400){
    subs <- seq(0,nrow(tmp_DT), by = 5)
    subs[1] <- 1
    tmp_DT <- tmp_DT[subs]
  }
  
  dX <- tmp_DT[!is.na(log_ret), log_ret] # both X and dX have length n
  x0 <- log(tmp_DT[!is.na(log_ret), p][1]) # initial value
  
  n <- length(dX)
  T_large <- 1/365.25
    
  if(n < 86400/5) {deltavec <- deltavec[2:length(deltavec)]}
  if(n < 86400/10) {deltavec <- deltavec[3:length(deltavec)]}
  if(n < 86400/15) return(data.table())
  
  
  par_grid <- setDT(expand.grid("p" = pvec,
                                "a" = atruncvec,
                                "gamma" = gammavec,
                                "delta" = deltavec,
                                "k" = kvec))
  
  par_grid[, nblag_j := delta/sort(unique(par_grid$delta)[1])]
  par_grid[, delta_j := delta/(24*60*60*365.25)]
  
  N_nblagj <- sort(unique(par_grid$nblag_j))
  
  
  for (i in seq_along(N_nblagj)){
    X <- x0 + cumsum(dX) # do this instead of X=log(price) to avoid including the large overnight returns
    nblagj <- unique(par_grid[nblag_j %in% sort(unique(par_grid$nblag_j))[i]]$nblag_j)
    deltaj <-  unique(par_grid[nblag_j %in% sort(unique(par_grid$nblag_j))[i]]$delta_j)
    dXobsj <- sort(abs(X[seq(from = (nblagj+1), to = n, by = nblagj)] - X[seq(from = 1, to = (n-nblagj), by = nblagj)])) # length(dXobsj) is equal to nj-1
    sigma_hat <- sqrt( (1/T_large) * sum( (abs(dXobsj)^2) * ( abs(dXobsj) <= 3 * 0.6 * deltaj^(1/2) )) )
    par_grid[nblag_j %in% nblagj, sigmahat := sigma_hat]
  }
  
  par_grid[, nblag_jk := nblag_j*k]
  par_grid[, thresh := gamma * a * sigmahat * sqrt(delta_j)]
  N_nblagjk <- sort(unique(par_grid$nblag_jk))
  
  list_dXobsjk <- lapply(seq_along(N_nblagjk), function(i){
    nblagjk <- unique(par_grid[nblag_jk %in% sort(unique(par_grid$nblag_jk))[i]]$nblag_jk)
    deltaj <-  unique(par_grid[nblag_jk %in% sort(unique(par_grid$nblag_jk))[i]]$delta_j)
    
    sort(abs(X[seq(from = (nblagjk+1), to = n, by = nblagjk)] - X[seq(from = 1, to = (n-nblagjk), by = nblagjk)]))
  })
  
  result_list <- vector(mode="list", length = length(N_nblagjk))
  
  for (i in seq_along(list_dXobsjk)){
    sa_dX_i <- list_dXobsjk[[i]]
    
    par_grid_tmp <- par_grid[nblag_jk %in% N_nblagjk[i]]
    
    test1 <- rep(NA, nrow(par_grid_tmp))
    loc <- findInterval(par_grid_tmp$thresh, sa_dX_i)
    loc[loc == 0] <- NA  # Handle threshold smaller than everything in dX_i
    
    for (pval in unique(par_grid_tmp$p)) {
      this.p <- par_grid_tmp$p == pval
      cs_dX_i_p <- cumsum(sa_dX_i^pval)
      test1[this.p] <- cs_dX_i_p[loc[this.p]]
    }
    test1[is.na(test1)] <- 0 
    
    par_grid_tmp[,B := test1]
    result_list[[i]] <- par_grid_tmp
  }
  
  ## get Npk values ##
  ks <- 2:3
  ps <- 2:4
  
  Npk_list <- lapply(ks, function(k) sapply(ps, function(p) calculateNpk(p,k)))
  names(Npk_list) <- as.character(ks)
  DT_npk <- rbindlist(lapply(Npk_list, function(x) data.table(t(data.table(x)))), idcol = "k")
  names(DT_npk) <- c("k", as.character(ps))
  DT_npk <- melt(DT_npk, id.vars = "k")
  names(DT_npk)[2] <- "p"  
  DT_npk[, `:=` ("k" = as.numeric(k), "p" = as.numeric(as.character(p))) ]
  ## 

  ### calculate SJ ###
  pvec_SJ <- pvec[which( (pvec >= 2.5 & pvec <= 6) )]
  p2vec_SJ <- pvec[which( (pvec >= 2.5*2 & pvec <= 6*2) )]
  deltavec_SJ <- deltavec[which(deltavec <= 120)]
  kvec_SJ <- kvec[which(kvec >= 2)]
  
  SJ <- rbindlist(result_list)[gamma == gammavec[1] & a == max(par_grid$a) & p %in% pvec_SJ & k %in%  kvec_SJ & delta %in% deltavec_SJ]
  B_lower <- rbindlist(result_list)[gamma == gammavec[1] & a == max(par_grid$a)& p %in% pvec_SJ & k == kvec[1] & delta %in% deltavec_SJ]
  B_p2 <- rbindlist(result_list)[gamma == gammavec[1] & a == max(par_grid$a)& p %in% p2vec_SJ & k %in% kvec_SJ & delta %in% deltavec_SJ]
  B_p2[, p_match := p/2]

  for (i in unique(kvec_SJ)){
    SJ[k == i, SJ := B / B_lower$B]
  }
  ###
  SJ[B_p2, B_p2 := i.B, on = c("p" = "p_match")]
  SJ[DT_npk, Npk:=i.value, on = c("k", "p")]
  SJ[, V := Npk*(B_p2/B^2)]
  
  ### calculate SFA ###
  pvec_SFA <- pvec[which( (pvec >= 2.5 & pvec <= 6) )]
  p2vec_SFA <- pvec[which( (pvec >= 2.5*2 & pvec <= 6*2) )]
  atruncvec_SFA <- atruncvec[which( (atruncvec > 5 & atruncvec < 10) )]
  deltavec_SFA <- deltavec[which(deltavec <= 120)]
  kvec_SFA <- kvec[which(kvec >= 2)]
  
  # B[[pvec_SFA[pindex],atruncvec_SFA[aindex],1,deltavec_SFA[dindex],kvec_SFA[kindex]]] / B[[pvec_SFA[pindex],atruncvec_SFA[aindex],1,deltavec_SFA[dindex],1]]
  SFA <- rbindlist(result_list)[gamma == gammavec[1] & a %in% atruncvec_SFA  & p %in% pvec_SFA & k %in%  kvec_SFA & delta %in% deltavec_SFA]
  B_lower <- rbindlist(result_list)[gamma == gammavec[1] & a %in% atruncvec_SFA  & p %in% pvec_SFA & k == kvec[1]  & delta %in% deltavec_SFA]
  B_p2 <- rbindlist(result_list)[gamma == gammavec[1] & a %in% atruncvec_SFA  & p %in% p2vec_SFA & k %in% kvec_SFA  & delta %in% deltavec_SFA]
  B_p2[, p_match := p/2]
  
  for (i in unique(kvec_SFA)){
    SFA[k == i, SFA := B / B_lower$B]
  }
  ###
  SFA[B_p2, B_p2 := i.B, on = c("p" = "p_match")]
  SFA[DT_npk, Npk:=i.value, on = c("k", "p")]
  SFA[, V := Npk*(B_p2/B^2)]
  
  ## add limits and indicators ##
  DT_SJ <- data.table("date" =  tmp_DT[,date][1],
                      "id" =  id_dummy,
                      "s" =  tmp_DT[,s][1],
                      SJ)
  DT_SJ[, limit_additive_noise := 1/k]
  DT_SJ[, limit_rounding_noise := 1/k^(1/2)]
  DT_SJ[, limit_jump := 1]
  DT_SJ[, limit_continous := k^(p/2-1)]
  DT_SJ[, test_statistic_SJ := (SJ - limit_jump)/sqrt(V)]
  DT_SJ[, critical_value := qnorm(1-alpha)]
  DT_SJ[, pvalue := 2 * pnorm(-abs(test_statistic_SJ))]
  DT_SJ[!test_statistic_SJ %between% c(-1*qnorm(1-alpha),qnorm(1-alpha)), Jump_indicator := 1]
  ## 
  
  ## add limits and indicators ##
  DT_SFA <- data.table("date" =  tmp_DT[,date][1],
                       "id" =  id_dummy,
                       "s" =  tmp_DT[,s][1],
                       SFA)
  DT_SFA[, limit_infinite_activity := 1]
  DT_SFA[, limit_finite_activity := k^(p/2-1)]
  DT_SFA[, test_statistic_SFA := (SFA - limit_finite_activity)/sqrt(V)]
  DT_SFA[, critical_value := qnorm(1-alpha)]
  DT_SFA[, pvalue := 2 * pnorm(-abs(test_statistic_SFA))]
  DT_SFA[!test_statistic_SFA %between% c(-1*qnorm(1-alpha),qnorm(1-alpha)), Jump_indicator := 1]  
  ##
  
  return(rbind(DT_SJ, DT_SFA, fill = TRUE))
}


#### testing ####

#### empirical ####
bla <- test_AJ_JumpTest(make_return_file(DT_split[[2728]], 5, IMPUTE = TRUE))
#bla <- test_AJ_JumpTest(make_return_file(DT_split[[2736]], 1, IMPUTE = TRUE))
bla[p==4  & !is.na(SJ), mean(SJ)]
bla[p==4  & !is.na(SJ), mean(test_statistic_SJ)]
plot(density(rnorm(86400, mean = bla[p==4 & !is.na(test_statistic_SJ), mean(test_statistic_SJ)])))
plot(density(rnorm(86400)))

View(bla[p==4])

#### simulation ####
sim_test <- data.table("p" = c(rnorm(86400, 2, 0.001)),
                       "date" = "test",
                       "s" = "test")
# sim_test <- data.table("p" = c(rnorm(5000, 2, 0.04), seq(2,9,0.4), rnorm(5000, 6, 0.04),seq(8,2,-0.4),rnorm(5000, 3, 0.04),rnorm(2200, 1.7, 0.04)),
#                        "date" = "test",
#                        "s" = "test")
sim_test[, t:=1:nrow(sim_test)]
sim_test[, log_ret:=quantmod::Delt(p, type = "log")]
delta <- 1
delta_n <- 1/365.25/(86400/delta)

test_result <- AJL_JumpTest_2012(sim_test, deltan = delta_n)
test_result


plot(sim_test$p)
plot(sim_test$log_ret)
bla <- test_AJ_JumpTest(sim_test)
bla[p==4 &!is.na(SFA), mean(SFA)]
bla[p==4 & !is.na(SFA), mean(test_statistic_SFA)]
bla[p==4 & !is.na(SFA), pvalue]


#### AJL Jump Test ####
delta <- 5
delta_n <- 1/365.25/(86400/delta)
test_data <- make_return_file(DT_split[[2728]], FREQ = delta)

test_result <- AJL_JumpTest_2012(test_data, deltan = 1/365.25/(86400/5))
str(test_result$result) 

####
oha <- split(DT_agg_sub[date == "2020-03-11"][, .(date, "p" = mean(p), "q" = sum(q)), by = c("t", "s")], by = c("date", "s"))
aha <- DT_agg_sub[date == "2020-03-11" & s == "bchusd"]
test <- split_by_id(DT_agg_sub[date == "2020-03-11"], IMPUTATION = TRUE)
bli <- LM_JumpTest(test$`2020-03-11.bchusd`, 1)
bli[Jump_indicator!=0]


####
bla <- LM_JumpTest(sim_test,1)
bla[Jump_indicator !=0 ]
