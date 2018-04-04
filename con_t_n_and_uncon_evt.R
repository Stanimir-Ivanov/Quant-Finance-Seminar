# Import data by read.zoo and pass it into function "data_read"
# Function "data_read" convert data into suitable format (positive log returns, 
# which are needed for garch_normal and garch_t)
# then run garch_normal and garch_t

# examples of importing data is the following: 
sp <- read.zoo("SP-500.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)
ing <- read.zoo("ING.csv", header = TRUE, sep = ",",format="%Y/%m/%d",index.column = 1)
rds <- read.zoo("RDS-B.csv", header = TRUE, sep = ",",format="%Y/%m/%d",index.column = 1)
aex <- read.zoo("AEX.csv", header = TRUE, sep = ",",format="%Y/%m/%d",index.column = 1)

# step1. convert data 
# step2. run data in garch_normal and garch_t 
# reprot the relevant var value

# Important !! Function garch_normal and garch_t !!  requries POSTITIVE log return as data input
# Function uncondiEvT requries negative log returns as we always did
#####
# sp_p is positive return return
# sp is negative return 
####
data_read <- function(data1)
{
# data1 <- read.zoo("SP-500.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)
data1 <- xts(data1)
data1 <- 100*diff(log(data1))
data1 <- data1[2: length(data1)]
return (data1)
}

egarch_normal <- function(data1)
{
  sp_garch_spec_n <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                                mean.model = list(armaOrder = c(1, 0)) #include.mean=FALSE)
                                ,distribution.model ="norm")

  sp_garch_n <-ugarchroll(spec =sp_garch_spec_n,data =data1, n.ahead = 1, 
                        n.start = 1000,
                        refit.every = 1, refit.window = "moving",
                        calculate.VaR = TRUE, VaR.alpha = c(0.005,0.01, 0.05))
   
}
egarch_t <-function(data1)
{
  sp_garch_spec_t <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                                mean.model = list(armaOrder = c(1, 0)),distribution.model ="std")
  # testing VaR with Student's t
  sp_garch_t <-ugarchroll(spec =sp_garch_spec_t,data =data1, n.ahead = 1, 
                          n.start = 1000,
                          refit.every = 1, refit.window = "moving",
                          calculate.VaR = TRUE, VaR.alpha = c(0.005,0.01, 0.05))
}

# example of reporting 
report(sp_garch_t, type="VaR", VaR.alpha = 0.005, conf.level = 0.95)    
report(sp_garch_t, type="VaR", VaR.alpha = 0.01, conf.level = 0.95) 
report(sp_garch_t, type="VaR", VaR.alpha = 0.05, conf.level = 0.95) 
report(sp_garch_t, type="fpm")

# function uncondiEvT calculate the VaR based on unciondtional EVT, i.e. apply GPD on raw data 
# input data should contain negative log return
# return 3 levels VaR 0.95, 0.99 and 0.995
# VaR needs to be passed into backtesting function
uncondiEvT <-function (data1)
{
  rollapply(data1,
            width = 1000, 
            FUN= function(data1)
            { 
              k <- 0.90  #  threshold quantile choice
              gpd_fit <-fevd(data1,type="GP",threshold= quantile(data1,k))
              coef <- gpd_fit$results$par
              mu <- mean(data1)
              sigma <- sd(data1)
              xi <-coef[2]
              beta <-coef[1]
              q1 <- 0.95   # VaR quantile 
              q2 <- 0.99
              q3 <- 0.995
              zq95 <- quantile(data1,(k-0.001)) + (beta/xi)* (((1-q1)/(1-k))^(-xi) -1)
              zq99 <- quantile(data1,(k-0.001)) + (beta/xi)* (((1-q2)/(1-k))^(-xi) -1)
              zq995 <- quantile(data1,(k-0.001)) + (beta/xi)* (((1-q3)/(1-k))^(-xi) -1)
              # var95 <-mu + sigma *zq95
              # var99 <-mu + sigma *zq99
              # var995 <-mu + sigma *zq995
              var <- cbind(zq95,zq99,zq995)
              colnames(var) <- c("VaR95%","VaR99%","VaR99.5%")
              return (var)
            },
            by.column=FALSE, align="right",na.pad=FALSE)
}

sp_p <- data_read(sp)
ing_p <- data_read(ing)
aex <- data_read(aex)
rds_p <- data_read(rds)
aex_data <- aex[!is.na(aex_data)]
# debuggingState(on=FALSE) , quite global debugging mode
# compute the running time of the program
start.time <- Sys.time()
tgarch_sp_con_n <- tgarch_normal(sp)
tgarch_sp_con_t <- tgarch_t(sp)
tgarch_ing_con_n <- tgarch_normal(ing)
tgarch_ing_con_t <- tgarch_t(ing)
tgarch_rds_con_n <- tgarch_normal(rds)
tgarch_rds_con_t <- tgarch_t(rds)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

ing <- -ing_p
sp <- -sp_p
rds <- -rds_p
rds_uncon_evt <-uncondiEvT(rds)
ing_uncon_evt <-uncondiEvT(ing)
sp_uncon_evt <-uncondiEvT(sp)


source("backtesting.R")
# test of sp 
sp_95 <- counting_violations(sp_uncon_evt[,1], sp_p[1002:8405])
sp_99 <- counting_violations(sp_uncon_evt[,2], sp_p[1000:8405])
sp_995 <- counting_violations(sp_uncon_evt[,3], sp_p[1000:8405])
uncon_cov_sp_95 <- unconditional_coverage (sp_95$v,length(sp_uncon_evt[,1]),0.05 )
uncon_cov_sp_99 <- unconditional_coverage (sp_99$v,length(sp_uncon_evt[,1]),0.01 )
uncon_cov_sp_995 <- unconditional_coverage (sp_995$v,length(sp_uncon_evt[,1]),0.005)
ind_sp_95 <- independence(sp_95$t11,sp_95$t10,sp_95$t01,sp_95$t00)
ind_sp_99 <- independence(sp_99$t11,sp_99$t10,sp_99$t01,sp_99$t00)
ind_sp_995 <- independence(sp_995$t11,sp_995$t10,sp_995$t01,sp_995$t00)
chris_sp_95 <- christoffersen(sp_uncon_evt[,1],sp[1000:length(sp)],0.05)
chris_sp_99 <- christoffersen(sp_uncon_evt[,2],sp[1000:length(sp)],0.01)
chris_sp_995 <- christoffersen(sp_uncon_evt[,3],sp[1000:length(sp)],0.005)

new_chris_sp95 <-chris(sp_garch_t@forecast$VaR[,3],sp[1002:length(sp)],0.05)
new_chris_sp99 <-chris(sp_garch_t@forecast$VaR[,2],sp[1002:length(sp)],0.01)
new_chris_sp995 <-chris(sp_garch_t@forecast$VaR[,1],sp[1002:length(sp)],0.005)




# test of ing 
ing_95 <- counting_violations(ing_uncon_evt[,1], ing[1000:length(ing)])
ing_99 <- counting_violations(ing_uncon_evt[,2], ing[1000:length(ing)])
ing_995 <- counting_violations(ing_uncon_evt[,3], ing[1000:length(ing)])
uncon_cov_ing_95 <- unconditional_coverage (ing_95$v,length(ing_uncon_evt[,1]),0.05 )
uncon_cov_ing_99 <- unconditional_coverage (ing_99$v,length(ing_uncon_evt[,1]),0.01 )
uncon_cov_ing_995 <- unconditional_coverage (ing_995$v,length(ing_uncon_evt[,1]),0.005 )
ind_ing_95 <- independence(ing_95$t11,ing_95$t10,ing_95$t01,ing_95$t00)
ind_ing_99 <- independence(ing_99$t11,ing_99$t10,ing_99$t01,ing_99$t00)
ind_ing_995 <- independence(ing_995$t11,ing_995$t10,ing_995$t01,ing_995$t00)
chris_ing_95 <- christoffersen(ing_uncon_evt[,1],ing[1000:length(ing)],0.05)
chris_ing_99 <- christoffersen(ing_uncon_evt[,2],ing[1000:length(ing)],0.01)
chris_ing_995 <- christoffersen(ing_uncon_evt[,3],ing[1000:length(ing)],0.005)


# test of rds 
rds_95 <- counting_violations(rds_uncon_evt[,1], rds[1000:length(rds)])
rds_99 <- counting_violations(rds_uncon_evt[,2], rds[1000:length(rds)])
rds_995 <- counting_violations(rds_uncon_evt[,3], rds[1000:length(rds)])
uncon_cov_rds_95 <- unconditional_coverage (rds_95$v,length(rds_uncon_evt[,1]),0.05 )
uncon_cov_rds_99 <- unconditional_coverage (rds_99$v,length(rds_uncon_evt[,1]),0.01 )
uncon_cov_rds_995 <- unconditional_coverage (rds_995$v,length(rds_uncon_evt[,1]),0.005 )
ind_rds_95 <- independence(rds_95$t11,rds_95$t10,rds_95$t01,rds_95$t00)
ind_rds_99 <- independence(rds_99$t11,rds_99$t10,rds_99$t01,rds_99$t00)
ind_rds_995 <- independence(rds_995$t11,rds_995$t10,rds_995$t01,rds_995$t00)
chris_rds_95 <- christoffersen(rds_uncon_evt[,1],rds[1000:length(rds)],0.05)
chris_rds_99 <- christoffersen(rds_uncon_evt[,2],rds[1000:length(rds)],0.01)
chris_rds_995 <- christoffersen(rds_uncon_evt[,3],rds[1000:length(rds)],0.005)
#rds_garch_t_995 <-rds_con_t@forecast$VaR[,1]
#rds_tgarch_t_995 <-tgarch_rds_con_t@forecast$VaR[,1]
rds_garch_t_chri995 <-chris(rds_con_t@forecast$VaR[,1],rds[1001:length(rds)],0.005)
rds_tgarch_t_chri995 <-chris(tgarch_rds_con_t@forecast$VaR[,1],rds_p[1001:length(rds)],0.005)
# egarch
start.time <- Sys.time()
egarch_sp_con_n <- egarch_normal(sp)
egarch_sp_con_t <- egarch_t(sp)
egarch_ing_con_n <- egarch_normal(ing)
egarch_ing_con_t <- egarch_t(ing)
egarch_rds_con_n <- egarch_normal(rds)
egarch_rds_con_t <- egarch_t(rds)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



report(tgarch_rds_con_t, type="VaR", VaR.alpha = 0.005, conf.level = 0.95)    
report(tgarch_rds_con_t, type="VaR", VaR.alpha = 0.01, conf.level = 0.95) 
report(tgarch_rds_con_t, type="VaR", VaR.alpha = 0.05, conf.level = 0.95) 

report(tgarch_rds_con_n, type="VaR", VaR.alpha = 0.005, conf.level = 0.95)    
report(tgarch_rds_con_n, type="VaR", VaR.alpha = 0.01, conf.level = 0.95) 
report(tgarch_rds_con_n, type="VaR", VaR.alpha = 0.05, conf.level = 0.95) 

##----------------------------------------------------------------------------------------------------------##
##Count violations of VaR and find transition matrix.
##----------------------------------------------------------------------------------------------------------##
counting_violations <- function(VaR, data_1)
{
  #Calculate violations vector
  violations <- data_1 < VaR
  violations <- violations[!is.na(violations)]
  n <- length(violations)
  n_violations <- sum(violations)
  
  v1 <- coredata(as.array(violations[1:n-1]))
  v2 <- coredata(as.array(violations[2:n]))
  
  T11 <- sum(v1 & v2)
  T00 <- sum(!v1 & !v2)
  T10 <- sum(v1 > v2)
  T01 <- sum(v2 < v1)
  
  return(list(v = n_violations, t11 = T11, t10 = T10, t01 = T01,  t00 = T00))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##





##----------------------------------------------------------------------------------------------------------##
##Calculate unconditional coverage.
##----------------------------------------------------------------------------------------------------------##
unconditional_coverage <- function(n_violations, length_data, q)
{ # q is 1-VaR, 0.05,0.01,0.005
  T1 <- n_violations
  T0 <- length_data - n_violations
  pi_hat <- T1/length_data
  
  H0 <- T0*log(1-q) + T1*log(q)
  HA <- T0*log(1-pi_hat) + T1*log(pi_hat)
  LR_uc <- -2*(H0 - HA)
  p_vale <- 1 - pchisq(q = LR_uc, df = 1)
  
  return(list(t0 = T0, t1 = T1, pi = pi_hat, LR = LR_uc, p = p_vale))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##





##----------------------------------------------------------------------------------------------------------##
##Calculate independence.
##----------------------------------------------------------------------------------------------------------##
independence <- function(T11, T10, T01, T00)
{
  pi11 <- T11/(T11+T10)
  pi10 <- T10/(T11+T10)
  pi01 <- T01/(T00+T01)
  pi00 <- T00/(T00+T01)
  pi2 = (T01+T11)/(T00+T01+T10+T11)
  
  H0 <- (T00+T10)*log(1-pi2) + (T11+T01)*log(pi2)
  HA <- T00*log(pi00) + T01*log(pi01) + T10*log(pi10) + T11*log(pi11)
  LR_ind <- -2*(H0-HA)
  p_value <- 1 - pchisq(q = LR_ind, df = 1)
  
  return(list(pi11 = pi11, pi10 = pi10, pi01 = pi01, pi00 = pi00, pi2 = pi2, LR = LR_ind, p = p_value))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##





##----------------------------------------------------------------------------------------------------------##
##Calculate conditional coverage.
##----------------------------------------------------------------------------------------------------------##
conditional_coverage <- function(LR_uc, LR_ind)
{
  LR_cc <- LR_uc + LR_ind
  p_value <- 1 - pchisq(q = LR_cc, df = 2)
  
  return(list(LR = LR_cc, p = p_value))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##





##----------------------------------------------------------------------------------------------------------##
##Run Christoffersen's VaR evaluation tests (runs the three functions above).
##----------------------------------------------------------------------------------------------------------##
chris <- function(VaR, data_1, q)
{
  violations_info <- counting_violations(VaR, data_1)
  unc_cov <- unconditional_coverage(violations_info$v, length(data_1), q)
  ind <- independence(violations_info$t11, violations_info$t10, violations_info$t01, violations_info$t00)
  cc <- conditional_coverage(unc_cov$LR, ind$LR)
  return(list(unconditional_coverage = unc_cov, independence = ind, conditional_coverage = cc))
}
##----------------------------------------------------------------------------------------------------------##

