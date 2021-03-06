##----------------------------------------------------------------------------------------------------------##
##Count violations of VaR and find transition matrix.
##----------------------------------------------------------------------------------------------------------##
counting_violations <- function(VaR, data_1)
{
  #Calculate violations vector
  violations <- data_1 > VaR
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
christoffersen <- function(VaR, data_1, q)
{
  violations_info <- counting_violations(VaR, data_1)
  unc_cov <- unconditional_coverage(violations_info$v, length(data_1), q)
  ind <- independence(violations_info$t11, violations_info$t10, violations_info$t01, violations_info$t00)
  cc <- conditional_coverage(unc_cov$LR, ind$LR)
  return(list(unconditional_coverage = unc_cov, independence = ind, conditional_coverage = cc))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##