#example inputs: 
#h = 10
#var_data = res_sp_garch$`1 Day 0.95`
#data = sp1[1001:8406]

sqr_root <- function(h, var_data, data)  #var_data consists of the 1 day ahead VaR 
{
  squared = sqrt(h) * var_data
  violations = 0
  for (    i in 1:(length(data)-h)    ) {
    hdayreturn = sum(data[(i):(i+h-1)])
    if (hdayreturn > squared[i]) {violations = violations +1}
  }
  return(violations)
}
#sqr_root(10, res_sp_garch$`1 Day 0.95`, sp1[1001:8406])                 643 violations
#sqr_root(10, res_sp_garch$`1 Day 0.99`, sp1[1001:8406])                 203 violations

hday_uncond <- function(h, var_data, data)  #var_data consists of the 10day ahead VaR 
{
  violations = 0
  for (    i in 1:(length(data)-h)    ) {
    hdayreturn = sum(data[(i):(i+h-1)])
    if (hdayreturn > var_data[i+h]) {violations = violations +1}
  }
  return(violations)
}
#hday_uncond(10, res_sp_garch$`10 Day 0.95`, sp1[1001:8406])    i 771    i+10 410
#hday_uncond(10, res_sp_garch$`10 Day 0.99`, sp1[1001:8406])    i 324    i+10 106

