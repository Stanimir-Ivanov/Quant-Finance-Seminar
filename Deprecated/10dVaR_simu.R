
install.packages("SpatialExtremes")
library("SpatialExtremes")

sp_garch_spec_n <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 0)) #include.mean=FALSE)
                              ,distribution.model ="norm")

simu_sp_garch_n<-ugarchfit(spec =sp_garch_spec_n ,data = sp1[2:1002])

inno_z <-simu_sp_garch_n@fit$z
sort_z <-as.vector(sort(inno_z))
sort_z <- sort_z[1:100]
right_tail <-fevd(inno_z,type="GP",threshold= quantile(inno_z,0.9))
left_tail <-fevd(-inno_z,type="GP",threshold= quantile(-inno_z,0.9))


# list of random numbers with replacement
random <- as.numeric(sample.int(1000,1000,replace=TRUE) )

right_excess <-function(right_tail)
{
coef <- as.vector(right_tail$results$par)
xi <-coef[2]
beta <-coef[1]
right_y1 <-rgpd(1, loc = 0, beta, xi)
return(right_y1)
}

left_excess <-function(left_tail)
{
  coef <- as.vector(left_tail$results$par)
  xi <-coef[2]
  beta <-coef[1]
  left_y1 <- rgpd(1, loc = 0, beta, xi)
  return(left_y1)
}



inno_z <-simu_sp_garch_n@fit$z 
right_thres <- as.numeric(quantile(inno_z,0.9))
left_thres <- as.numeric(quantile(inno_z,0.1))

# function models the simulation of innovation for h-days VaR
# input 1. n:  #  simulation picks 
# input 2. n:  #  innovation
test11 <- inno_z
hdays_VaR_simulation <-function (n,inno_z)
{
  random <- sample.int(1000,n,replace=TRUE) 

  for (i in 1:n) 
  {
    m <- random[i]
    if (inno_z[m] > right_thres)
    {
      #replace(inno_z, n, right_thres + right_excess(right_tail) ) }
      inno_z[m] <- right_thres + right_excess(right_tail)}
    
    if (inno_z[m] < left_thres)
    { inno_z[m] <- left_thres - left_excess(left_tail)}
  
  }
    return(inno_z)
}
inno_z <- hdays_VaR_simulation(1000,inno_z) 
