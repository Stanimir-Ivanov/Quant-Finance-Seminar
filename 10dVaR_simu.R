
install.packages("SpatialExtremes")
library("SpatialExtremes")

sp_garch_spec_n <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 0)) #include.mean=FALSE)
                              ,distribution.model ="norm")

simu_sp_garch_n<-ugarchfit(spec =sp_garch_spec_n ,data = sp1[2:1002])

inno_z <-simu_sp_garch_n@fit$z

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
  left_y1 <- -rgpd(1, loc = 0, beta, xi)
  return(left_y1)
}

right_thres <- as.numeric(quantile(inno_z,0.9))
left_thres <- as.numeric(quantile(inno_z,0.1))


#simulation step 234, needs to be combined with apply or for-loop

inno_z <-inno_z[1:10]
step234 <- function(n,inno_z)
{ 
  if (inno_z[n] > right_thres)
  { 
    #replace(inno_z, n, right_thres + right_excess(right_tail) ) }
    inno_z[n] <- right_thres + right_excess(right_tail)}
  
  else if (inno_z[n] < left_thres)
  { inno_z[n] <- left_thres + left_excess(left_tail)}
  
  else
  { inno_z[n] <-inno_z[n] }
  inno_z <- inno_z
   return(inno_z)
  }


inno_z <-simu_sp_garch_n@fit$z

simu_inno <- sapply(random, function(random)
{
  if (inno_z[n] > right_thres)
  {
    #replace(inno_z, n, right_thres + right_excess(right_tail) ) }
    inno_z[n] <- right_thres + right_excess(right_tail)}

  else if (inno_z[n] < left_thres)
  { inno_z[n] <- left_thres + left_excess(left_tail)}

  else
  { inno_z[n] <-inno_z[n] }

  return(inno_z)
})
simu_inno

  

