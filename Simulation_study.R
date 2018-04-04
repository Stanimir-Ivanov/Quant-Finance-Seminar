t <-lapply(1:1000, function(t) (rt(1000,4)))
k <-seq(from=0.99,to=0.6,by= -0.001)
try <- lapply(k, FUN= function(k){
  sapply(t,FUN=function(t) {
    simu_fit<- fevd(t,type="GP",threshold= quantile(t,k))
    coef <- simu_fit$results$par
    xi <-coef[2] 
    beta <-coef[1]
    zq_hat <- quantile(t,(k-0.001)) + (beta/xi)*  ( (0.01/(1-k))^(-xi) -1)
    # zq_sample <-quantile(t,0.99)
    zq <-qt(0.99,4)
    out <-union(zq,zq_hat)
    
    #mse <-mse(out[,1],mse[,2])
    return(out)
  } )
} )


library(Metrics)
mse <- sapply(try,FUN=function(try) {
  try<-t(as.data.frame(try))
  mse(try[,1],try[,2])
})
plot(mse)
