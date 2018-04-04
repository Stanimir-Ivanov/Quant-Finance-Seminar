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
plot(mse,type="l",xlab="k",ylab="MSE(0.99quantile)")

bias <- sapply(try,FUN=function(try) {
  try<-t(as.data.frame(try))
  #bias(try[,2],try[,1])
  mean(try[,2]-try[,1])
})
plot(bias[20:length(bias)] ,type="l",xlab="k",ylab="Bias(0.99quantile)")
#points(mse,bias, col="red", ylab="k")
# par(mar=c(1,1,1,1))

plot(bias,mse)
bias_mse <-data.frame(cbind(bias,mse))


