# estimates a matrix of garch coefficients 
# where each row is a vector of parameters 
# estimated with a moving window sample
library(rugarch)
library(tcltk)
coefMatrix <- function(return_df){
  returns <- return_df$Returns[2:length(rds_b$Returns)]
  T <- length(returns)
  # size of moving window
  n <- 1000
  # create progress bar
  pb <- tkProgressBar(title = "progress bar", min = 0,
                      max = T - n, width = 300)
  # model spec
  gspec.ru <-  ugarchspec(mean.model=list(
    armaOrder=c(1,0)), distribution="std")
  c <- matrix(,nrow = 0, ncol = 6)
  colnames(c) <- c("mu", "ar1", "omega", "alpha1", "beta1", "shape")
  # run garch
  for(i in n:T){
    # fit model
    gfit.ru <- ugarchfit(gspec.ru, returns[i - n + 1:i])
    #update progress bar
    setTkProgressBar(pb, i - n, label=paste( round((i - n)/(T - n)*100, 0),
                                         "% done"))
    c <- rbind(c, coef(gfit.ru))
  }
  close(pb)
  # return coefficient matrix
  c
}
