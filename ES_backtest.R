library(resample)

backtest_ES <- function(es,data_1,var,sigma)
    # input of this function 
    # es: expected shortfall 
    # data_1 : neg log return data
    # var: 1day ahead VaR
    # sigma: forecaste sigma from GARCH
  
  { #data_1 <-lag(data_1) 
    input <-cbind(var,data_1,es,sigma)
    # when violation occurs, calculate the exceedance residuals
    exRes <- apply(input, 1, FUN = function(x)
      (
        if (x[1] < x[2])
        {return ((x[2]-x[3])/x[4])}
      ) )
    # bootstrap exceedance residuals ,calculate mean of each replicated sample (1000 times)
    boots <- bootstrap(exRes,mean,1000)   
    # test if mean is zero , 
    mu <- boots$stats$Observed
    sigma <-boots$stats$SE
    # sample mean distribution follows normal ,under central limit theorem
    p <- mu/sigma   
    return (list(boots,p))
    # plot(boots) to get nice graph
   }
    
    