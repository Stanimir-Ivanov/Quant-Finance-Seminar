library(resample)

backtest_ES <- function(var,data_1,es,sigma)
    # input of this function 
    # es: expected shortfall 
    # data_1 : neg log return data
    # var: 1day ahead VaR
    # sigma: forecaste sigma from GARCH
  
  { #data_1 <-lag(data_1) 
    input <-as.matrix(cbind(var,data_1,es,sigma))
    # when violation occurs, calculate the exceedance residuals
    exRes <- c()
    for (i in 1: length(input[,1]))
    {
      if (input[i,1] < input[i,2]) {exRes[i] <- (input[i,2]-input[i,3])/input[i,4]}
    }
    # exRes <- apply(input, 1, FUN = function(x)
    #   (
    #     if (x[1] < x[2])
    #     {return ((x[2]-x[3])/x[4])}
    #   ) )
    # bootstrap exceedance residuals ,calculate mean of each replicated sample (1000 times)
    exRes <-exRes[!is.na(exRes)]
    boots <- bootstrap(exRes,mean,1000)   
    # test if mean is zero , 
    mu <- boots$stats$Observed
    sigma <-boots$stats$SE
    # sample mean distribution follows normal ,under central limit theorem
    stats <- mu/sigma 
    p <- pt(stats,df)
  
    return (list(boots,p))
    # plot(boots) to get nice graph
   }
    
es_sp_garch95 <- backtest_ES(res_sp_garch[2:7406,1],sp_data[1002:length(sp_data)],
                             res_sp_garch[2:7406,4],garch_sp_con_n@forecast$density[,2])


# 
# out <- c()
# for (i in 1:length(abcd[,1]))
# {
#   if (abcd[i,1] < abcd[i,2]) {out[i] <- (abcd[i,2]-abcd[i,3])/abcd[i,4]}
#   
# }

