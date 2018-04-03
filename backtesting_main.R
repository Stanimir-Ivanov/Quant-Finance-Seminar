source("backtesting_functions.R")

##----------------------------------------------------------------------------------------------------------##
##Count violations of VaR and find transition matrix.
##----------------------------------------------------------------------------------------------------------##
backtesting <- function(data_1, results)
{
  q = c(0.95, 0.99, 0.995)
  h = 10
  data_oneday <- data_1[1001:length(data_1)]
  data_hday <- (rollapply(data_1, 10, sum))[1001:length(data_1)]
  results_oneday <- results[,1:3]
  results_shortfall <- results[,4:6]
  results_hday <- results[,7:9]
  
  counter <- c(1, 2, 3)
  backtesting_results <- lapply(counter, fun <- function(i)
                               {
                                 backtesting_oneday <- backtesting_function(results_oneday[,i],
                                                                            data_oneday, 1, 1-q[i])
                                 backtesting_hday <- backtesting_function(results_hday[,i], 
                                                                          data_hday, h, 1-q[i])
                                 return(list(oneday = backtesting_oneday, 
                                             hday = list(v = backtesting_hday$v, ind = backtesting_hday$ind)))
                               })
  return(backtesting_results)
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##




##--- Write a certain file in excel --> install.packages("WriteXLS")
##--- WriteXLS(x = as.data.frame(name_of_file), ExcelFileName = "backtesting.xls", SheetNames = "sheet_name", row.names = FALSE) 