##----------------------------------------------------------------------------------------------------------##
##Function to estimate GARCH model and forecast 1-step ahead.
##----------------------------------------------------------------------------------------------------------##
estimate_garch <- function(garch_type, distribution, ar, ma, arch_lag, garch_lag, data_1, 
                           days_ahead, n_roll, n_outofsample)
{
  #Define specifications of the GARCH model based on the input values.
  specifications <- ugarchspec(variance.model = list(model = garch_type, garchOrder = c(arch_lag, garch_lag)),
                               mean.model = list(armaOrder = c(ar, ma), include.mean = FALSE), 
                               distribution.model = distribution)
  
  #Fit the GARCH model.
  garch_fit <- ugarchfit(spec = specifications, data = data_1)
  
  #Forecast according to the input values.
  garch_forecast <- ugarchforecast(fitORspec = garch_fit, data = data_1, n.ahead = days_ahead, 
                                   n.roll = n_roll, out.sample = n_outofsample)
  
  #Return the specifications and estimation of the GARCH model.
  return(list(a = specifications, b = garch_fit, c = garch_forecast))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##





##----------------------------------------------------------------------------------------------------------##
##Function to estimate GPD distribution.
##----------------------------------------------------------------------------------------------------------##
estimate_gpd <- function(innovations_garch)
{
  GPD_fit <- fevd(innovations_garch, type = "GP", threshold = quantile(innovations_garch, 0.90))
  return(GPD_fit)
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------## 

  
  

    
##----------------------------------------------------------------------------------------------------------##
##Function to calculate VAR from GPD distribution and quantile of innovations z.
##----------------------------------------------------------------------------------------------------------##
VAR_estimation <- function(garch_type, distribution, ar, ma, arch_lag, garch_lag, data_1, 
                                   days_ahead, n_roll, n_outofsample)
{
  VAR_results = matrix(,nrow = 0, ncol = 1)
  roll_w <- rollapply(data_1, width = 1000,
                              function_window = function(data_1)
                              {
                                results_data <- estimate_garch(garch_type, distribution, ar, ma, arch_lag, 
                                                              garch_lag, data_1, days_ahead, n_roll,
                                                              n_outofsample)
                                mu <- results_data$c@forecast$seriesFor
                                sigma <- results_data$c@forecast$sigmaFor
                                
                                GPD_dist <- estimate_gpd(results_sp2$b@fit$z)
                                GPD_coef <- GPD_dist$results$par
                                
                                q <- 0.95
                                beta <- GPD_coef[1]
                                xi <- GPD_coef[2]
                                innovations_quantile = quantile(res_garch,0.899) + 
                                  (beta/xi)*(((1-q)*1000/100)^(-xi) -1)
                                
                                VAR_onestep = mu + sigma*innovations_quantile
                                VAR_results = rbind(VAR_results, VAR_onestep)
                                },
                      by.column=FALSE, align="right")
  return(VAR_results)
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##





##----------------------------------------------------------------------------------------------------------##
##Main function.
##----------------------------------------------------------------------------------------------------------##


##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##