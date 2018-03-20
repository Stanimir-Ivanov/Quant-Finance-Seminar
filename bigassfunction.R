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
##Function to calculate GPD distribution and quantile of innovations z.
##----------------------------------------------------------------------------------------------------------##
rolling_window <- function(garch_type, distribution, ar, ma, arch_lag, garch_lag, data_1, 
                                   days_ahead, n_roll, n_outofsample)
{
  garch_forecasts <- matrix(,nrow = 0, ncol = 2)
  roll_w <- rollapply(data_1, width = 1000,
                              function_window = function(data_1)
                              {
                                results_sp2 <- estimate_garch(garch_type, distribution, ar, ma, arch_lag, 
                                                              garch_lag, data_1, days_ahead, n_roll,
                                                              n_outofsample)
                                
                                garch_forecasts <- rbind(forecast_results, c(results_sp2$c@forecast$seriesFor,
                                                                              results_sp2$c@forecast$sigmaFor))
                                
                                GPD_dist <- estimate_gpd(results_sp2$b@fit$z)
                                GPD_coef <- GPD_dist$results$par
                                
                                q <- 0.95
                                beta <- GPD_coef[1]
                                xi <- GPD_coef[2]
                                innovations_quantile = quantile(res_garch,0.899) + 
                                  (beta/xi)*(((1-q)*1000/100)^(-xi) -1)
                                }
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##


recur_sp <-rollapply(sp1,
                     width = 1000, 
                     FUN= function(sp1)
                     { 
                       recur_garch<- ugarchfit(spec =sp_garch_spec_t ,data =as.data.frame(sp1))
                       res_garch <-recur_garch@fit$residuals
                       gpd_fit <-fevd(res_garch,type="GP",threshold= quantile(res_garch,0.90))
                       coef <- gpd_fit$results$par
                       xi <-coef[2]
                       beta <-coef[1]
                       q <- 0.95
                       var <- quantile(res_garch,0.899) + (beta/xi)* (((1-q)*1000/100)^(-xi) -1)
                       out <-union(var,coef)
                       return (out)
                     },
                     by.column=FALSE, align="right")
recur_sp 
