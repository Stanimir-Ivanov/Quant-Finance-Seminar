##----------------------------------------------------------------------------------------------------------##
##Function to estimate GARCH model and forecast 1-step ahead.
##----------------------------------------------------------------------------------------------------------##
estimate_garch <- function(specifications, data_1)
{
  #Fit the GARCH model.
  garch_fit <- ugarchfit(spec = specifications, data = data_1)
  
  #Forecast according to the input values.
  garch_forecast <- ugarchforecast(fitORspec = garch_fit, data = data_1, n.ahead = 1, 
                                   n.roll = 0, out.sample = 0)
  
  #Return the specifications and estimation of the GARCH model.
  return(list(fit = garch_fit, forecast = garch_forecast))
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
VAR_estimation <- function(specifications, data_1)
{
  VAR_results <- rollapply(data_1, width = 1000,
                              FUN = function(data_1)
                              {
                                #Estimate GARCH model and standirdized residuals
                                results_data <- estimate_garch(specifications, data_1)
                                mu <- results_data$forecast@forecast$seriesFor
                                sigma <- results_data$forecast@forecast$sigmaFor
                                innovations <- results_data$fit@fit$z
                                
                                #Estimate GPD distribution coefficients
                                GPD_dist <- estimate_gpd(innovations)
                                GPD_coef <- GPD_dist$results$par
                                
                                #Calculate quantile of the innovations
                                q <- 0.95
                                beta <- GPD_coef[1]
                                xi <- GPD_coef[2]
                                innovations_quantile = quantile(innovations, 0.899) + 
                                  (beta/xi)*(((1-q)*1000/100)^(-xi) -1)
                                
                                #Calculate the VAR
                                VAR_onestep <- mu + sigma*innovations_quantile
                                return(VAR_onestep)
                                },
                      by.column=FALSE, align="right")
  return(VAR_results)  
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##





##----------------------------------------------------------------------------------------------------------##
##Main function.
##----------------------------------------------------------------------------------------------------------##
#Define specifications of the GARCH model based on the input values.
sp1 <-sp[1:1030];
sp_garch_spec_t <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 0), include.mean = FALSE), 
                              distribution.model ="std")

VaR_results <- VAR_estimation(specifications = sp_garch_spec_n, data_1 = sp1)
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##