##----------------------------------------------------------------------------------------------------------##
##Function to estimate GARCH model and forecast 1-step ahead.
##----------------------------------------------------------------------------------------------------------##
estimate_garch <- function(specifications, data_1)
{
  #Fit the GARCH model.
  garch_fit <- ugarchfit(spec = specifications, data = data_1, solver = "hybrid")
  
  #Forecast according to the input values.
  garch_forecast <- ugarchforecast(fitORspec = garch_fit, data = data_1, n.ahead = 1)
  
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
##Function to estimate GPD distribution.
##----------------------------------------------------------------------------------------------------------##
calculate_VaR <- function(z_k1, beta, xi, mu, sigma, q)
{
  innovations_quantile = z_k1 + 
    (beta/xi)*(((1 - q)*1000/100)^(-xi) - 1)
  VaR <- mu + sigma*innovations_quantile
  names(VaR) <- q
  return(VaR)
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------## 




##----------------------------------------------------------------------------------------------------------##
##Function to calculate VAR from GPD distribution and quantile of innovations z.
##----------------------------------------------------------------------------------------------------------##
day_ahead_VAR <- function(specifications, data_1, q)
{
  VaR_results <- rollapply(data_1, width = 1000,
                           FUN = function(data_1)
                           {
                             # Estimate GARCH model and standirdized residuals
                             results_data <- estimate_garch(specifications, data_1)
                             mu <- results_data$forecast@forecast$seriesFor
                             sigma <- results_data$forecast@forecast$sigmaFor
                             innovations <- results_data$fit@fit$z
                             
                             # Estimate GPD distribution coefficients
                             GPD_dist <- estimate_gpd(innovations)
                             GPD_coef <- GPD_dist$results$par
                             
                             # Calculate quantile of the innovations
                             beta <- GPD_coef['scale']
                             xi <- GPD_coef['shape']
                             
                             # Calculate z_k+1
                             z_k1 <- quantile(innovations, 0.899);
                             
                             # Calculate VaR
                             VaR_onestep <- calculate_VaR(z_k1, beta, xi, mu, sigma, q)

                             return(VaR_onestep)
                           },
                           by.column=FALSE, align="right")
  VaR_results <- lag(VaR_results)
  return(VaR_results)  
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##