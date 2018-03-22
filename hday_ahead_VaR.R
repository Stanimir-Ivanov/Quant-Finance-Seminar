source("two_step_method.R")

##----------------------------------------------------------------------------------------------------------##
##Function to calculate VAR from GPD distribution and quantile of innovations z.
##----------------------------------------------------------------------------------------------------------##
hday_simulation_function <- function(specifications, data_1, q, bigassmofo)
{
  hday_simulation <- rollapply(data_1, width = 1000,
                           FUN = function(data_1)
                           {
                             # Estimate GARCH model and standirdized residuals
                             results_data <- estimate_garch(specifications, data_1)
                             mu <- results_data$forecast@forecast$seriesFor
                             sigma <- results_data$forecast@forecast$sigmaFor
                             innovations <- results_data$fit@fit$z
                             
                             xt1 <- mu + sigma*innovations
                             
                             xt2_h <- lapply()
                             
                             cbind(xt a)
                             
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