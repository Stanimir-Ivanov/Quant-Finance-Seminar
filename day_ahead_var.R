source("two_step_method.R")

##----------------------------------------------------------------------------------------------------------##
##Function to calculate VaR.
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
                             garch <- estimate_garch(specifications, data_1)
                             mu <- garch$forecast@forecast$seriesFor
                             sigma <- garch$forecast@forecast$sigmaFor
                             innovations <- garch$fit@fit$z
                             
                             # Estimate GPD distribution coefficients
                             gpd <- estimate_gpd(innovations)
                             gpd_coef <- gpd$right_tail$results$par
                             
                             # Calculate quantile of the innovations
                             beta <- gpd_coef['scale']
                             xi <- gpd_coef['shape']
                             
                             # Calculate z_k+1
                             z_k1 <- quantile(innovations, 0.899);
                             
                             # Calculate VaR
                             VaR_onestep <- calculate_VaR(z_k1, beta, xi, mu, sigma, q)
                             VaR_onestep <- cbind(t(VaR_onestep), 
                                              gpd$right_tail$results$par['scale'], 
                                              gpd$right_tail$results$par['shape'], 
                                              gpd$left_tail$results$par['scale'], 
                                              gpd$left_tail$results$par['shape'], 
                                              t(garch$fit@fit$coef),
                                              garch$forecast@forecast$seriesFor,
                                              garch$forecast@forecast$sigmaFor)
                             names(VaR_onestep) <- c(q, 
                                                      "right_tail_beta",
                                                      "right_tail_xi",
                                                      "left_tail_beta",
                                                      "left_tail_xi",
                                                      names(garch$fit@fit$coef),
                                                      "muFor",
                                                      "sigmaFor"
                                                      )
                             return(VaR_onestep)
                           },
                           by.column=FALSE, align="right")
  VaR_results <- lag(VaR_results)
  VaR_results <- VaR_results[!is.na(VaR_results[,1])]
  return(VaR_results)  
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##