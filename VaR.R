source("two_step_method.R")

##----------------------------------------------------------------------------------------------------------##
##Function to calculate VaR.
##----------------------------------------------------------------------------------------------------------##
calculate_VaR <- function(z_k1, beta, xi, mu, sigma, q)
{
  innovations_quantile = z_k1 + 
    (beta/xi)*(((1 - q)*1000/100)^(-xi) - 1)
  VaR <- mu + sigma*innovations_quantile
  return(VaR)
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------## 





##----------------------------------------------------------------------------------------------------------##
##Random innovation sumulation function.
##----------------------------------------------------------------------------------------------------------##
rand_innovation <- function(z_vec, gpd)
{
  r_threshold <- as.numeric(quantile(z_vec,0.9))
  l_threshold <- as.numeric(quantile(z_vec,0.1))
  
  r <- sample(1:1000, 1)
  if (z_vec[r] > r_threshold)
  {
    z_vec[r] <- r_threshold + rgpd(1, loc = 0, scale = gpd$right_tail$results$par['scale'],
                                   shape = gpd$right_tail$results$par['shape'])
  }
  if (z_vec[r] < l_threshold)
  {
    z_vec[r] <- l_threshold - rgpd(1, loc = 0, scale = gpd$left_tail$results$par['scale'],
                                   shape = gpd$left_tail$results$par['shape'])
  }
  return(list(innovations = z_vec, z = z_vec[r]))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##





##----------------------------------------------------------------------------------------------------------##
##Function to calculate 1-day and h-day ahead VAR from GPD distribution and quantile of innovations z.
##----------------------------------------------------------------------------------------------------------##
VaR_estimation <- function(specifications, data_1, q, h, n)
{
  VaR_results <- rollapply(data_1, width = 1000,
                           FUN = function(data_1)
                           {
                             ## ONE DAY AHEAD VAR ---
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
                             z_k1 <- quantile(innovations, 0.899);
                             
                             # Calculate day-ahead VaR
                             VaR_1day <- calculate_VaR(z_k1, beta, xi, mu, sigma, q)
                             ## ---
                             
                             
                             
                             ## H-DAY AHEAD VAR ---
                             # Define coefficients
                             mu_t <- mu
                             sigma_t <- sigma
                             ARmu <- garch$fit@fit$coef['mu']
                             phi <- garch$fit@fit$coef['ar1']
                             omega <- garch$fit@fit$coef['omega']
                             alpha1 <- garch$fit@fit$coef['alpha1']
                             beta1 <- garch$fit@fit$coef['beta1']
                             
                             
                             simulations <- as.matrix(1:n)
                             paths <- apply(simulations, MARGIN = 1, PATHS <- function(k)
                             {
                               hdays <- as.matrix(1:h)
                               return (sum(apply(hdays, MARGIN = 1,
                                                 HDAYS <- function(j) 
                                                 {
                                                   z <- rand_innovation(innovations, gpd)
                                                   innovations <- z$innovations
                                                   xt <- mu_t + sigma_t * as.numeric(z$z)
                                                   eps <- xt - mu_t
                                                   mu_t <- phi*xt
                                                   sigma_t <- (omega + alpha1*eps^2 + beta1*sigma_t^2)^(1/2)
                                                   return(xt)
                                                 })))
                             })
                             VaR_hday <- quantile(paths, q)
                             return(c(VaR_1day, t(VaR_hday)))
                           },
                           by.column=FALSE, align="right")
  VaR_results <- lag(VaR_results)
  names <- c(paste("1 Day", q), paste(h, "Day", q))
  colnames(VaR_results) <- names
  VaR_results <- VaR_results[!is.na(VaR_results[,1])]
  return(VaR_results)  
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##