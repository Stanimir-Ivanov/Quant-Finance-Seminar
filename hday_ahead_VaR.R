source("two_step_method.R")
library(SpatialExtremes)


##----------------------------------------------------------------------------------------------------------##
##Random innovation sumulation function.
##----------------------------------------------------------------------------------------------------------##
rand_innovation <- function(z_vec, gpd,n)
{
  random <- sample.int(1000,n,replace=TRUE) 
  for (i in 1:n) 
  {
    m <- random[i]
    if (z_vec[m] > gpd$right_thresh)
    {
      z_vec[m] <- gpd$right_thresh + rgpd(1, loc = 0, gpd$right_beta, gpd$right_xi)
    }
    if (z_vec[m] < gpd$left_thresh)
    {
      z_vec[m] <- left_thresh - rgpd(1, loc = 0, gpd$left_beta, gpd$left_xi)
    }
    
    return(z_vec)
  }
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##





##----------------------------------------------------------------------------------------------------------##
##Function to calculate VAR from GPD distribution and quantile of innovations z.
##----------------------------------------------------------------------------------------------------------##
hday_simulation_function <- function(specifications, data_1, q, results, h)
{
  counter <- 1
  hday_simulation <- lapply(1:length(data_1),
                           FUN = function()
                           {
                             # Define parameters
                             muFor <- results$muFor[counter]
                             sigmaFor <- results$sigmaFor[counter]
                             innovations <- results[counter, (length(results) - 999):length(results)]
                             
                             gpd <- list(right_thresh = as.numeric(quantile(innovations,0.9)),
                                         left_thresh = as.numeric(quantile(innovations,0.1)),
                                         right_beta = results$right_tail_beta[counter],
                                         right_xi = results$right_tail_xi[counter],
                                         left_beta = results$left_tail_beta[counter],
                                         left_xi = results$left_tail_xi[counter])
                             
                             garch <- list(ar1_mu = results$mu[counter],
                                           ar1_phi = results$ar1[counter],
                                           garch_mu = results$omega[counter],
                                           garch_a = results$alpha1[counter],
                                           garch_b = results$beta1[counter])
                             
                             # Simulate random innovation
                             z <- rand_innovation(innovations, gpd)
                             innovations <- z$innovations
                             xt1 <- muFor + sigmaFor*z$z
                             
                             # Define variables for hday-loop
                             x <- xt1
                             mu_old <- muFor
                             sigma_old <- sigmaFor
                             
                             xt2_h <- lapply(1:(h-1),
                                             FUN <- function() 
                                             {
                                               # Calculate next day variables
                                               mu_new <- garch$ar1_phi*x
                                               eps <- x - mu_old
                                               sigma_new <- garch$garch_mu + garch$garch_a*eps
                                                            + garch$garch_a*sigma_old
                                               z <- rand_innovation(innovations, gpd)
                                               innovations <- z$innovations
                                               xt <- mu_new + sigma_new*z$z
                                               
                                               # Define variables for next loop
                                               x <- xt
                                               mu_old <- mu_new
                                               sigma_old <- sigma_new
                                               
                                               return(xt)
                                             })
                             
                             counter <<- counter + 1
                             return(c(xt1, xt2_h))
                           },
                           by.column=FALSE, align="right")
  VaR_results <- lag(VaR_results)
  return(VaR_results)  
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##