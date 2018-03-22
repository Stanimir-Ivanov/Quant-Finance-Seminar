source("two_step_method.R")
library(SpatialExtremes)


##----------------------------------------------------------------------------------------------------------##
##Random innovation sumulation function.
##----------------------------------------------------------------------------------------------------------##
rand_innovation <- function(z_vec, k_high, k_low)
{
  n_rand <- as.numeric(sample.int(1:1000, 1, replace = TRUE))
  z_rand <- z_vec[n]
  
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
                             right_thres <- as.numeric(quantile(innovations,0.9))
                             left_thres <- as.numeric(quantile(innovations,0.1))
                             
                             right_beta <- results$right_tail_beta[counter]
                             right_xi <- results$right_tail_xi[counter]
                             left_beta <- results$left_tail_beta[counter]
                             left_xi <- results$left_tail_xi[counter]
                             
                             ar1_mu <- results$mu[counter]
                             ar1_phi <- results$ar1[counter]
                             
                             garch_mu <- results$omega[counter]
                             garch_a <- results$alpha1[counter]
                             garch_b <- results$beta1[counter]
                             
                             
                             xt1 <- muFor + sigmaFor*
                             
                             xt2_h <- lapply(1:(h-1),
                                             FUN <- function() 
                                             {
                                               
                                             })
                             
                             counter <<- counter + 1
                             return()
                           },
                           by.column=FALSE, align="right")
  VaR_results <- lag(VaR_results)
  return(VaR_results)  
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##