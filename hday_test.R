source("two_step_method.R")
library(SpatialExtremes)


##----------------------------------------------------------------------------------------------------------##
##Random innovation sumulation function.
##----------------------------------------------------------------------------------------------------------##
rand_innovation <- function(z_vec, gpd)
{
  print(length(z_vec))
  r <- sample(1:1000, 1)
  if (z_vec[r] > gpd$right_thresh)
  {
    z_vec[r] <- gpd$right_thresh + rgpd(1, loc = 0, gpd$right_beta, gpd$right_xi)
  }
  if (z_vec[r] < gpd$left_thresh)
  {
    z_vec[r] <- gpd$left_thresh - rgpd(1, loc = 0, gpd$left_beta, gpd$left_xi)
  }
  return(list(innovations = z_vec, z = z_vec[r]))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##





##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##
hday_loop <- function(data_1, results, h)
{
  counter <- 1:length(data_1)
  hday_simulation <- lapply(counter, SIM <- function(i)
                            {
                              # Define parameters
                              muFor <- results$muFor[i]
                              sigmaFor <- results$sigmaFor[i]
                              innovations <- as.data.frame(results[i, (length(results[1,]) - 999)
                                                                   :length(results[1,])])
                              
                              gpd <- list(right_thresh = as.numeric(quantile(innovations,0.9)),
                                          left_thresh = as.numeric(quantile(innovations,0.1)),
                                          right_beta = results$right_tail_beta[i],
                                          right_xi = results$right_tail_xi[i],
                                          left_beta = results$left_tail_beta[i],
                                          left_xi = results$left_tail_xi[i])
                              
                              garch <- list(ar1_mu = results$mu[i],
                                            ar1_phi = results$ar1[i],
                                            garch_mu = results$omega[i],
                                            garch_a = results$alpha1[i],
                                            garch_b = results$beta1[i])
                              
                              # Simulate random innovation
                              z <- rand_innovation(innovations, gpd)
                              innovations <- z$innovations
                              print(sigmaFor)
                              print(z$z)
                              print(muFor)
                              xt1 <- muFor + sigmaFor*as.numeric(z$z)
                              print(xt1)
                              
                              # Define variables for hday-loop
                              x <- xt1
                              mu_old <- muFor
                              sigma_old <- sigmaFor
                              
                              days <- 1:(h-1)
                              xt2_h <- lapply(days,
                                              HDAYS <- function(j) 
                                              {
                                                # Calculate next day variables
                                                h <- j
                                                mu_new <- garch$ar1_phi*x
                                                eps <- x - mu_old
                                                sigma_new <- as.numeric(garch$garch_mu + garch$garch_a*eps)
                                                + garch$garch_a*sigma_old
                                                z <- rand_innovation(innovations, gpd)
                                                innovations <- z$innovations
                                                xt <- mu_new + sigma_new*as.numeric(z$z)
                                                
                                                # Define variables for next loop
                                                x <- xt
                                                mu_old <- mu_new
                                                sigma_old <- sigma_new
                                                
                                                return(xt)
                                              })
                              
                              mu_t <- muFor
                              sigma_t <- sigmaFor
                              
                              hdays <- 1:h
                              xt_h <- lapply(hdays,
                                             HDAYS <- function(j) 
                                             {
                                               d <- j
                                               z <- rand_innovation(innovations, gpd)
                                               innovations <- z$innovations
                                               xt <- mu_t + sigma_t * as.numeric(z$z)
                                               
                                               eps <- xt - mu_t
                                               mu_t <- garch$ar1_phi*xt
                                               sigma_t <- garch$garch_mu + garch$garch_a*eps + garch$garch_a*sigma_t
                                               return(xt)
                                             })
                              return(c(xt1, as.vector(xt2_h)))
                            })
  return(hday_simulation)
}