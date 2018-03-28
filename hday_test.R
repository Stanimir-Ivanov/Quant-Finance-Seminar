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
                              row_row_results = results[i,]
                              muFor <- row_results$muFor 
                              sigmaFor <- row_results$sigmaFor 
                              innovations <- as.data.frame(row_results[i, (length(row_results) - 999)
                                                                   :length(row_results)])
                              
                              gpd <- list(right_thresh = as.numeric(quantile(innovations,0.9)),
                                          left_thresh = as.numeric(quantile(innovations,0.1)),
                                          right_beta = row_results$right_tail_beta ,
                                          right_xi = row_results$right_tail_xi ,
                                          left_beta = row_results$left_tail_beta ,
                                          left_xi = row_results$left_tail_xi )
                              
                              garch <- list(ar1_mu = row_results$mu ,
                                            ar1_phi = row_results$ar1 ,
                                            garch_mu = row_results$omega ,
                                            garch_a = row_results$alpha1 ,
                                            garch_b = row_results$beta1 )
                              # 
                              # # Simulate random innovation
                              # z <- rand_innovation(innovations, gpd)
                              # innovations <- z$innovations
                              # print(sigmaFor)
                              # print(z$z)
                              # print(muFor)
                              # xt1 <- muFor + sigmaFor*as.numeric(z$z)
                              # print(xt1)
                              # 
                              # # Define variables for hday-loop
                              # x <- xt1
                              # mu_old <- muFor
                              # sigma_old <- sigmaFor
                              # 
                              # days <- 1:(h-1)
                              # xt2_h <- lapply(days,
                              #                 HDAYS <- function(j) 
                              #                 {
                              #                   # Calculate next day variables
                              #                   h <- j
                              #                   mu_new <- garch$ar1_phi*x
                              #                   eps <- x - mu_old
                              #                   sigma_new <- as.numeric(garch$garch_mu + garch$garch_a*eps)
                              #                   + garch$garch_a*sigma_old
                              #                   z <- rand_innovation(innovations, gpd)
                              #                   innovations <- z$innovations
                              #                   xt <- mu_new + sigma_new*as.numeric(z$z)
                              #                   
                              #                   # Define variables for next loop
                              #                   x <- xt
                              #                   mu_old <- mu_new
                              #                   sigma_old <- sigma_new
                              #                   
                              #                   return(xt)
                              #                 })
                               
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
                                               sigma_t <- (garch$garch_mu + garch$garch_a*eps^2 + garch$garch_b*sigma_t^2)^(1/2)
                                               return(xt)
                                             })
                              return(xt_h)
                            })
  return(hday_simulation)
}