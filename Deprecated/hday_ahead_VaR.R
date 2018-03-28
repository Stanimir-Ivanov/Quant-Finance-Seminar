##----------------------------------------------------------------------------------------------------------##
##Random innovation sumulation function.
##----------------------------------------------------------------------------------------------------------##
rand_innovation <- function(z_vec, gpd)
{
  r <- sample(1:1000, 1)
  if (z_vec[r] > gpd$right_thresh)
  {
    z_vec[r] <- gpd$right_thresh + rgpd(1, loc = 0, scale = gpd$right_beta, shape = gpd$right_xi)
  }
  if (z_vec[r] < gpd$left_thresh)
  {
    z_vec[r] <- gpd$left_thresh - rgpd(1, loc = 0, scale = gpd$left_beta, shape = gpd$left_xi)
  }
  return(list(innovations = z_vec, z = z_vec[r]))
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##





##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##
hday_VaR <- function(data_length, results, h, q, n)
{
  counter <- as.matrix(1:data_length)
  hday_simulation <- apply(counter, MARGIN = 1, SIM <- function(i)
  {
    # Define parameters
    row_results = results[i,]                              
    innovations <- as.data.frame(row_results[, (length(row_results) - 999)
                                             :length(row_results)])
    
    gpd <- list(right_thresh = as.numeric(quantile(innovations,0.9)),
                left_thresh = as.numeric(quantile(innovations,0.1)),
                right_beta = row_results$right_tail_beta,
                right_xi = row_results$right_tail_xi,
                left_beta = row_results$left_tail_beta,
                left_xi = row_results$left_tail_xi)
    
    garch <- list(ar1_mu = row_results$mu ,
                  ar1_phi = row_results$ar1 ,
                  garch_mu = row_results$omega ,
                  garch_a = row_results$alpha1 ,
                  garch_b = row_results$beta1)                                                           
    
    mu_t <- row_results$muFor 
    sigma_t <- row_results$sigmaFor 
    
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
                          mu_t <- garch$ar1_phi*xt
                          sigma_t <- (garch$garch_mu + garch$garch_a*eps^2 + garch$garch_b*sigma_t^2)^(1/2)
                          return(xt)
                        })))
    })
    return(quantile(paths, q))
  })
  dates <- as.Date(time(results))
  dates <- dates[1:data_length]
  hday_VaR <- xts(t(hday_simulation), dates)
  return(hday_VaR)
}
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##