estimate_garch <- function(garch_type, distribution, ar, ma, arch_lag, garch_lag, data_1, 
                           days_ahead, n_roll, n_outofsample)
{
  #Define specifications of the GARCH model based on the input values.
  specifications <- ugarchspec(variance.model = list(model = garch_type, garchOrder = c(arch_lag, garch_lag)),
                               mean.model = list(armaOrder = c(ar, ma), include.mean = FALSE), distribution.model = distribution)
  
  #Fit the GARCH model.
  garch_fit <- ugarchfit(spec = sepcifications, data = data_1)
  
  #Forecast according to the input values.
  garch_forecast <- ugarchforecast(specifications, data = data_1, n.ahead = days_ahead, 
                                   n.roll = n_roll, out.sample = n_outofsample)
  
  #Return the specifications and estimation of the GARCH model.
  return(list(a = specifications, b = garch_fit, c = garch_forecast))
}