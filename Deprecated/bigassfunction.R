##----------------------------------------------------------------------------------------------------------##
##Main function.
##----------------------------------------------------------------------------------------------------------##
#Define specifications of the GARCH model based on the input values.
source("day_ahead_var.R")
library(rugarch)
library(extRemes)
sp1 <-sp[2:1010];
sp_garch_spec_t <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 0), include.mean = FALSE), 
                              distribution.model ="std")

VaR_results <- VAR_estimation(specifications = sp_garch_spec_t, data_1 = sp1)
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##