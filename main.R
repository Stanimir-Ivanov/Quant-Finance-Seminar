##----------------------------------------------------------------------------------------------------------##
##Main function.
##----------------------------------------------------------------------------------------------------------##
source("read_and_transform.R") # read data
source("day_ahead_var.R") # read day ahead VaR functions

library(rugarch)
library(extRemes)

sp1 <-sp[2:length(sp)];
#Define specifications of the GARCH model based on the input values.
sp_garch_spec_t <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 0), include.mean = FALSE), 
                              distribution.model ="norm")

VaR_results <- day_ahead_VAR(specifications = sp_garch_spec_t, data_1 = sp1)
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##