##----------------------------------------------------------------------------------------------------------##
##Main function.
##----------------------------------------------------------------------------------------------------------##
source("read_and_transform.R") # read data
source("day_ahead_var.R") # read day ahead VaR functions
source("garch_spec.R")

library(rugarch)
library(extRemes)

sp1 <-sp[2:length(sp)];
ing1 <- ing[2:length(ing)]
#Define specifications of the GARCH model based on the input values.

VaR_results <- day_ahead_VAR(specifications = spec$garch_t, data_1 = sp1)
VaR_results_ing <- day_ahead_VAR(specifications = spec$garch_t, data_1 = ing1)
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##