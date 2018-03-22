##----------------------------------------------------------------------------------------------------------##
##Main function.
##----------------------------------------------------------------------------------------------------------##
source("read_and_transform.R") # read data
source("day_ahead_var.R") # read day ahead VaR functions
source("garch_spec.R") # read garch specs
source("backtesting.R")

library(rugarch)
library(extRemes)

q <- c(0.95, 0.975, .99)

sp1 <-sp[2:length(sp)]

VaR_results <- day_ahead_VAR(specifications = spec$garch, data_1 = sp1, q)

##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##