##----------------------------------------------------------------------------------------------------------##
##Main function.
##----------------------------------------------------------------------------------------------------------##
library(xts)
library(zoo)
library(SpatialExtremes)
library(rugarch)
library(extRemes)

source("read_and_transform.R") # read data
source("day_ahead_var.R") # read day ahead VaR functions
source("hday_ahead_VaR.R")
source("garch_spec.R") # read garch specs
source("backtesting.R")
source("VaR.R")

q <- c(0.95, 0.975, .99)

sp1 <-sp[2:length(sp)]
sp_test <- sp[2:1005]

# VaR_results <- day_ahead_VAR(specifications = spec$garch, data_1 = sp1, q)
VaR_results <- VaR_estimation(specifications = spec$garch, data_1 = sp_test, q, 10, 1000)

##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##