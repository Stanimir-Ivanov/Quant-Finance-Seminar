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

q <- c(.95, .99, .995)

##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##