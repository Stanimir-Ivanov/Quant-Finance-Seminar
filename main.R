##----------------------------------------------------------------------------------------------------------##
##Main function.
##----------------------------------------------------------------------------------------------------------##
library(xts)
library(zoo)
library(SpatialExtremes)
library(rugarch)
library(extRemes)

source("read_and_transform.R") # read data
source("garch_spec.R") # read garch specs
source("backtesting.R")
source("VaR.R")

q <- c(.95, .99, .995)
h <- 10
n <- 1000

# sp
res_sp_garch <- VaR_estimation(specifications = spec$garch, data_1 = sp_data, q, h, n)
res_sp_tgarch <- VaR_estimation(specifications = spec$tgarch, data_1 = sp_data, q, h, n)
res_sp_egarch <- VaR_estimation(specifications = spec$egarch, data_1 = sp_data, q, h, n)

# ing
res_ing_garch <- VaR_estimation(specifications = spec$garch, data_1 = ing_data, q, h, n)
res_ing_tgarch <- VaR_estimation(specifications = spec$tgarch, data_1 = ing_data, q, h, n)
res_ing_egarch <- VaR_estimation(specifications = spec$egarch, data_1 = ing_data, q, h, n)

# rds
res_rds_garch <- VaR_estimation(specifications = spec$garch, data_1 = rds_data, q, h, n)
res_rds_tgarch <- VaR_estimation(specifications = spec$tgarch, data_1 = rds_data, q, h, n)
res_rds_egarch <- VaR_estimation(specifications = spec$egarch, data_1 = rds_data, q, h, n)

# aex
res_aex_garch <- VaR_estimation(specifications = spec$garch, data_1 = aex_data, q, h, n)
res_aex_tgarch <- VaR_estimation(specifications = spec$tgarch, data_1 = aex_data, q, h, n)
res_aex_egarch <- VaR_estimation(specifications = spec$egarch, data_1 = aex_data, q, h, n)
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##