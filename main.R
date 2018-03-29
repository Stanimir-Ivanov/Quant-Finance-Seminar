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

# after you are done
# setwd() to repo beforehand
# save(res_dataName_specName, '.\Data\condi_evt\res_dataName_specName.RData')

# sp
res_sp_garch <- VaR_estimation(specifications = spec$garch, data_1 = sp_data, q, h, n) # done
res_sp_tgarch <- VaR_estimation(specifications = spec$tgarch, data_1 = sp_data, q, h, n) # Stan
res_sp_egarch <- VaR_estimation(specifications = spec$egarch, data_1 = sp_data, q, h, n) # Stan

# ing
res_ing_garch <- VaR_estimation(specifications = spec$garch, data_1 = ing_data, q, h, n) # Stan
res_ing_tgarch <- VaR_estimation(specifications = spec$tgarch, data_1 = ing_data, q, h, n) # Stan
res_ing_egarch <- VaR_estimation(specifications = spec$egarch, data_1 = ing_data, q, h, n) # Tommaso	

# rds
res_rds_garch <- VaR_estimation(specifications = spec$garch, data_1 = rds_data, q, h, n) # Tommaso
res_rds_tgarch <- VaR_estimation(specifications = spec$tgarch, data_1 = rds_data, q, h, n) # Tommaso 
res_rds_egarch <- VaR_estimation(sp0ecifications = spec$egarch, data_1 = rds_data, q, h, n) # Tommaso

# aex
res_aex_garch <- VaR_estimation(specifications = spec$garch, data_1 = aex_data, q, h, n) # Thomas
res_aex_tgarch <- VaR_estimation(specifications = spec$tgarch, data_1 = aex_data, q, h, n) # Thomas
res_aex_egarch <- VaR_estimation(specifications = spec$egarch, data_1 = aex_data, q, h, n) # Thomas
##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##