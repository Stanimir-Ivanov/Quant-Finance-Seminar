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
source("backtesting_main.R")
source("VaR.R")

q <- c(.95, .99, .995)
h <- 10
n <- 1000

# sp
res_sp_garch <- VaR_estimation(specifications = spec$garch, data_1 = sp_data, q, h, n) # Stan
res_sp_tgarch <- VaR_estimation(specifications = spec$tgarch, data_1 = sp_data, q, h, n) # Stan
res_sp_egarch <- VaR_estimation(specifications = spec$egarch, data_1 = sp_data, q, h, n) # Stan

# gold
res_gold_garch <- VaR_estimation(specifications = spec$garch, data_1 = gold_data, q, h, n) # Stan
res_gold_tgarch <- VaR_estimation(specifications = spec$tgarch, data_1 = gold_data, q, h, n) # Tommaso
res_gold_egarch <- VaR_estimation(specifications = spec$egarch, data_1 = gold_data, q, h, n) # Thomas	

# rds
res_rds_garch <- VaR_estimation(specifications = spec$garch, data_1 = rds_data, q, h, n) # Tommaso
res_rds_tgarch <- VaR_estimation(specifications = spec$tgarch, data_1 = rds_data, q, h, n) # Tommaso 
res_rds_egarch <- VaR_estimation(sp0ecifications = spec$egarch, data_1 = rds_data, q, h, n) # Thomas

# aex
res_aex_garch <- VaR_estimation(specifications = spec$garch, data_1 = aex_data, q, h, n) # Thomas
res_aex_tgarch <- VaR_estimation(specifications = spec$tgarch, data_1 = aex_data, q, h, n) # Thomas
res_aex_egarch <- VaR_estimation(specifications = spec$egarch, data_1 = aex_data, q, h, n) # Thomas

# jpm
res_jpm_garch <- VaR_estimation(specifications = spec$garch, data_1 = jpm_data, q, h, n) # 
res_jpm_tgarch <- VaR_estimation(specifications = spec$tgarch, data_1 = jpm_data, q, h, n) # 
res_jpm_egarch <- VaR_estimation(specifications = spec$egarch, data_1 = jpm_data, q, h, n) # 

# bac
res_bac_garch <- VaR_estimation(specifications = spec$garch, data_1 = bac_data, q, h, n) # 
res_bac_tgarch <- VaR_estimation(specifications = spec$tgarch, data_1 = bac_data, q, h, n) # 
res_bac_egarch <- VaR_estimation(specifications = spec$egarch, data_1 = bac_data, q, h, n) # 

##----------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------##