library(xts)
library(rugarch)
library(zoo)

sp <- read.zoo("SP-500.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)
sp <- xts(sp)
sp <- 100*diff(log(sp))
sp <-sp[2:8405]


sp_garch_spec_t <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 0)),distribution.model ="std")
# testing VaR with Student's t
sp_garch_t <-ugarchroll(spec =sp_garch_spec_t,data =sp, n.ahead = 1, 
                        n.start = 1000,
                        refit.every = 1, refit.window = "moving",
                        calculate.VaR = TRUE, VaR.alpha = c(0.005,0.01, 0.05))
report(sp_garch_t, type="VaR", VaR.alpha = 0.005, conf.level = 0.95)    
report(sp_garch_t, type="VaR", VaR.alpha = 0.01, conf.level = 0.95) 
report(sp_garch_t, type="VaR", VaR.alpha = 0.05, conf.level = 0.95) 
report(sp_garch_t, type="fpm")


# GARCH with Normal distribution 
sp_garch_spec_n <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 0)) #include.mean=FALSE)
                              ,distribution.model ="norm")

sp_garch_n <-ugarchroll(spec =sp_garch_spec_n,data =sp, n.ahead = 1, 
                        n.start = 1000,
                        refit.every = 1, refit.window = "moving",
                        calculate.VaR = TRUE, VaR.alpha = c(0.005,0.01, 0.05))
report(sp_garch_n, type="VaR", VaR.alpha = 0.005, conf.level = 0.95)    
report(sp_garch_n, type="VaR", VaR.alpha = 0.01, conf.level = 0.95) 
report(sp_garch_n, type="VaR", VaR.alpha = 0.05, conf.level = 0.95) 
report(sp_garch_n, type="fpm")
 