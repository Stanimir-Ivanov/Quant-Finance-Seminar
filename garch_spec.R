garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 0)), 
                              distribution.model ="norm")

tgarch <- ugarchspec(variance.model = list(model = "fGARCH",  submodel = "TGARCH", garchOrder = c(1, 1)),
                             mean.model = list(armaOrder = c(1, 0)), 
                             distribution.model ="norm")

egarch <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 0)), 
                              distribution.model ="norm")

spec <- list(garch = garch, tgarch = tgarch, egarch = egarch)

