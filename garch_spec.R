garch_spec_n <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 0)), 
                              distribution.model ="norm")

tgarch_spec_n <- ugarchspec(variance.model = list(model = "fGARCH",  submodel = "TGARCH", garchOrder = c(1, 1)),
                             mean.model = list(armaOrder = c(1, 0)), 
                             distribution.model ="norm")

egarch_spec_n <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 0)), 
                              distribution.model ="norm")

spec <- list(garch = garch_spec_n, tgarch = tgarch_spec_n, egarch = egarch_spec_n)

