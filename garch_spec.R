garch_spec_t <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 0), include.mean = FALSE), 
                              distribution.model ="std")

garch_spec_n <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 0), include.mean = FALSE), 
                              distribution.model ="norm")

spec <- list(garch_t = garch_spec_t, garch_n = garch_spec_n)

