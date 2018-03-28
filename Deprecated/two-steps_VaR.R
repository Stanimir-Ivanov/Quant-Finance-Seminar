# Two step methonds for estimating VaR based on GARCH and EVT 

# GARCH with Normal distribution 

sp_garch_spec_n <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(1, 0)) #include.mean=FALSE)
                              ,distribution.model ="norm")

sp_garch_n1 <-ugarchfit(spec =sp_garch_spec_n ,data = sp1)
# conditional sigma , mu and standardized residuals
condi_sigma <-sp_garch_n1@fit$sigma 
inno_z <-sp_garch_n1@fit$z
condi_mu <- sp2 - condi_sigma*inno_z
garch_res <- sp_garch_n1@fit$residuals
std_res <-(garch_res - condi_mu)/condi_sigma
acf(abs(std_res))

sp1 <-sp[1:1005]
#sp1 <-as.data.frame(sp1)
recur_sp_VaR <-rollapply(sp1,
                     width = 1000, 
                     FUN= function(sp1)
                     { # fit GARCH, get standardized residuals
                       recur_garch<- ugarchfit(spec =sp_garch_spec_n ,data =as.data.frame(sp1))
                       res_garch <-recur_garch@fit$z
                       
                       # GARCH 1-step ahead forecast, get mu_t+1 and sigma_t+1
                       recur_garch_forecast <-ugarchforecast(recur_garch,
                                                             data =as.data.frame(sp1),n.ahead=1)
                       fore_mu<- recur_garch_forecast@forecast$seriesFor
                       fore_sigma<- recur_garch_forecast@forecast$sigmaFor
                       # fit standardized residuals in to GPD
                       k <- 0.90  #  threshold quantile choice
                       gpd_fit <-fevd(res_garch,type="GP",threshold= quantile(res_garch,k))
                       coef <- gpd_fit$results$par
                       xi <-coef[2]
                       beta <-coef[1]
                       q <- 0.95   # VaR quantile 
                       zq <- quantile(res_garch,(k-0.001)) + (beta/xi)* (((1-q)/(1-k))^(-xi) -1)
                       var <-fore_mu + fore_sigma *zq
                       return (var)
                     },
                     by.column=FALSE, align="right",na.pad=FALSE)

# sp_garch_roll <-ugarchroll(spec =sp_garch_spec_n,data =sp1, n.ahead = 1, 
#                         n.start = 1000,
#                         refit.every = 1, refit.window = "moving",
#                         calculate.VaR = TRUE, VaR.alpha = c(0.005,0.01, 0.05))
# sp_garch_roll@forecast$density[1:2]
