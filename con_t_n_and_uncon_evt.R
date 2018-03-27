# Import data by read.zoo and pass it into function "data_read"
# Function "data_read" convert data into suitable format 
# then run garch_normal and garch_t

# an example of importing data is the following: 
sp <- read.zoo("SP-500.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)

# step1. convert data 
# step2. run data in garch_normal and garch_t 
# reprot the relevant var value

data_read <- function(data1)
{
# data1 <- read.zoo("SP-500.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)
data1 <- xts(data1)
data1 <- 100*diff(log(data1))
data1 <-data1[2: length(data1)]
return (data1)
}

garch_normal <- function(data1)
{
  sp_garch_spec_n <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                                mean.model = list(armaOrder = c(1, 0)) #include.mean=FALSE)
                                ,distribution.model ="norm")

  sp_garch_n <-ugarchroll(spec =sp_garch_spec_n,data =data1, n.ahead = 1, 
                        n.start = 1000,
                        refit.every = 1, refit.window = "moving",
                        calculate.VaR = TRUE, VaR.alpha = c(0.005,0.01, 0.05))
   
}
garch_t <-function(data1)
{
  sp_garch_spec_t <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                                mean.model = list(armaOrder = c(1, 0)),distribution.model ="std")
  # testing VaR with Student's t
  sp_garch_t <-ugarchroll(spec =sp_garch_spec_t,data =data1, n.ahead = 1, 
                          n.start = 1000,
                          refit.every = 1, refit.window = "moving",
                          calculate.VaR = TRUE, VaR.alpha = c(0.005,0.01, 0.05))
}

# example of reporting 
report(sp_garch_t, type="VaR", VaR.alpha = 0.005, conf.level = 0.95)    
report(sp_garch_t, type="VaR", VaR.alpha = 0.01, conf.level = 0.95) 
report(sp_garch_t, type="VaR", VaR.alpha = 0.05, conf.level = 0.95) 
report(sp_garch_t, type="fpm")

# function uncondiEvT calculate the VaR based on unciondtional EVT, i.e. apply GPD on raw data 
# return 3 levels VaR 0.95, 0.99 and 0.995
# VaR needs to be passed into backtesting function
uncondiEvT <-function (data1)
{
  rollapply(data1,
            width = 1000, 
            FUN= function(sp1)
            { 
              k <- 0.90  #  threshold quantile choice
              gpd_fit <-fevd(data1,type="GP",threshold= quantile(data1,k))
              coef <- gpd_fit$results$par
              mu <- mean(data1)
              sigma <- sd(data1)
              xi <-coef[2]
              beta <-coef[1]
              q1 <- 0.95   # VaR quantile 
              q2 <- 0.99
              q3 <- 0.995
              zq95 <- quantile(data1,(k-0.001)) + (beta/xi)* (((1-q1)/(1-k))^(-xi) -1)
              zq99 <- quantile(data1,(k-0.001)) + (beta/xi)* (((1-q2)/(1-k))^(-xi) -1)
              zq995 <- quantile(data1,(k-0.001)) + (beta/xi)* (((1-q3)/(1-k))^(-xi) -1)
              var95 <-mu + sigma *zq95
              var99 <-mu + sigma *zq99
              var995 <-mu + sigma *zq995
              return (list(var95,var99,var995))
            },
            by.column=FALSE, align="right",na.pad=FALSE)
}



