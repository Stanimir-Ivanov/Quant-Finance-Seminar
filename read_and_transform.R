library(xts)
library(zoo)

# Import data and convert into time series format
sp <- read.zoo("./Data/SP-500.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)
ing <- read.zoo("./Data/ING.csv", header = TRUE, sep = ",",format="%Y/%m/%d",index.column = 1)
rds <- read.zoo("./Data/RDS-B.csv", header = TRUE, sep = ",",format="%Y/%m/%d",index.column = 1)
aex <- read.zoo("./Data/AEX.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)
# transform to xts
sp <- xts(sp)
ing <- xts(ing)
aex <-xts(aex)
rds <- xts(rds)
# Negative log returns
sp <- - 100*diff(log(sp))
ing <- - 100*diff(log(ing))
rds<- - 100*diff(log(rds))
aex<- - 100*diff(log(aex))