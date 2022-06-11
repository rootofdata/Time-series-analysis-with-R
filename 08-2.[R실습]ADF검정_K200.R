
getwd()
setwd("//")

rm(list=ls())
dev.off()

#install.packages("readxl") # to read excel data
#install.packages("xts") # to handle time-series data efficiently
#install.packages("fUnitRoots") # for Unit Root Test

############
### Data ###
############
library(readxl)
library(xts)

## Data loading
xlsdata = read_excel("data_K200.xlsx", sheet="data_m", col_types=c("date", "numeric", "numeric"))
xlsdata = data.frame(xlsdata)
head(xlsdata)

## Column selection
data = data.frame(K200=xlsdata[,2], row.names=as.Date(xlsdata[,1], format="%y-%m-%d"))
xts.data = as.xts(data) # convert to xts

## Row selection
par(mfrow=c(1,1), cex.main=1, cex.axis=1, cex.lab=1, lwd=2)
plot(xts.data, type = 'l', main="Price", xlab="Date", ylab="Price")
startdate = as.Date("1990-01-01")
enddate = as.Date("2019-04-15")
y = xts.data[paste(startdate, "::", enddate, sep = "")]

## Scaling
y = log(y)
y.lag1 = lag(y); colnames(y.lag1) = "K200.lag1"
dy = diff(y); colnames(dy) = "K200.I1"

y = y[-1,]
y.lag1 = y.lag1[-1,]
dy = dy[-1,]

(T = length(dy))

###########
### EDA ###
###########
pmax = as.integer(T^(1/3)) # Said and Dicky(1984)
#pmax = as.integer(12*(T/100)^(1/4)) # Schwert(1989)
#pmax = 50

par(mfrow=c(1,2))
plot(y, type = 'l', main="Price", xlab="Date", ylab="Price")
acf(ts(y), main="ACF", lag.max=pmax)

par(mfrow=c(1,1))
plot(dy, type = 'l', main="Price", xlab="Date", ylab="Price")

par(mfrow=c(2,1))
acf(ts(dy), main="ACF", lag.max=pmax) # try large pmax
pacf(ts(dy), main="ACF", lag.max=pmax)

Box.test(dy, lag=pmax)


#######################################
### ADF Test (H0: unit root exists) ###
#######################################
library(fUnitRoots)

### (Case 4) ###
tt = xts(x=c(NA, 1:(length(y)-1)), order.by=index(y), colnames=c("time")) # time trend
X = merge(tt, y.lag1) 

# Selecting p
pmax = 0 # lag of dy (not y)
arima.case4 = arima(dy, order=c(pmax,0,0), include.mean=TRUE, xreg=X)
tval.case4 = arima.case4$coef/sqrt(diag(arima.case4$var.coef))
(pval.case4 = pnorm(tval.case4))
Box.test(arima.case4$residuals, lag=10)

# Test
pmax = 0
(adf.case4 = adfTest(y, lags=pmax, type="ct"))
#adf.case4@test$lm
Box.test(adf.case4@test$lm$residuals, lag=10)

### (Case 2) ###
X = y.lag1

# Select p
pmax = 4
arima.case2 = arima(dy, order=c(c(pmax,0,0), include.mean=TRUE, xreg=X)
tval.case2 = arima.case2$coef/sqrt(diag(arima.case2$var.coef))
(pval.case2 = pnorm(tval.case2))
Box.test(arima.case2$residuals, lag=10)

# Test
pmax = 4
(adf.case2 = adfTest(y, lags=pmax, type="c"))
Box.test(adf.case2@test$lm$residuals, lag=10)

### (Case 1) ###
# Selecting p
pmax = 4
arima.case1 = arima(dy, order=c(pmax,0,0), include.mean=FALSE)
tval.case1 = arima.case1$coef/sqrt(diag(arima.case1$var.coef))
(pval.case1 = pnorm(tval.case1))
Box.test(arima.case1$residuals, lag=10)

# Test
pmax = 4
(adf.case1 = adfTest(y, lags=pmax, type="nc"))
Box.test(adf.case1@test$lm$residuals, lag=50)

### Stationarity check 
(adfTest(dy, lags=pmax, type="nc"))

