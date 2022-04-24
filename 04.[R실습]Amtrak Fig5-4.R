# UTF-8

wd = getwd() # 현재 디렉토리
setwd(wd)    # 작업 디렉토리 설정

rm(list=ls())   # environment 창 변수 모두 지움
graphics.off()  # plots 창 그림 모두 지움 
cat("\014")     # console 창 메세지 모두 지움

#install.packages("forecast")
#install.packages("zoo")
library(forecast)
library(zoo)

## Data loading ##
Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

### Figure 5-4 ###
par(mfrow = c(2,2))

# raw series
plot(ridership.ts, ylab = "Ridership", xlab = "Time", bty = "l", xlim = c(1991,2004.25), main = "Ridership")
summary(tslm(ridership.ts ~ trend + season)) # check time trend and seasonality roughly

# lag-12 differenced series: deseason
plot(diff(ridership.ts, lag=12), ylab = "Lag-12", xlab = "Time", bty = "l", xlim = c(1991,2004.25), main = "Lag-12 Difference")
summary(tslm(diff(ridership.ts, lag=12) ~ trend + season))

# lag-1 differenced series: detrend
plot(diff(ridership.ts, lag=1), ylab = "Lag-1", xlab = "Time", bty = "l", xlim = c(1991,2004.25), main = "Lag-1 Difference")
summary(tslm(diff(ridership.ts, lag=1) ~ trend + season))

# lag-1 and 12 differenced series: detrend + deseason
plot(diff(diff(ridership.ts, lag=12), lag=1), ylab = "Lag-12, then Lag-1", xlab = "Time", bty = "l", xlim = c(1991,2004.25), main = "Twice-Differenced (Lag-12, Lag-1)")
summary(tslm(diff(diff(ridership.ts, lag=12), lag=1) ~ trend + season))

dev.off()
