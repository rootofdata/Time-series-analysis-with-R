##########################################
####train period : 91.01 ~ 00.03##########
####validation period : 00.04 ~ 02.03#####
####testing period : 02.04 ~ 04.03########
##########################################
################Setting a working directory##################
setwd('C:/Users/dudtj/OneDrive - 숭실대학교 - Soongsil University/Desktop/대학자료/4-1/시계열 분석/실습')
getwd()

#######################필요 패키지 설치#######################
library(forecast)
library(zoo)
library(ggplot2)
#######################Loading the data#######################
Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, 
                   start = c(1991, 1), end = c(2004, 3), freq = 12)
Amtrak.data

#######################Data partitioning#######################
nValid <- 24 #  2 years
ntest <- 24  #  2 years
nTrain <- length(ridership.ts) - nValid- ntest
###############################################################
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), 
                   end = c(1991, nTrain + nValid))
test.ts <- window(ridership.ts, start = c(1991,  nTrain + nValid+1), 
                  end = c(1991, nTrain+nValid+ntest))

#### model1. naive forecast with roll-forward partitioning####
#table 3.3 참고
mae<-rep(0,nValid)
rmse<-rep(0,nValid)
mape<-rep(0,nValid)
for(i in 1:24){
  stepsAhead<-i
  error<-rep(0,24-stepsAhead+1)
  percent.error<-rep(0,24-stepsAhead+1)
  for(j in nTrain:(nTrain+nValid-stepsAhead)){
    train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, j))
    valid.ts <- window(ridership.ts, start = c(1991, j + stepsAhead), 
                       end = c(1991, j + stepsAhead))
    naive.pred <-naive(train.ts,h=stepsAhead)
    error[j-nTrain+1]<-valid.ts-naive.pred$mean[stepsAhead]
    percent.error[j-nTrain+1]<-error[j-nTrain+1]/valid.ts
  }
  mae[i]<-mean(abs(error))
  rmse[i]<-sqrt(abs(error^2))
  mape[i]<-mean(abs(percent.error))
}
accuracy(naive.pred,valid.ts)
mean(mae)
mean(rmse)
mean(mape)*100
# Setting again
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), 
                   end = c(1991, nTrain + nValid))
test.ts <- window(ridership.ts, start = c(1991,  nTrain + nValid+1), 
                  end = c(1991, nTrain+nValid+ntest))
####model2. trailing moving average with 12 months of window width###
ma.trailing <- rollmean(train.ts, k = 12, align = "right")
last.ma <- tail(ma.trailing, 1)
ma.trailing.pred <- ts(rep(last.ma, nValid), start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid), freq = 12)
accuracy(ma.trailing.pred,valid.ts)

####model3. Holt-Winter's DES with additive trend, multiplicative seasonality and multiplicative error###
hwin1 <- ets(train.ts,model='MAM',alpha=0.6,beta=0.1,gamma = 0.15)
hwin1.pred<-forecast(hwin1,h=nValid,level=0.95)
accuracy(hwin1.pred,valid.ts)
summary(hwin1.pred)

####model4. Holt-Winter's DES with additive trend, additive seasonality and additive error###
hwin2 <- ets(train.ts,model='AAA',alpha=0.5,beta=0.1,gamma=0.35)
hwin2.pred<-forecast(hwin2,h=nValid,level=0.95)
accuracy(hwin2.pred,valid.ts)
summary(hwin2.pred)

####model5. Regression model with quadratic trend and seasonal dummy variables###
train.lm.season <-tslm(train.ts ~trend+I(trend^2)+season)
summary(train.lm.season)
train.lm.season.pred <- forecast(train.lm.season,h=nValid,level=0.95)
accuracy(train.lm.season.pred,valid.ts)

####model6. Regression model with quadratic trend and trigonometric functions###
train.lm.trig<-tslm(train.ts~trend+I(trend^2)+I(sin(2*pi*trend/12))+I(cos(2*pi*trend/12)))
train.lm.trig.pred <- forecast(train.lm.trig,h=nValid,level=0.95)
summary(train.lm.trig)
accuracy(train.lm.trig.pred,valid.ts)

####Final model - model3###
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1),
                   end = c(1991, nTrain + nValid))
test.ts <- window(ridership.ts, start = c(1991,  nTrain + nValid+1), 
                  end = c(1991, nTrain+nValid+ntest))
final_train.ts<- window(ridership.ts, start = c(1991, 1), 
                        end = c(1991, nTrain + nValid))
hwin <- ets(final_train.ts,model='MAM',alpha=0.6,beta=0.1,gamma = 0.15)
#summary(hwin)
hwin.pred<-forecast(hwin,h=ntest,level=0.95)
#hwin.pred
accuracy(hwin.pred,test.ts)
summary(hwin.pred)


#####################Visualization###################################
#image1
plot(ridership.ts, xlab = "Months", ylab = "Ridership",
     main = "Amtrak Passengers Data", 
     ylim = c(1300,2300), bty = "l")

#image2
rider = ts(Amtrak.data$Ridership, frequency = 12, start = c(1991,1))
rider2 = window(rider, start = 1991,end=2004)
ggseasonplot(rider2, ylab = "Ridership") + ggtitle("Seasonal Plot : Ridership")
#image3
ggmonthplot(rider2, ylab = "Ridership") + ggtitle("Seasonal Subseries Plot: Ridership")
#image4
par(mfrow = c(2,2))
# raw series
plot(ridership.ts, ylab = "Ridership", xlab = "Time", bty = "l", xlim = c(1991,2004.25), main = "Ridership")
# lag-12 differenced series: deseason
plot(diff(ridership.ts, lag=12), ylab = "Lag-12", xlab = "Time", bty = "l", xlim = c(1991,2004.25), main = "Lag-12 Difference")
# lag-1 differenced series: detrend
plot(diff(ridership.ts, lag=1), ylab = "Lag-1", xlab = "Time", bty = "l", xlim = c(1991,2004.25), main = "Lag-1 Difference")
# lag-1 and 12 differenced series: detrend + deseason
plot(diff(diff(ridership.ts, lag=12), lag=1), ylab = "Lag-12, then Lag-1", xlab = "Time", bty = "l", xlim = c(1991,2004.25), main = "Twice-Differenced (Lag-12, Lag-1)")

######model visualization

etc<- function(){lines(c(2000.25, 2000.25), c(0, 3500)) 
  lines(c(2002.25, 2002.25), c(0, 3500)) 
  lines(c(2004.25, 2004.25), c(0, 3500))
  text(1995.55, 2550, "Training")
  text(2001.25, 2550, "Validation")
  text(2003.25, 2550, "Future")
  arrows(2000-0.5, 2450, 1991.25, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
  arrows(2000.45, 2450, 2002.2-0.1, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
  arrows(2002.45, 2450, 2004.2-0.1, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
  
}
####model1. naive forecast with roll-forward partitioning
plot(naive.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", main = "model1. naive forecast with roll-forward partitioning",  
     bty = "l", xaxt = "n", xlim = c(1991,2005.25), flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
# validation data
lines(valid.ts)
etc()

####model2. trailing moving average with 12 months of window width
plot(train.ts, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2005.25), main = "model2. trailing moving average with 12 months of window width")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ma.trailing, lwd = 2)
lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2) 
lines(valid.ts)
etc()

#### model3. Holt-Winter's DES with additive trend, multiplicative seasonality and multiplicative error
plot(hwin1.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", main = "model3. Holt-Winter's DES with additive trend, \nmultiplicative seasonality and multiplicative error",  
     bty = "l", xaxt = "n", xlim = c(1991,2005.25), flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts)
etc()

#### model4. Holt-Winter's DES with additive trend, additive seasonality and additive error
plot(hwin2.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", main = "model4. Holt-Winter's DES with additive trend, \nadditive seasonality and additive error",  
     bty = "l", xaxt = "n", xlim = c(1991,2005.25), flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts)
etc()

####model5. Regression model with quadratic trend and seasonal dummy variables
plot(train.lm.season.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", main = "model5. Regression model with quadratic trend and seasonal dummy variables",  
     bty = "l", xaxt = "n", xlim = c(1991,2005.25), flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts)
etc()
####model6. Regression model with quadratic trend and trigonometric functions
plot(train.lm.season.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", main = "model6. Regression model with quadratic trend and trigonometric functions",  
     bty = "l", xaxt = "n", xlim = c(1991,2005.25), flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(valid.ts)
etc()

####final model - model3
plot(hwin.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", main = "Holt-Winter's DES with additive trend, \nmultiplicative seasonality and multiplicative error",  
     bty = "l", xaxt = "n", xlim = c(1991,2005.25), flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
#### validation data
lines(valid.ts)
etc()
