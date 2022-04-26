wd=getwd('C:/Users/dudtj/OneDrive - 숭실대학교 - Soongsil University/Desktop/대학자료/4-1/시계열 분석')
setwd('C:/Users/dudtj/OneDrive - 숭실대학교 - Soongsil University/Desktop/대학자료/4-1/시계열 분석')
#install.packages("forecast")
#install.packages('zoo')
library(forecast)
library(zoo)

###data loading###
Amtrak.data <-read.csv("Amtrak data.csv")
ridership.ts <-ts(Amtrak.data$Ridership,start=c(1991,1),end=c(2004,3),freq=12) #ts=timeseries

###data partitioning###
nValid<-36
nTrain<-length(ridership.ts)-nValid
train.ts<-window(ridership.ts,start=c(1991,1),end=c(1991,nTrain))
valid.ts <- window(ridership.ts,start=c(1991,nTrain+1),end=c(1991,nTrain+nValid))

###Train###
ma.trailing <-rollmean(train.ts,k=12,align="right")

###prediction for validation period###
last.ma<-tail(ma.trailing,1)
ma.trailing.pred <-ts(rep(last.ma,nValid),start=c(1991,nTrain+1),end=c(1991,nTrain+nValid),freq=12)

###figure 5-3 ###
#training period
plot(train.ts,ylim=c(1300,2600),ylab='ridership',xlab='time',bty='l',xaxt='n',xlim=c(1991,2006.25),main="")
axis(1,at=seq(1991,2006,1),labels=format(seq(1991,2006,1)))
lines(ma.trailing,lwd=2)

#validation period
lines(ma.trailing.pred,lwd=2,col='blue',lty=2)
lines(valid.ts)

#etc
lines(c(2004.25-3,2004.25-3),c(0,3500))
lines(c(2004.25,2004.25),c(0,3500))
text(1996.25,2500,'training')
text(2002.75,2500,'validation')
text(2005.25,2500,'future')
arrows(2004-3,2450,1991.25,2450,code=3,length=0.1,lwd=1,angle=30)
arrows(2004.5-3,2450,2004,2450,code=3,length=0.1,lwd=1,angle=30)
arrows(2004.5,2450,2006,2450,code=3,length=0.1,lwd=1,angle=30)


#############2번째 실습######################
#Figure 5-4
par(mfrow=c(2,2))

#Raw series
plot(ridership.ts,ylab='ridership',xlab='time',bty='l',xlim=c(1991,2004.25),main='ridership')
summary(tslm(ridership.ts~trend+season)) #check time trend and seasonality rough

#lag-12 differenced series : deseason
plot(diff(ridership.ts,lag=12),ylab='Lag-12',xlab='time',bty='l',xlim=c(1991,2004.25),main='ridership')
summary(tslm(diff(ridership.ts,lag=12)~trend+season)) #check time trend and seasonality rough

#lag-1 differenced series : detrend
plot(diff(ridership.ts,lag=1),ylab='Lag-12',xlab='time',bty='l',xlim=c(1991,2004.25),main='ridership')
summary(tslm(diff(ridership.ts,lag=1)~trend+season)) #check time trend and seasonality rough

#lag-1 and 12 differenced series : detrend +deseason
plot(diff(diff(ridership.ts,lag=12),lag=1),ylab='Lag-12',xlab='time',bty='l',xlim=c(1991,2004.25),main='ridership')
summary(tslm(diff(diff(ridership.ts,lag=12),lag=1)~trend+season)) #check time trend and seasonality rough