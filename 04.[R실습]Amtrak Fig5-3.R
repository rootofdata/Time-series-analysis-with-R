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

### Data loading ###
Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

### Data partitioning ###
nValid <- 36 # 3년치
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))

### Training ###
ma.trailing <- rollmean(train.ts, k = 12, align = "right")

### Prediction for validation period ###
last.ma <- tail(ma.trailing, 1)
ma.trailing.pred <- ts(rep(last.ma, nValid), start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid), freq = 12)

### Figure 5-3 ###
# training period
plot(train.ts, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ma.trailing, lwd = 2)

# validation period
lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2) 
lines(valid.ts)

# etc
lines(c(2004.25 - 3, 2004.25 - 3), c(0, 3500)) 
lines(c(2004.25, 2004.25), c(0, 3500))
text(1996.25, 2500, "Training")
text(2002.75, 2500, "Validation")
text(2005.25, 2500, "Future")
arrows(2004 - 3, 2450, 1991.25, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 2450, 2004, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 2450, 2006, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)




