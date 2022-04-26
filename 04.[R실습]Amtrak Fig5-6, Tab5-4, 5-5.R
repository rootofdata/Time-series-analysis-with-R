# UTF-8

wd = getwd() # 현재 디렉토리
setwd(wd)    # 작업 디렉토리 설정

rm(list=ls())   # environment 창 변수 모두 지움
graphics.off()  # plots 창 그림 모두 지움 
cat("\014")     # console 창 메세지 모두 지움

#install.packages("forecast")
#install.packages("zoo")
#library(forecast)
#library(zoo)

## Data loading and preprocessing ##
Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

### Data partitioning ###
nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), 
                   end = c(1991, nTrain + nValid))

### Training ###
hwin <- ets(train.ts, model = "MAA")

### h-step ahead forecasting ###
hwin.pred <- forecast(hwin, h = nValid, level = c(80, 95)) # with forecast interval level

### Figure 5-6 ###
# training data + forecast values for validation period
plot(hwin.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))

# fitted values for training period
lines(hwin.pred$fitted, lwd = 2, col = "blue")

# validation data
lines(valid.ts)

# etc
lines(c(2004.25 - 3, 2004.25 - 3), c(0, 3500)) 
lines(c(2004.25, 2004.25), c(0, 3500))
text(1996.25, 2550, "Training")
text(2002.75, 2550, "Validation")
text(2005.25, 2550, "Future")
arrows(2004 - 3, 2450, 1991.25, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 2450, 2004, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 2450, 2006, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)

### Table 5.2 ###
hwin
hwin$states[1, ]  # Initial states
hwin$states[nrow(hwin$states), ]  # Final states
hwin$states

### Table 5.4 ###
# optimal training
ets.opt <- ets(y=train.ts, restrict=FALSE, allow.multiplicative.trend=TRUE)

### Table 5.5 ###
# validation error comparison
ets.opt.pred <- forecast(object=ets.opt, h=nValid, level=0)
plot(ets.opt.pred)
accuracy(hwin.pred, valid.ts)
accuracy(ets.opt.pred, valid.ts, opt.crit="mse", ic="aicc")
