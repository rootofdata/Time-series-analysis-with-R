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
diff.twice.ts <- diff(diff(ridership.ts, lag = 12), lag = 1)

### Data partitioning ###
nValid <- 36
nTrain <- length(diff.twice.ts) - nValid
train.ts <- window(diff.twice.ts, start = c(1992, 2), end = c(1992, nTrain + 1))
valid.ts <- window(diff.twice.ts, start = c(1992, nTrain + 2), 
                   end = c(1992, nTrain + 1 + nValid))

### Training ###
ses <- ets(train.ts, model="ANN", alpha=0.2)

### h-step ahead forecasting ###
ses.pred <- forecast(ses, h=nValid)

### Figure 5-5 ###
# training data + forecast values for validation period
plot(ses.pred, ylim = c(-250, 300),  ylab = "Ridership (Twice-Differenced)", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main ="", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))

# fitted values for training period
lines(ses.pred$fitted, lwd = 2, col = "blue")

# validation data
lines(valid.ts)

# etc
lines(c(2004.25 - 3, 2004.25 - 3), c(-250, 350)) 
lines(c(2004.25, 2004.25), c(-250, 350))
text(1996.25, 275, "Training")
text(2002.75, 275, "Validation")
text(2005.25, 275, "Future")
arrows(2004 - 3, 245, 1991.5, 245, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 245, 2004, 245, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 245, 2006, 245, code = 3, length = 0.1, lwd = 1, angle = 30)

dev.off()

### Table 5.1 ###
# optimal fitting
ses
ses.opt <- ets(train.ts, model="ANN") # α를 지정하지 않음
ses.opt.pred <- forecast(ses.opt, h=nValid, level=0)
ses.opt
summary(ses.opt) 

# validation error comparison
accuracy(ses.pred, valid.ts)
accuracy(ses.opt.pred, valid.ts)

?ets #exponential smoothing 해주는 모델 
#model='zzz' : 알아서 수정해주는 것.


b<-matrix(1:9,nrow=3)
colnames(b)<-

b[0:-2]

