library(forecast)
####1###
##1(a)##
#cleaning up the Sept11Travel.csv file before converting to a time series
#mutate the Rail and Air miles to per 1,000,000 miles

Travel <- read.csv("Sept11Travel.csv")
Travel <- Travel %>%
  mutate(Rail_Miles = ((round(((Rail/1000000)), digits=1)))) %>%
  mutate(Air_Miles = ((round(((Air/1000000)), digits=1))))

Travel <- select(Travel, "Month", "Rail_Miles", "Air_Miles", "VMT")
colnames(Travel) <- c("Month", "Rail Miles (mm)", "Air Miles (mm)", "Car Miles")
Travel <- ts(Travel[,2:4], start = 1990, frequency = 12)

#plot the three (Rail, Air, and Car) charts indicating miles traveled
autoplot(Travel, facets = TRUE, xlab="Time", ylab="Miles Traveled", main="Miles Travelled by Month from 1990 to 2004")

#수준 - 계열의 평균 값을 설명합니다.
#추세 - 한 기간에서 다음 기간으로의 계열 변화입니다.
#계절성 - 주어진 계열 내에서 여러 번 관찰할 수 있는 단기 순환 동작을 설명합니다.
#노이즈 - 시계열의 비체계적인 부분입니다.

#위의 이벤트 전 시계열 플롯을 기반으로 철도, 항공 또는 자동차로 여행하는 계절성 경향이 있습니다. ggsubseriesplots에서 상대적으로 일관된 수준이 존재하고 계절적 요인이 작용할 뿐만 아니라 여행하는 사람들의 수가 증가함에 따라 전반적인 추세가 있다는 중요한 표시가 있음을 알 수 있습니다.

###2###