library(fpp2)

setwd("C:\\Users\\JB Aposol\\Downloads")
stocks <- read.csv("Low.csv")
summary(stocks)

stocks_ts <- ts(stocks$Low_k,start=c(2020,6),frequency=12)
autoplot(stocks_ts)


#Naive
fit<-naive(stocks_ts)
print(summary(fit))
checkresiduals(fit)

#Seasonal Naive
fit_snaive<-snaive(stocks_ts)
print(summary(fit_snaive))
checkresiduals(fit_snaive)
fcst_snaive<-forecast(fit_snaive, h=7)

#Arima
fit_arima<-auto.arima(stocks_ts,d=4,stepwise = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)

#Forecast with Arima
fcst<-forecast(fit_arima, h=7)
autoplot(fcst)
print(summary(fcst))

#Differencing
stocks_ts_d1 <- diff(stocks_ts, differences = 1)
autoplot(stocks_ts_d1) 

Pacf(stocks_ts_d1)
Acf(stocks_ts_d1)

stocks_ts_d2 <- diff(stocks_ts, differences = 2)
autoplot(stocks_ts_d2) 

Pacf(stocks_ts_d2)
Acf(stocks_ts_d2)

stocks_ts_d3 <- diff(stocks_ts, differences = 3)
autoplot(stocks_ts_d3) 

Pacf(stocks_ts_d3)
Acf(stocks_ts_d3)

#Plotting
plot(fit)
plot(fit_snaive)
plot(fcst)

