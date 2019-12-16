install.packages('forecast')
library(forecast)
library('ggplot2')
library('forecast')
library('tseries')
library(readxl)

#ts plot of the gas dataset
plot(gas)

View(gas)

boxplot(gas)
str(gas)
summary(gas)

seasonplot(gas)
tsdisplay(gas)
plot(subset(gas,month="November"))
#gas_data=read_excel("data.xlsx")

monthplot(gas)
#The frequency is given in cycle
cycle(gas)
boxplot(gas~cycle(gas))

#Decompose the data
#decomp = stl(gas, s.window = "periodic")
#deseasonal_gas=seasadj(decomp)
#plot(deseasonal_gas) 
#plot(decomp)


decomp_gas=decompose(gas,type="multiplicative")
plot(decomp_gas)
decomp_gas
deseasonal_gas=seasadj(decomp_gas,s.window="p")

#decomp$time.series
#series_names <- c('Deseasoned', 'Actual')
#Deseason_gas <- (decomp$time.series[,2]+decomp$time.series[,3]) 
ts.plot(gas, deseasonal_gas, col=c("red", "blue"), 
        main="ItemA Demand vs De-seasoned Demand")

#Check for stationarity using the Augmented Dickey-Fuller test

adf.test(deseasonal_gas, alternative = "stationary")

#0.5826 for 95% confidence level. < 0.05

#Series is not stationary

#Check autocorrelation plots
#ACF and PACF plots
acf(gas)
pacf(gas)
acf(gas, lag.max = 50)
pacf(gas, lag.max = 50)

#There are significant autocorrelations with many lags in our demand series, as shown by 
#the ACF plot. 

#PACF plot shows that there could be monthly seasonality since the plot peaks at 
#at various intervals

#Differencing the time series data
#deseasonal_gas=seasadj(gas)
count_d1 = diff(deseasonal_gas, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

#Differenced demand is stationary

#acf and pacf for dif time series. ARIMA(p,d,q)
Acf(count_d1, main='ACF for Differenced Series',lag=50) #--- q
Pacf(count_d1, main='PACF for Differenced Series',lag.max = 50) #--- p

#From the ACF plot, there is a cut off after lag 0. This implies that q=0. 
#PACF cuts off after lag 10. Hence p=4.

#p=4,d=1,q=0

#Splitting into training and test sets
gas_train <- window(deseasonal_gas, start=c(1970,1), end=c(1990,12), frequency=12) 
gas_test <- window(deseasonal_gas, start=c(1991,1), frequency=12)

#Fitting an ARIMA model

gas_ARIMA = arima(gas_train, order=c(4,1,0))
gas_ARIMA
tsdisplay(residuals(gas_ARIMA), main='Model Residuals')

ArimaForecast <- forecast(gas_ARIMA, h=56) 
plot(ArimaForecast) 
VecA01 <- cbind(gas_test,ArimaForecast)
par(mfrow=c(1,1), mar=c(2, 2, 2, 2), mgp=c(3, 1, 0), las=0)
ts.plot(VecA01[,1],VecA01[,2], col=c("blue","red"),xlab="year", ylab="gas", main="gas: Actual vs Forecast")

Box.test(ArimaForecast$residuals)
#p 0.001959 -

#  MAPE
MAPE(VecA01[,1],VecA01[,2])

#There are no significant autocorrelations present. If the model is not correctly 
#specified, that will usually be reflected in residuals in the form of trends, 
#skeweness, or any other patterns not captured by the model. Ideally, residuals 
#should look like white noise, meaning they are normally distributed.  
#Residuals plots show a smaller error range, more or less centered around 0.

#Fitting with Auto ARIMA
fit<-auto.arima(gas_train, seasonal=FALSE)
fit
tsdisplay(residuals(fit), lag.max=45, main='Auto ARIMA Model Residuals')

#Auto ARIMA also fits the same p and q parameters for the model, but has a slightly 
#lower AIC.

#Residual analysis using Ljung-Box test
#H0: Residuals are independent
#Ha: Residuals are not independent
library(stats)

#Validate both the manual and automatically fitted ARIMA models
fcast <- forecast(gas_ARIMA, h=56)
fcast1 <- forecast(fit, h=56)
plot(fcast)
plot(fcast1)

##box ljung test for auto arima
VecA03 <- cbind(gas_test,fcast1)
par(mfrow=c(1,1), mar=c(2, 2, 2, 2), mgp=c(3, 1, 0), las=0)
ts.plot(VecA03[,1],VecA03[,2], col=c("blue","red"),xlab="year", ylab="gas", main="gas: Actual vs Forecast")

Box.test(fcast1$residuals, lag=20, type="Ljung-Box")
#p 0.003425 - residuals are independent-good

MAPE(VecA03[,1],VecA03[,2])

#Forecast into the future
fit1<-auto.arima(gas, seasonal=FALSE)
fcast2=forecast(fit1, h=12)
plot(fcast2)

fit2<-arima(gas,order=c(4,1,0))
fcast3=forecast(fit2, h=12)
plot(fcast3)



## alternative method#####
##decomposition method

gas.stl <- stl(gas_train, s.window="p") 
library(forecast)
## Warning: package 'forecast' was built under R version 3.4.2
 
fcst.gas.stl <- forecast(gas.stl, method='rwdrift',h=56)
plot(fcst.gas.stl)

#comparing testing data with mean of forecasted data
#(similar to confusion matrix)
VecA<- cbind(gas_test,fcst.gas.stl$mean) 
VecA 

par(mfrow=c(1,1), mar=c(2, 2, 2, 2), mgp=c(3, 1, 0), las=0)
ts.plot(VecA, col=c("blue", "red"),xlab="year", ylab="gas", main="Quarterl y gas: Actual vs Forecast")

#Box-Ljung test:
#H0: Residuals are Independent-good
#Ha: Residuals are not Independent-bad

Box.test(fcst.gas.stl$residuals, lag=30, type="Ljung-Box")
#residuals are not indepedent-lack of fit

##since p<0.05 hence we reject null hypothesis; therefore model is not good fit
MAPEA <- mean(abs(VecA[,1]-VecA[,2])/VecA[,1]) 
MAPEA

## [1] 0.09396317
hwA <- HoltWinters(as.ts(gas_train),seasonal="multiplicative") 
hwA
plot(hwA)
hwAForecast <- forecast(hwA, h=56) 
plot(hwAForecast)
VecA1 <- cbind(gas_test,hwAForecast)
par(mfrow=c(1,1), mar=c(2, 2, 2, 2), mgp=c(3, 1, 0), las=0)
ts.plot(VecA1[,1],VecA1[,2], col=c("blue","red"),xlab="year", ylab="gas", main="gas: Actual vs Forecast")

Box.test(hwAForecast$residuals)
#p 0.1505 - residuals are independent-good

install.packages('MLmetrics')
library(MLmetrics)
accuracy(hwAForecast,gas_test)
# another way to calc. MAPE
MAPE(VecA1[,1],VecA1[,2])
#0.03830469

#arima model
library(readxl)
gas_total=read_excel("C:/Users/Chetan Suvarna/Desktop/project/Time Series-p6/data.xlsx")
gas_t1 <- ts(gas_total[,3], start=c(1970,1), frequency=12)
View(gas_t1)
plot(gas_t1)

library(tseries) 

adf.test(gas_t1)
#Series is stationary

diff_gas <- diff(gas_t1)
plot(diff_gas)

adf.test(diff(gas))

acf(gas_t1)
acf(gas_t1,lag=15)
acf(diff_gas, lag=15)
#q=2
#Checking with Lag 50
acf(gas,lag=50)
acf(diff_gas, lag=50)
#q=2
pacf(gas)
pacf(diff_gas)
#p=10

#pdq=10,1,2

#SARIMA
gas_train1 <- window(gas, start=c(1970,1), end=c(1990,12), frequency=12) 
gas_test1 <- window(gas, start=c(1991,1), frequency=12)

gas.arima.fit.train <- auto.arima(gas_train1, seasonal=TRUE)
gas.arima.fit.train
plot(gas.arima.fit.train$residuals)

plot(gas.arima.fit.train$x,col="blue") 
lines(gas.arima.fit.train$fitted,col="red",main="Demand A: Actual vs Forecast ")

MAPE(gas.arima.fit.train$fitted,gas.arima.fit.train$x)
#0.03961592

acf(gas.arima.fit.train$residuals)
pacf(gas.arima.fit.train$residuals)

Box.test(gas.arima.fit.train$residuals)
#0.0117 -  not good fit

#Forecasting on Hold-out(test) data:

Sarimafcast <- forecast(gas.arima.fit.train, h=56) 
VecA2 <- cbind(gas_test,Sarimafcast)
par(mfrow=c(1,1), mar=c(2, 2, 2, 2), mgp=c(3, 1, 0), las=0)
ts.plot(VecA2[,1],VecA2[,2], col=c("blue","red"),xlab="year", ylab="demand", main="Demand A: Actual vs Forecast")
accuracy(Sarimafcast,gas_test1)

#auto.arima
decomp_gas_t1=decompose(gas_t1,type="multiplicative")
plot(decomp_gas_t1)
decomp_gas_t1
deseasonal_gas_t1=seasadj(decomp_gas_t1,s.window="p")

#decomp$time.series
#series_names <- c('Deseasoned', 'Actual')
#Deseason_gas <- (decomp$time.series[,2]+decomp$time.series[,3]) 
ts.plot(gas_t1, deseasonal_gas_t1, col=c("red", "blue"), 
        main="ItemA Demand vs De-seasoned Demand")

#Check for stationarity using the Augmented Dickey-Fuller test

adf.test(deseasonal_gas_t1, alternative = "stationary")


gas.autoarima.fit.train <- auto.arima(gas_train, seasonal=FALSE)
gas.autoarima.fit.train

plot(gas.autoarima.fit.train$residuals)

plot(gas.autoarima.fit.train$x,col="blue") 
lines(gas.autoarima.fit.train$fitted,col="red",main="Demand A: Actual vs Forecast ")

MAPE(gas.autoarima.fit.train$fitted,gas.autoarima.fit.train$x)
#0.06777542

acf(gas.autoarima.fit.train$residuals)
pacf(gas.autoarima.fit.train$residuals)

Box.test(gas.autoarima.fit.train$residuals, lag = 30, type = c("Ljung-Box"), fitdf = 0)
#< 2.2e-16 -  not good fit

#Forecasting on Hold-out(test) data:

arimafcast <- forecast(gas.autoarima.fit.train, h=56) 
VecA2 <- cbind(gas_test,arimafcast)
par(mfrow=c(1,1), mar=c(2, 2, 2, 2), mgp=c(3, 1, 0), las=0)
ts.plot(VecA2[,1],VecA2[,2], col=c("blue","red"),xlab="year", ylab="demand", main="Demand A: Actual vs Forecast")

