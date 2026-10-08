
## create a data frame with a time varible
name.col <- c("Y", "time")
n <- 100

df <- matrix(0, nrow=n, ncol= length(name.col))
colnames(df) <- name.col
df <- as.data.frame(df)

## Generate the Y value
df$Y <- rnorm(n)

## by month
df$time <- seq(as.Date("2000/1/1"), by = "month", length.out = n)

## plot the data
plot(Y~time, data=df, type="l")

## convert the data frame into time series data format
y.ts <- ts(df$Y, start=c(2000, 1), frequency=12)
y.ts

## plot the time series data using plot.ts() function
plot.ts(y.ts , type="l", col="blue", lty="dashed", ylab="y")

## decompose the time series 
y.components <- decompose(y.ts)

## plot the all components
plot(y.components , type="l", col="blue", lty="dashed")

## plot trend
plot.ts(y.components$trend , type="l", col="blue", lty="dashed", ylab="trend")

## plot the seasonal component
plot.ts(y.components$seasonal , type="l", col="blue", lty="dashed", ylab="seasonal")

## plot the residual component
plot.ts(y.components$random, type="l", col="blue", lty="dashed", ylab="residual")




## package for forecasting
library(forecast)
fit <- auto.arima(y.components$random)
predict(fit, n.ahead=10, se.fit=T)
## to vidualize the predicted values
y.forecast <- forecast(object=fit, h=10) 
plot(y.forecast)




################################
## Real data analysis
################################
## read the data
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
births.ts <- ts(births, start=c(1946,1), frequency=12)

## plot the time series data
plot.ts(births.ts , type="l", col="blue", lty="dashed", ylab="number of births")


## decompose the time series 
births.components <- decompose(births.ts)

## plot the all components
plot(births.components , type="l", col="blue", lty="dashed")

## plot trend
plot.ts(births.components$trend , type="l", col="blue", lty="dashed", ylab="trend")

## plot the seasonal component
plot.ts(births.components$seasonal , type="l", col="blue", lty="dashed", ylab="seasonal")

## plot the residual component
plot.ts(births.components$random, type="l", col="blue", lty="dashed", ylab="residual")

acf(na.omit(births.components$random))
pacf(na.omit(births.components$random))


## package for forecasting through ARIMA model
library(forecast)
fit <- auto.arima(y.components$random)
predict(fit, n.ahead=10, se.fit=T)
## to vidualize the predicted values
y.forecast <- forecast(object=fit, h=10) 
plot(y.forecast)


