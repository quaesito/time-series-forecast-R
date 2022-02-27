#Analysis of Fish sales dataset
#Import important library for time series analysis in R
library(tidyverse)
library(fpp)
library(forecast)
library(backtest)
library(quantmod)
library(lubridate)
library(dplyr)


#import dataset
Sale_of_fish <- read.csv("Fish dataset.csv",header=T) #data source=FranceAgriMer/VISIOMer

head(Sale_of_fish) #See head of the dataset
tail(Sale_of_fish) #see tail of the dataset
#work with the number of sales from dataset 
adj <- select(Sale_of_fish, -c("year", "month", "end.of.period", "value..E.", "unsoldes..Kg."))
adj$sales <- as.numeric(gsub(' ', '', adj$sales))
adj$start <- dmy(adj$start)##This tells you that the data series is in a time series format

adj$logr <- log(lag(adj$sales)) - log(adj$sales)
head(adj)
narm <- function (x) {
  x[is.na(x)] <- 1
  return(x)
}

adj$logr <- narm(adj$logr)
head(adj)

max_Date <- max(adj$start)
min_Date <- min(adj$start)
test_ts <- ts(adj$logr, end=c(year(max_Date), month(max_Date)),start=c(year(min_Date), month(min_Date)),frequency=12)#freq 12 => Monthly data

logr <- adj
logr
dev.off() #to face margin problem error
plot(test_ts)
plot(stl(test_ts,s.window = "periodic"))

# both acf() and pacf() generates plots by default
acf = acf(test_ts, main='ACF Plot', lag.max=100) # autocorrelation
pacf.logr = pacf(test_ts, main='PACF Plot', lag.max=100) # partial autocorrelation

#Augmented Dickey-Fuller(ADF) Test
print(adf.test(test_ts)) # p-value < 0.05 indicates the TS is stationary

#Estimate the coefficients Using Seasonal Arima model
m1 <- auto.arima(test_ts, seasonal = TRUE)
summary(m1)
#Check Accuracy
accuracy(forecast(m1))

#studying the residues
checkresiduals(m1) # p-value over .05 confirms no autocorrelations


#### Analysis for Yahoo finance data
#Import important library
library(quantmod)
library(fpp)
library(backtest)
#read yahoo finance datasets for last one year CA.PA 
data <- getSymbols("CA.PA", from="2019-11-25",to = "2020-11-24" ,src="yahoo", auto.assign=F)
head(data)#see the head of dataset
tail(data) #see the tail of dataset

adj <- data[,5]
adj

dev.off() #to face margin problem error
plot(data[,6], main = "Adhusted Closing Price")
log <- periodReturn(adj,period = "daily", type = "log", leading = TRUE)
log <- 1+log
head(log)

#differentiate
logr <- diff(log) 

narf <- function (x) {
  x[is.infinite(x)] <- 1
  return(x)
}

adj$logr <- narm(narf(adj$logr))
test_ts <- ts(adj$logr, end=c(year(max_Date), month(max_Date)),start=c(year(min_Date), month(min_Date)),frequency=12)#freq 12 => Monthly data

logr <- adj
head(logr)

# both acf() and pacf() generates plots by default
acf = acf(test_ts, main='ACF Plot', lag.max=100) # autocorrelation
pacf.logr = pacf(test_ts, main='PACF Plot', lag.max=100) # partial autocorrelation

#Augmented Dickey-Fuller(ADF) Test
print(adf.test(test_ts)) # p-value < 0.05 indicates the TS is stationary

#Estimate the coefficients Using Seasonal Arima model
m1 <- auto.arima(logr, seasonal = TRUE)
summary(m1)

#studying the residues
checkresiduals(m1) # p-value over .05 confirms no autocorrelations
