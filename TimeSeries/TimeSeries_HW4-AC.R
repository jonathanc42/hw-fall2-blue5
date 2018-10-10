library(readxl)
library(dplyr)
library(zoo)
library(lubridate)
library(forecast)
library(stats)
library(ggplot2)
library(tseries)

## Import excel file
welldf <- read_excel("/Users/anniecooper/Documents/MSA_2019/Time Series/G-2866_T.xlsx", sheet = "Well")
#View(welldf)

##--------------------Correct date formats and aggregate data to hourly -------------------------##

# Create corrected 'date/time' and 'date/time/hour' columns
welldf <- welldf %>%
  mutate(correctedtime=paste(ymd(date),hour(time))) %>%
  mutate(correctedtime=ymd_h(correctedtime))

# Group by Hour, calculate mean and sd
welldf_hour <- welldf %>%
  group_by(correctedtime) %>%
  summarise(wellft=mean(Corrected)) %>%
  mutate(wellft)
#View(welldf_hour)

##--------------------Confirm there are no null months in the data set ----------------------------##

# Create sequence of date/time/hours from 10/01/2007 01:00:00 - 6/12/2018 23:00:00
hoursequence <-seq(as.POSIXct('2007/10/01 01:00:00'), as.POSIXct('2018/06/12 23:00:00'),"hour")
df1 <- data.frame(
  correctedtime=hoursequence,
  stringsAsFactors = FALSE
)
#View(df1)

# Join welldf_hour and df1
welldf_join <- df1 %>%
  left_join(welldf_hour, by="correctedtime")
summary(welldf_join)
#View(welldf_join)

##-----------------------------------------Create Time Series Data Object-------------------------------------##
##-----------------------------------Check for seasonality/stationarity/trend-----------------------------------##

## Create Time Series Data Object
Wellft <- ts(na.approx(welldf_join$wellft), start = 2007, frequency =8766)
plot(Wellft_train)

#Using a holdout data set for last week of data
training=subset(Wellft,end=length(Wellft)-24*7)
test=subset(Wellft,start=length(Wellft)-(24*7-1))

## Seasonal decomposition
decomp_wellft <- stl(training, s.window=7)
plot(decomp_wellft)

## Sines & Cosines
arima.s<-Arima(training,order=c(0,0,0),xreg=fourier(training,K=4))
## Test for Differences in residuals: number of differences requried for a stationary series
ndiffs(arima.s$residuals)
training_diff<-diff(arima.s$x, lag=1)
ndiffs(training_diff)

## Test for autocorrelation
Acf(training_diff, lag=10)$acf
Pacf(training_diff, lag=10)$acf

## Arima Model: 
arima.final<-Arima(arima.s,order=c(4,1,24),fixed=c(NA,NA,NA,NA,NA,0,0,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA),xreg=1:length(training))
?Arima
arima.sd<-Arima(arima.s$residuals, order = c(0, 1, 0))
summary(Wellft_arima)
plot(arima.final$residuals)
Acf(arima.final$residuals, main = "")$acf
Pacf(arima.final$residuals, main = "")$acf

## Check White Noise
White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(arima.final$residuals, lag = i, type = "Ljung", fitdf = 2)$p.value
}
White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")


##-----------------------------Predictions on the Holdout Set----------------------------##

## Using a holdout data set
test.arima.s=Arima(test,order=c(0,0,0),xreg=fourier(test, K=1, h=24*7))
test.arima.final<-Arima(test.arima.s$x,order=c(4,1,24),fixed=c(NA,NA,NA,NA,NA,0,0,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA),xreg=1:length(test))
test.results=forecast(test.arima.final,xreg=fourier(test,K=1,h=24*7))

## Model Diagnostic Statistics
error=test-test.arima.final$mean
MAE=mean(abs(error))
MAE
MAPE=mean(abs(error)/abs(test))
MAPE
SMAPE=mean(abs(error)/(abs(test)+abs(test.arima.final$mean)))
SMAPE

## Create data set with last week predicted vs. actual well ft
actual_1 <- tail(welldf_join,n=24*7)
#View(actual_1)
actual_1$pred = test.results$mean
actual_1$date <-as.Date(actual_1$correctedtime)

## Create plot
ggplot(actual_1, aes(actual_1$date)) +
  geom_line(aes(y=actual_1$wellft), colour='blue') +
  geom_line(aes(y=actual_1$pred), colour='orange') + xlab('Time')+ylab('Well Ft') +
  ggtitle('Actual vs Predicted Well Ft for 2018')

