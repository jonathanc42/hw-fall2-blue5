library(readxl)
library(foreign)
library(dplyr)
library(zoo)
library(lubridate)
library(forecast)
library(stats)
library(ggplot2)
library(tseries)
library(haven)
library(data.table)
library(fma)
library(expsmooth)
library(lmtest)
library(caschrono)
library(TSA)
library(quantmod)

setwd('/Users/anniecooper/Documents/MSA_2019/Time Series/')

####------------------------Clean the full (wellft/rain/tide) data set----------------------####

################################ Aggregate Well Data Hourly ###################################

#Create Date Time Object
well <- read_xlsx('Well Data/G-2866_T.xlsx', 'Well')
well <- mutate(well, datetime=date(date))  # adds date to datetime
hour(well$datetime) <- hour(well$time) # Adds hour to datetime. Removes minutes from all hours
well$datetime <- as.POSIXct(well$datetime, tz='EST')  # change time type of newly created Datetime

#Aggregate Well Data by hour
well_agg <- well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))

################################ Aggregate Tide Data Hourly ###################################

#Create datetime column
tide <- fread('Well Data/station_8722859.csv')
tide <- mutate(tide, 
               Time = paste(Date, Time),
               datetime=date(Date))  # adds date to datetime
hour(tide$datetime) <- hour(tide$Time)  # Adds hour to datetime. Removes minutes from all hours
tide$datetime <- as.POSIXct(tide$datetime, tz='EST')  # change time type of newly created Datetime

#Aggregate hourly
tide_agg <- tide %>%
  group_by(datetime) %>%
  summarise(tide_ft = mean(Prediction))

################################ Aggregate Rain Data Hourly ###################################

#Create datetime column
rain <- read_xlsx('Well Data/G-2866_T.xlsx', 'Rain')
rain <- mutate(rain, datetime=date(Date))
hour(rain$datetime) <- hour(rain$Date) # adds date to datetime
rain$datetime <- as.POSIXct(rain$datetime, tz='EST')  # change time type of newly created Datetime

#Aggregate hourly
rain_agg <- rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))

############################### Merge all data ############################################

# create full sequence of dates
time_seq <- as.data.frame(seq(as.POSIXct("2007-10-01 01:00:00", tz='EST'), as.POSIXct("2018-06-12 23:00:00", tz='EST'), "hour"))
names(time_seq) <- c("datetime")
View(time_seq)
#Merge Left Outer join of time sequence on well data
df <- time_seq %>%
  left_join(well_agg, by='datetime') %>%
  left_join(rain_agg, by='datetime') %>%
  left_join(tide_agg, by='datetime')
View(df)
############################### Impute Missing Values ####################################

# print missing values by column
colSums(is.na(df))

# Only need to impute for well_ft and rain_ft
df <- df %>%
  mutate(well_ft = na.approx(well_ft, rule=2),
         tide_ft = na.approx(tide_ft, rule=2)) 

# Check for missing values
colSums(is.na(df))

# Only use data from 2014-2018
df2 <- df %>%
  mutate(well_ft = na.approx(well_ft, rule=2),
         tide_ft = na.approx(tide_ft, rule=2)) %>%
  filter(datetime >'2016-10-01 0:00:00' & datetime < '2018-06-08 09:00:00')

# Check for missing values in subsetted data
df_na <- df %>%
  filter(datetime > '2016-10-01 00:00:00' & datetime < '2018-06-08 11:00:00')
colSums(is.na(df_na))
View(df_na)
View(df)

############################### Export to csv or SAS ########################################

#write.csv(df2, 'combined_well.csv')
#write_sas(df2,'combined_well.sas7bdat')
#library(foreign)
#write.foreign(df2, "combined_well.txt", "combined_well.sas7bdat", package = "SAS")

####-----------------------------Prepare data, ts object, and training/test sets--------------------------------####

## Import sas file
#welldf<-read_sas("combined_well.sas7bdat")
#welldf<- read_sas("ts_hw2_2016.sas7bdat")
#resdf<- read_sas("residualdata.sas7bdat")
#hw1df<-read_sas("ts.sas7bdat")

## Rename the dataframe to use for time series
#welldf <- df         # full data set
welldf <- df2         # subsetted data set
View(welldf)

## Create Time Series Data Object
wellts <- ts(welldf,frequency =365.25*24)
plot(wellts)

## Training Data set
training=subset(wellts,end=length(welldf$well_ft)-24*7)
Wellft <- training[,2]
Wellft.v <- as.vector(training[,2])
Rainin <- training[,3]
Rainin.v <- as.vector(training[,3])
Tideft <- training[,4]
Tideft.v <- as.vector(training[,4])

## Test Data set
test=subset(wellts,start=length(welldf$well_ft)-(24*7-1))
Wellft.test <- test[,2]
Wellft.test.v <- as.vector(test[,2])
Rainin.test <- test[,3]
Rainin.test.v <- as.vector(test[,3])
Tideft.test <- test[,4]
Tideft.test.v <- as.vector(test[,4])

####--------------WELL FT ONLY: Check seasonality/autocorrelation of residuals and error-----------####

## Seasonal decomposition
decomp_wellft <- stl(Wellft, s.window=7)
plot(decomp_wellft)

## Sines & Cosines
#arima.s<-Arima(Wellft,xreg=fourier(Wellft,K=12))

## Test for Differences in residuals: number of differences required for a stationary series
#plot(arima.s$residuals)
#ndiffs(arima.s$residuals)
#training_diff<-diff(Wellft, lag=1)
#ndiffs(training_diff)
#plot(training_diff)

## Augmented Dickey-Fuller Test: test for trend
#adf.test(training_diff, alternative = "stationary", k = 0)
#adf.test(training_diff, alternative = "stationary", k = 1)
#adf.test(training_diff, alternative = "stationary", k = 2)

## Acf/Pacf plots
#Acf(training_diff, lag=30)$acf
#Pacf(training_diff, lag=30)$acf

####-----------------------------------WELL FT ONLY: Arima Model ------------------------------------####

## Original Arima model from hw1
#arima.final<-Arima(Wellft,order=c(4,1,24),xreg=fourier(Wellft,K=4),fixed=c(NA,NA,NA,NA,NA,0,0,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA,NA,NA,NA,NA,NA,NA),method="ML")      # q=(1,12,24)
#summary(arima.final)

## Final Arima model
#arima.final=Arima(Wellft,order=c(4,1,23), xreg=fourier(Wellft,K=12), fixed=c(NA,NA,NA,NA,NA,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), method="ML")    # q=(1,5,23) and fourier
arima.final=Arima(Wellft,order=c(4,1,5), xreg=fourier(Wellft,K=15), method="ML")    # fourier
#arima.final=Arima(Wellft,order=c(4,1,23), fixed=c(NA,NA,NA,NA,NA,0,0,0,0,NA,0,NA,NA,0,0,0,NA,0,0,0,0,0,0,0,0,0,NA) , method="ML")    # q=(1,6,9,13,23)

####-----------------------------------WELL FT/RAIN/TIDE: Arima Model ------------------------------------####

## Arima model with Rain and Tide
#x.reg=cbind(Rainin.v,Tideft.v)                   # try with Rain and Tide
#x.reg=cbind(Rainin.v, fourier(Wellft,K=4))       # try with Rain and fourier transform
x.reg=cbind(Rainin.v)                             # try with Rain only

## Check for differences
model1=Arima(Wellft,order=c(2,0,0),xreg=x.reg, method="ML")
ndiffs(model1$residuals)
plot(model1$residuals)

## Auto Arima with Rain and Tide
#auto.arima(model1$residuals,seasonal=T)

## Final Arima model
#arima.final=Arima(Wellft,order=c(4,1,5), xreg=cbind(x.reg, fourier(Wellft,K=15)), method="ML")    # fourier
arima.final=arima(Wellft,order=c(4,0,23),xreg=x.reg, fixed=c(NA,NA,NA,NA,NA,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA), method="ML")    # q=(1,5,23) with rain
#arima.final=arima(Wellft,order=c(4,1,23),xreg=x.reg,method="ML")    # q=(1,5,23) with rain

####-------------------------------------Diagnostic Plots-------------------------------------####

## Check stationarity
plot(arima.final$residuals)
ndiffs(model1$residuals)

## Updated Acf/Pacf plots
Acf(arima.final$residuals, lag=25)$acf
Pacf(arima.final$residuals, lag=25)$acf

## White Noise plot
White.LB <- rep(NA, 25)
for(i in 1:10){
  White.LB[i] <- Box.test(arima.final$residuals, lag = i, type = "Ljung", fitdf = 2)$p.value
}
White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.05, lty = "dashed", col = "black")

####-----------------------------Predictions on the Holdout Set----------------------------####

## If using Rain & Tide, must use individual ARIMAs to forecast x's
#model.Rainin=Arima(Rainin,order=c(4,0,23),fixed=c(NA,NA,NA,NA,NA,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,NA), method="ML")     # q=(1,5,23)
#forecast.Rainin=forecast(model.Rainin,h=24*7)
#R.f=forecast.Rainin$mean
#newx=cbind(R.f)
#model.Tideft=Arima(Tideft.test,order=c(1,0,0))
#forecast.Tideft=forecast(model.Tideft,h=24*7)
#T.f=forecast.Tideft$mean
#newx=cbind(R.f,T.f)

## Forecast for Wellft on holdout data set
#forecast.final=forecast(arima.final,xreg=newx,h=24*7)                   # Uses forecasted rain data
forecast.final=forecast(arima.final,xreg=Rainin.test.v,h=24*7)          # Uses actual rain data
#forecast.final=forecast(arima.final,xreg=fourier(Wellft,K=15,h=24*7),h=24*7)     # Fourier
forecastdf<-data.frame(tail(welldf$datetime,n=24*7),round(forecast.final$mean,4),Wellft.test)
names(forecastdf)[1]<-'date'
names(forecastdf)[2]<-'forecast'
names(forecastdf)[3]<-'actual'
#View(forecastdf)

## Create plot of actual vs predicted
ggplot(forecastdf, aes(forecastdf$date)) +
  geom_line(aes(y=forecastdf$actual), colour='blue') +
  geom_line(aes(y=forecastdf$forecast), colour='orange') + xlab('Time')+ylab('Well Ft') +
  ggtitle('Actual vs Predicted Well Ft for 2018')

## Model Diagnostic Statistics
error=forecastdf$actual-forecastdf$forecast
MAE=mean(abs(error))
MAE
MAPE=mean(abs(error)/abs(forecastdf$actual))
MAPE
