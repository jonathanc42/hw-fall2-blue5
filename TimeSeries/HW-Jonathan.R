# Jonathan R homework

#### Start ####
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(ggplot2)
library(lubridate)
library(dplyr)
library(openxlsx)

#Read in well water depth data
well <- read.xlsx("TimeSeries/Well Data/G-2866_T.xlsx",sheet=3)

#Create Date Time Object

well$time <- convertToDateTime(well$time)
well$date <- convertToDate(well$date)
well$datetime <- as.POSIXct(paste(format(well$date,"%Y-%m-%d"),
                                  format(well$time,"%H:%M")),
                            format="%Y-%m-%d %H:%M")

#### Aggregate Well Data by hour ####

well_agg <- aggregate(list(Corrected = well$Corrected), 
                      list(datetime = cut(well$datetime, "1 hour")), 
                      mean)
well_agg$datetime <- as.POSIXct(well_agg$datetime)

time_seq <- seq(as.POSIXct("2007-10-01 01:00:00"), as.POSIXct("2018-06-12 23:00:00"), "hour")
time_seq <- as.data.frame(time_seq)
names(time_seq) <- c("datetime")
str(time_seq)

#Merge Left Outer join of time sequence on well data
well_merge <- merge(time_seq, well_agg, by = "datetime", all.x=TRUE)

#Find number of hourly entries with missing well data
sum(is.na(well_merge$Corrected))

#Find which hourly entries are missing
well_merge %>% filter(is.na(well_merge$Corrected))

#### Create time series object ####
Well = ts(well_merge$Corrected, frequency=24*365.25)
Well = tsclean(Well)

#Using a holdout data set for last week of data
training=subset(Well,end=length(Well)-24*7)
test=subset(Well,start=length(Well)-(24*7-1))

#### Time Series Decomposition ...STL ####
decomp_stl <- stl(training, s.window = 7)
plot(decomp_stl)

#Well Data with Seasonal Component removed 
deseasonal_well = seasadj(decomp_stl)
plot(deseasonal_well)

plot(Well, col = "grey", main = "Well Water Depth - Trend/Cycle", xlab = "", ylab = "Water Depth (m)", lwd = 2)
lines(decomp_stl$time.series[,2], col = "red", lwd = 2)

seas_well=Well-decomp_stl$time.series[,1]
plot(Well, col = "grey", main = "Well Water Depth - Seasonally Adjusted", xlab = "", ylab = "Water Depth (m)", lwd = 2)
lines(seas_well, col = "red", lwd = 2)

#### Augmented Dickey-Fuller Testing ####
adf.test(training, k=0, alternative = "stationary")

count_well = diff(deseasonal_well, differences = 1)
plot(count_well)
adf.test(count_well, alternative = "stationary", k=0)

plot(count_well)

Acf(count_well, main='ACF for Differenced Series', lag = 10)
Pacf(count_well, main='PACF for Differenced Series', lag = 10)

fit<-auto.arima(deseasonal_well, seasonal=FALSE)
summary(fit)
tsdisplay(residuals(fit), lag.max=10, main='(2,1,2) Model Residuals')

#### Fit Using Seasonal Model Methods ####

arima_seas<-Arima(training,order=c(2,1,2),xreg=fourier(training,K=1)) 
summary(arima_seas)

fcast <- forecast(arima_seas,xreg=fourier(training, K=1, h=24*7))
error=test-fcast$mean
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(test))

#### Plot Forecase vs Actual ####
plot(fcast$mean, ylim=c(7.5, 10.5), xlab="Hourly Data Over a Week", ylab="Water Depth", xaxt="n")
lines(test, col="red")