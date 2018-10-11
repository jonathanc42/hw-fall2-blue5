library(readxl)
library(lubridate)
library(zoo)
library(dplyr)
library(data.table)


setwd('/Users/mwitebsky/Documents/NCSU/AA502/Time Series/Well Project/Assets/')

################################ Aggregate Well Data Hourly ###################################

#Create Date Time Object
well <- read_xlsx('G-2866_T.xlsx', 'Well')
well <- mutate(well, datetime=date(date))  # adds date to datetime
hour(well$datetime) <- hour(well$time) # Adds hour to datetime. Removes minutes from all hours
well$datetime <- as.POSIXct(well$datetime)  # change time type of newly created Datetime

#Aggregate Well Data by hour
well_agg <- well %>%    # summarizes to hourly data and  
              group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
              summarise(well_ft=mean(Corrected))



################################ Aggregate Tide Data Hourly ###################################

#Create datetime column
tide <- fread('station_8722859.csv')
tide <- mutate(tide, 
               Time = paste(Date, Time),
               datetime=date(Date))  # adds date to datetime
hour(tide$datetime) <- hour(tide$Time)  # Adds hour to datetime. Removes minutes from all hours
tide$datetime <- as.POSIXct(tide$datetime)  # change time type of newly created Datetime

#Aggregate hourly
tide_agg <- tide %>%
              group_by(datetime) %>%
              summarise(tide_ft = mean(Prediction))




################################ Aggregate Rain Data Hourly ###################################

#Create datetime column
rain <- read_xlsx('G-2866_T.xlsx', 'Rain')
rain <- mutate(rain, datetime=date(Date))
hour(rain$datetime) <- hour(rain$Date) # adds date to datetime
rain$datetime <- as.POSIXct(rain$datetime)  # change time type of newly created Datetime

#Aggregate hourly
rain_agg <- rain %>%
              group_by(datetime) %>%
              summarise(rain_in = sum(RAIN_FT*12))



############################### Merge all data ############################################

# create full sequence of dates
time_seq <- as.data.frame(seq(as.POSIXct("2007-10-01 01:00:00"), as.POSIXct("2018-06-12 23:00:00"), "hour"))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df <- time_seq %>%
        left_join(well_agg, by='datetime') %>%
        left_join(rain_agg, by='datetime') %>%
        left_join(tide_agg, by='datetime')



############################### Impute Missing Values ####################################

# print missing values by column
colSums(is.na(df))

# Only need to impute for well_ft and rain_ft
df <- df %>%
        mutate(well_ft = na.approx(well_ft, rule=2),
               tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df))



############################### Export to csv or SAS ########################################
fwrite(df, '../Outputs/combined_well.csv')

##############################Create text files to import into SAS###########################

library(foreign)


write.foreign(df, "C:/Users/senor/Documents/Time_Series/timeseries2_hw2.txt", "C:/Users/senor/Documents/Time_Series/timeseries2_hw2.sas", package = "SAS")
