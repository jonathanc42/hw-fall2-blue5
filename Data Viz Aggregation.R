
library(readxl)
library(lubridate)
library(zoo)
library(dplyr)
library(data.table)

setwd('C:\\Users\\aalmq\\OneDrive\\Documents\\Data Visualization\\Well_Data\\Well Data')

################################# Well G-2866 ###################################

#Create Date Time Object
G_2866_well <- read_xlsx('G-2866_T.xlsx', 'Well')
G_2866_well <- mutate(G_2866_well, datetime=date(date))  # adds date to datetime
hour(G_2866_well$datetime) <- hour(G_2866_well$time) # Adds hour to datetime. Removes minutes from all hours
G_2866_well$datetime <- as.POSIXct(G_2866_well$datetime)  # change time type of newly created Datetime

#Aggregate Well Data by hour
G_2866_well_agg <- G_2866_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data Hourly ##

#Create datetime column
# G_2866_tide <- fread('station_8722859.csv')
# G_2866_tide <- mutate(G_2866_tide, 
#                Time = paste(Date, Time),
#                datetime=date(Date))  # adds date to datetime
# hour(G_2866_tide$datetime) <- hour(G_2866_tide$Time)  # Adds hour to datetime. Removes minutes from all hours
# G_2866_tide$datetime <- as.POSIXct(G_2866_tide$datetime)  # change time type of newly created Datetime
# 
# #Aggregate hourly
# G_2866_tide_agg <- G_2866_tide %>%
#   group_by(datetime) %>%
#   summarise(tide_ft = mean(Prediction))




## Aggregate Rain Data Hourly ##

#Create datetime column
G_2866_rain <- read_xlsx('G-2866_T.xlsx', 'Rain')
G_2866_rain <- mutate(G_2866_rain, datetime=date(Date))
hour(G_2866_rain$datetime) <- hour(G_2866_rain$Date) # adds date to datetime
G_2866_rain$datetime <- as.POSIXct(G_2866_rain$datetime)  # change time type of newly created Datetime

#Aggregate hourly
G_2866_rain_agg <- G_2866_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(seq(as.POSIXct("2007-10-01 01:00:00"), as.POSIXct("2018-06-12 23:00:00"), "hour"))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_2866 <- time_seq %>%
  left_join(G_2866_well_agg, by='datetime') %>%
  left_join(G_2866_rain_agg, by='datetime')
  #left_join(G_2866_tide_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_G_2866))

# Only need to impute for well_ft and rain_ft
df_G_2866 <- df_G_2866 %>%
  mutate(well_ft = na.approx(well_ft, rule=2))
         #tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_G_2866))

df_G_2866$Well = "G-2866"

########################################## Well F-45 ######################################

## Aggregate Well Data Hourly ##



#Create Date Time Object for Well F-45
F_45_well <- read_xlsx('F-45.xlsx', 'Well')
F_45_well <- mutate(F_45, datetime=date(date))  # adds date to datetime
hour(F_45_well$datetime) <- hour(F_45_well$time) # Adds hour to datetime. Removes minutes from all hours
F_45_well$datetime <- as.POSIXct(F_45_well$datetime)  # change time type of newly created Datetime

#Aggregate Well Data by hour
F_45_well_agg <- F_45_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data Hourly ##

#Create datetime column
# F_45_tide <- read_xlsx('F-45.xlsx', 'Tide')
# F_45_tide <- mutate(F_45_tide, datetime=date(Date))  # adds date to datetime
# hour(F_45_tide$datetime) <- hour(F_45_tide$Time) # Adds hour to datetime. Removes minutes from all hours
# F_45_tide$datetime <- as.POSIXct(F_45_tide$datetime)


#Aggregate hourly
# F_45_tide_agg <- tide %>%
#   group_by(datetime) %>%
#   summarise(tide_ft = mean(Tide_ft))




## Aggregate Rain Data Hourly ##

#Create datetime column
F_45_rain <- read_xlsx('F-45.xlsx', 'Rain')
F_45_rain <- mutate(F_45_rain, datetime=date(Date))
hour(F_45_rain$datetime) <- hour(F_45_rain$Date) # adds date to datetime
F_45_rain$datetime <- as.POSIXct(F_45_rain$datetime)  # change time type of newly created Datetime

#Aggregate hourly
F_45_rain_agg <- F_45_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(seq(as.POSIXct("2007-10-01 01:00:00"), as.POSIXct("2018-06-12 23:00:00"), "hour"))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_F_45 <- time_seq %>%
  left_join(F_45_well_agg, by='datetime') %>%
  #left_join(F_45_tide_agg, by='datetime') %>%
  left_join(F_45_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_F_45))

# Only need to impute for well_ft
df_F_45 <- df_F_45 %>%
  mutate(well_ft = na.approx(well_ft, rule=2))

colSums(is.na(df_F_45))

df_F_45$Well = "F-45"
################################# Well F-179 ###################################

F_179_well <- read_xlsx('F-179.xlsx', 'Well')
F_179_well <- mutate(F_179_well, datetime=date(date))  # adds date to datetime
hour(F_179_well$datetime) <- hour(F_179_well$time) # Adds hour to datetime. Removes minutes from all hours
F_179_well$datetime <- as.POSIXct(F_179_well$datetime)  # change time type of newly created Datetime

#Aggregate Well Data by hour
F_179_well_agg <- F_179_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data Hourly ##

#Create datetime column
# F_179_tide <- read_xlsx('F-179.xlsx', 'Tide')
# F_179_tide <- mutate(F_179_tide, datetime=date(Date))  # adds date to datetime
# hour(tide$datetime) <- hour(F_179_tide$Time) # Adds hour to datetime. Removes minutes from all hours
# F_179_tide$datetime <- as.POSIXct(F_179_tide$datetime)  # change time type of newly created Datetime
# 
# #Aggregate hourly
# F_179_tide_agg <- F_179_tide %>%
#   group_by(datetime) %>%
#   summarise(tide_ft = mean(Tide_ft))




## Aggregate Rain Data Hourly ##

#Create datetime column
F_179_rain <- read_xlsx('F-179.xlsx', 'Rain')
F_179_rain <- mutate(F_179_rain, datetime=date(Date))
hour(F_179_rain$datetime) <- hour(F_179_rain$Date) # adds date to datetime
F_179_rain$datetime <- as.POSIXct(F_179_rain$datetime)  # change time type of newly created Datetime

#Aggregate hourly
F_179_rain_agg <- F_179_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(seq(as.POSIXct("2007-10-01 01:00:00"), as.POSIXct("2018-06-12 23:00:00"), "hour"))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_F_179 <- time_seq %>%
  left_join(F_179_well_agg, by='datetime') %>%
  #left_join(F_179_tide_agg, by='datetime') %>%
  left_join(F_179_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_F_179))

# Only need to impute for well_ft and tide_ft
df_F_179 <- df_F_179 %>%
  mutate(well_ft = na.approx(well_ft, rule=2))
         #tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_F_179))

df_F_179$Well = "F-179"

################################# Well F-319 ###################################

F_319_well <- read_xlsx('F-319.xlsx', 'Well')
F_319_well <- mutate(F_319_well, datetime=date(date))  # adds date to datetime
hour(F_319_well$datetime) <- hour(F_319_well$time) # Adds hour to datetime. Removes minutes from all hours
F_319_well$datetime <- as.POSIXct(F_319_well$datetime)  # change time type of newly created Datetime

#Aggregate Well Data by hour
F_319_well_agg <- F_319_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data Hourly ##

#Create datetime column
# F_319_tide <- read_xlsx('F-319.xlsx', 'Tide')
# F_319_tide <- mutate(F_139_tide, datetime=date(Date))  # adds date to datetime
# hour(F_139_tide$datetime) <- hour(F_139_tide$Time) # Adds hour to datetime. Removes minutes from all hours
# F_139_tide$datetime <- as.POSIXct(F_139_tide$datetime)  # change time type of newly created Datetime

#Aggregate hourly
# F_319_tide_agg <- F_139_tide %>%
#   group_by(datetime) %>%
#   summarise(tide_ft = mean(Tide_ft))




## Aggregate Rain Data Hourly ##

#Create datetime column
F_319_rain <- read_xlsx('F-319.xlsx', 'Rain')
F_319_rain <- mutate(F_319_rain, datetime=date(Date))
hour(rain$datetime) <- hour(F_319_rain$Date) # adds date to datetime
F_319_rain$datetime <- as.POSIXct(F_319_rain$datetime)  # change time type of newly created Datetime

#Aggregate hourly
F_319_rain_agg <- rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(seq(as.POSIXct("2007-10-01 01:00:00"), as.POSIXct("2018-06-04 11:00:00"), "hour"))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_F_319 <- time_seq %>%
  left_join(F_319_well_agg, by='datetime') %>%
  #left_join(F_319_tide_agg, by='datetime') %>%
  left_join(F_319_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_F_319))

# Only need to impute for well_ft and tide_ft
df_F_319 <- df_F_319 %>%
  mutate(well_ft = na.approx(well_ft, rule=2))
         #tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_F_319))

df_F_319$Well = "F-319"

################################# Well G-561 ###################################

G_561_well <- read_xlsx('G-561_T.xlsx', 'Well')
G_561_well <- mutate(G_561_well, datetime=date(date))  # adds date to datetime
hour(G_561_well$datetime) <- hour(G_561_well$time) # Adds hour to datetime. Removes minutes from all hours
G_561_well$datetime <- as.POSIXct(G_561_well$datetime)  # change time type of newly created Datetime

#Aggregate Well Data by hour
G_561_well_agg <- G_561_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data Hourly ##

#Create datetime column
# G_561_tide <- fread('station_8722956.csv')
# G_561_tide <- mutate(G_561_tide, 
#                Time = paste(Date, Time),
#                datetime=date(Date))  # adds date to datetime
# hour(G_561_tide$datetime) <- hour(G_561_tide$Time)  # Adds hour to datetime. Removes minutes from all hours
# G_561_tide$datetime <- as.POSIXct(G_561_tide$datetime)  # change time type of newly created Datetime

#Aggregate hourly
# G_561_tide_agg <- G_561_tide %>%
#   group_by(datetime) %>%
#   summarise(tide_ft = mean(Prediction))




## Aggregate Rain Data Hourly ##

#Create datetime column
G_561_rain <- read_xlsx('G-561_T.xlsx', 'Rain')
G_561_rain <- mutate(G_561_rain, datetime=date(Date))
hour(G_561_rain$datetime) <- hour(G_561_rain$Date) # adds date to datetime
G_561_rain$datetime <- as.POSIXct(G_561_rain$datetime)  # change time type of newly created Datetime

#Aggregate hourly
G_561_rain_agg <- G_561_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(seq(as.POSIXct("2007-10-05 00:00:00"), as.POSIXct("2018-06-12 23:00:00"), "hour"))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_561 <- time_seq %>%
  left_join(G_561_well_agg, by='datetime') %>%
  #left_join(G_561_tide_agg, by='datetime') %>%
  left_join(G_561_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_G_561))

# Only need to impute for well_ft and tide_ft
df_G_561 <- df_G_561 %>%
  mutate(well_ft = na.approx(well_ft, rule=2))
         #tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_G_561))

df_G_561$Well = "G-561"

################################# Well G-580A ###################################

G_580A_well <- read_xlsx('G-580A.xlsx', 'Well')
G_580A_well <- mutate(G_580A_well, datetime=date(date))  # adds date to datetime
hour(G_580A_well$datetime) <- hour(G_580A_well$time) # Adds hour to datetime. Removes minutes from all hours
G_580A_well$datetime <- as.POSIXct(G_580A_well$datetime)  # change time type of newly created Datetime

#Aggregate Well Data by hour
G_580A_well_agg <- G_580A_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data Hourly ##

#Create datetime column
# G_580A_tide <- read_xlsx('G-580A.xlsx', 'Tide')
# G_580A_tide <- mutate(G_580A_tide, datetime=date(Date))  # adds date to datetime
# hour(G_580A_tide$datetime) <- hour(G_580A_tide$Time) # Adds hour to datetime. Removes minutes from all hours
# G_580A_tide$datetime <- as.POSIXct(G_580A_tide$datetime)  # change time type of newly created Datetime
# 
# #Aggregate hourly
# G_580A_tide_agg <- G_580A_tide %>%
#   group_by(datetime) %>%
#   summarise(tide_ft = mean(Tide_ft))




## Aggregate Rain Data Hourly ##

#Create datetime column
G_580A_rain <- read_xlsx('G-580A.xlsx', 'Rain')
G_580A_rain <- mutate(G_580A_rain, datetime=date(Date))
hour(G_580A_rain$datetime) <- hour(G_580A_rain$Date) # adds date to datetime
G_580A_rain$datetime <- as.POSIXct(G_580A_rain$datetime)  # change time type of newly created Datetime

#Aggregate hourly
G_580A_rain_agg <- G_580A_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(seq(as.POSIXct("2007-10-01 00:00:00"), as.POSIXct("2018-04-09 11:00:00"), "hour"))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_580A <- time_seq %>%
  left_join(G_580A_well_agg, by='datetime') %>%
  #left_join(G_580A_tide_agg, by='datetime') %>%
  left_join(G_580A_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_G_580A))

# Only need to impute for well_ft
df_G_580A <- df_G_580A %>%
  mutate(well_ft = na.approx(well_ft, rule=2))

colSums(is.na(df_G_580A))

df_G_580A$Well = "G-580A"

################################# Well G-852 ###################################

G_852_well <- read_xlsx('G-852.xlsx', 'Well')
G_852_well <- mutate(G_852_well, datetime=date(Date))  # adds date to datetime
hour(G_852_well$datetime) <- hour(G_852_well$Time) # Adds hour to datetime. Removes minutes from all hours
G_852_well$datetime <- as.POSIXct(G_852_well$datetime)  # change time type of newly created Datetime

#Aggregate Well Data by hour
G_852_well_agg <- G_852_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data Hourly ##

#Create datetime column
# G_852_tide <- read_xlsx('G-852.xlsx', 'Tide')
# G_852_tide <- mutate(G_852_tide, datetime=date(Date))  # adds date to datetime
# hour(G_852_tide$datetime) <- hour(G_852_tide$Time) # Adds hour to datetime. Removes minutes from all hours
# G_852_tide$datetime <- as.POSIXct(G_852_tide$datetime)  # change time type of newly created Datetime
# 
# #Aggregate hourly
# G_852_tide_agg <- G_852_tide %>%
#   group_by(datetime) %>%
#   summarise(tide_ft = mean(Tide_ft))




## Aggregate Rain Data Hourly ##

#Create datetime column
G_852_rain <- read_xlsx('G-852.xlsx', 'Rain')
G_852_rain <- mutate(G_852_rain, datetime=date(Date))
hour(G_852_rain$datetime) <- hour(G_852_rain$Date) # adds date to datetime
G_852_rain$datetime <- as.POSIXct(G_852_rain$datetime)  # change time type of newly created Datetime

#Aggregate hourly
G_852_rain_agg <- rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(seq(as.POSIXct("2007-10-01 01:00:00"), as.POSIXct("2018-04-09 13:00:00"), "hour"))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_852 <- time_seq %>%
  left_join(G_852_well_agg, by='datetime') %>%
  #left_join(G_852_tide_agg, by='datetime') %>%
  left_join(G_852_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_G_852))


# Only need to impute for well_ft and tide_ft CHECK ON TIDE VALUES
df_G_852 <- df_G_852 %>%
  mutate(well_ft = na.approx(well_ft, rule=2))
        #tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_G_852))

df_G_852$Well = "G-852"
################################# Well G-860 ###################################

G_860_well <- read_xlsx('G-860.xlsx', 'Well')
G_860_well <- mutate(G_860_well, datetime=date(date))  # adds date to datetime
hour(G_860_well$datetime) <- hour(G_860_well$time) # Adds hour to datetime. Removes minutes from all hours
G_860_well$datetime <- as.POSIXct(G_860_well$datetime)  # change time type of newly created Datetime

#Aggregate Well Data by hour
G_860_well_agg <- G_860_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data Hourly ##

#Create datetime column
# G_860_tide <- read_xlsx('G-860.xlsx', 'Tide')
# G_860_tide <- mutate(tide, datetime=date(Date))  # adds date to datetime
# hour(G_860_tide$datetime) <- hour(G_860_tide$Time) # Adds hour to datetime. Removes minutes from all hours
# G_860_tide$datetime <- as.POSIXct(G_860tide$datetime)  # change time type of newly created Datetime
# 
# #Aggregate hourly
# G_860_tide_agg <- G_860_tide %>%
#   group_by(datetime) %>%
#   summarise(tide_ft = mean(Tide_ft))




## Aggregate Rain Data Hourly ##

#Create datetime column
G_860_rain <- read_xlsx('G-860.xlsx', 'Rain')
G_860_rain <- mutate(rain, datetime=date(Date))
hour(G_860_rain$datetime) <- hour(G_860_rain$Date) # adds date to datetime
G_860_rain$datetime <- as.POSIXct(G_860_rain$datetime)  # change time type of newly created Datetime

#Aggregate hourly
G_860_rain_agg <- G_860_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(seq(as.POSIXct("2007-10-01 01:00:00"), as.POSIXct("2018-06-04 12:00:00"), "hour"))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_860 <- time_seq %>%
  left_join(G_860_well_agg, by='datetime') %>%
  #left_join(G_860_tide_agg, by='datetime') %>%
  left_join(G_860_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_G_860))

# Only need to impute for well_ft
df_G_860 <- df_G_860 %>%
  mutate(well_ft = na.approx(well_ft, rule=2))

colSums(is.na(df_G_860))

df_G_860$Well = "G-860"
################################# Well G-1220 ###################################


##FOLLOW UP ON TIDE DATA, EXTERNAL FILE HAS NO VALUES##
G_1220_well <- read_xlsx('G-1220_T.xlsx', 'Well')
G_1220_well <- mutate(G_1220_well, datetime=date(date))  # adds date to datetime
hour(G_1220_well$datetime) <- hour(G_1220_well$time) # Adds hour to datetime. Removes minutes from all hours
G_1220_well$datetime <- as.POSIXct(G_1220_well$datetime)  # change time type of newly created Datetime

#Aggregate Well Data by hour
G_1220_well_agg <- G_1220_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data Hourly ##

#Create datetime column
# tide <- fread('station_8722939.csv')
# tide <- mutate(tide, 
#                Time = paste(Date, Time),
#                datetime=date(Date))  # adds date to datetime
# hour(tide$datetime) <- hour(tide$Time)  # Adds hour to datetime. Removes minutes from all hours
# tide$datetime <- as.POSIXct(tide$datetime)  # change time type of newly created Datetime
# 
# #Aggregate hourly
# G_1220_tide_agg <- tide %>%
#   group_by(datetime) %>%
#   summarise(tide_ft = mean(Prediction))




## Aggregate Rain Data Hourly ##

#Create datetime column
G_1220_rain <- read_xlsx('G-1220_T.xlsx', 'Rain')
G_1220_rain <- mutate(G_1220_rain, datetime=date(Date))
hour(G_1220_rain$datetime) <- hour(G_1220_rain$Date) # adds date to datetime
G_1220_rain$datetime <- as.POSIXct(G_1220_rain$datetime)  # change time type of newly created Datetime

#Aggregate hourly
G_1220_rain_agg <- G_1220_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(seq(as.POSIXct("2007-10-01 01:00:00"), as.POSIXct("2018-04-21 21:00:00"), "hour"))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_1220 <- time_seq %>%
  left_join(G_1220_well_agg, by='datetime') %>%
  #left_join(G_1220_tide_agg, by='datetime') %>%
  left_join(G_1220_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_G_1220))

# Only need to impute for well_ft and tide_ft
#df_G_1220 <- df_G_1220 %>%
 # mutate(well_ft = na.approx(well_ft, rule=2),
         #tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_G_1220))

df_G_1220$Well = "G-1220"
################################# Well G-1260 ###################################


##FOLLOW UP ON TIDE DATA, EXTERNAL FILE HAS NO VALUES##
G_1260_well <- read_xlsx('G-1260_T.xlsx', 'Well')
G_1260_well <- mutate(G_1260_well, datetime=date(date))  # adds date to datetime
hour(G_1260_well$datetime) <- hour(G_1260_well$time) # Adds hour to datetime. Removes minutes from all hours
G_1260_well$datetime <- as.POSIXct(G_1260_well$datetime)  # change time type of newly created Datetime

#Aggregate Well Data by hour
G_1260_well_agg <- G_1260_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data Hourly ##

#Create datetime column
# G_126_tide <- fread('station_8722802.csv')
# G_1260_tide <- mutate(G_1260_tide, 
#                Time = paste(Date, Time),
#                datetime=date(Date))  # adds date to datetime
# hour(G_1260_tide$datetime) <- hour(G_1260_tide$Time)  # Adds hour to datetime. Removes minutes from all hours
# G_1260_tide$datetime <- as.POSIXct(G_1260_tide$datetime)  # change time type of newly created Datetime
# 
# #Aggregate hourly
# G_1260_tide_agg <- G_1260_tide %>%
#   group_by(datetime) %>%
#   summarise(tide_ft = mean(Prediction))




## Aggregate Rain Data Hourly ##

#Create datetime column
G_1260_rain <- read_xlsx('G-1260_T.xlsx', 'Rain')
G_1260_rain <- mutate(G_1260_rain, datetime=date(Date))
hour(G_1260_rain$datetime) <- hour(G_1260_rain$Date) # adds date to datetime
G_1260_rain$datetime <- as.POSIXct(G_1260_rain$datetime)  # change time type of newly created Datetime

#Aggregate hourly
G_1260_rain_agg <- G_1260_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(seq(as.POSIXct("2007-10-01 01:00:00"), as.POSIXct("2018-06-08 11:00:00"), "hour"))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_1260 <- time_seq %>%
  left_join(G_1260_well_agg, by='datetime') %>%
  #left_join(G_1260_tide_agg, by='datetime') %>%
  left_join(G_1260_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_G_1260))

 #Only need to impute for well_ft and tide_ft
df_G_1260 <- df_G_1260 %>%
 mutate(well_ft = na.approx(well_ft, rule=2))
#tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_G_1260))

df_G_1260$Well = "G-1260"

################################# Well G-2147 ###################################


##FOLLOW UP ON TIDE DATA, EXTERNAL FILE HAS NO VALUES##
G_2147_well <- read_xlsx('G-2147_T.xlsx', 'Well')
G_2147_well <- mutate(G_2147_well, datetime=date(date))  # adds date to datetime
hour(G_2147_well$datetime) <- hour(G_2147_well$time) # Adds hour to datetime. Removes minutes from all hours
G_2147_well$datetime <- as.POSIXct(G_2147_well$datetime)  # change time type of newly created Datetime

#Aggregate Well Data by hour
G_2147_well_agg <- G_2147_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data Hourly ##

#Create datetime column
# G_2147_tide <- fread('station_8722859.csv')
# G_1247_tide <- mutate(G_1247_tide, 
#                Time = paste(Date, Time),
#                datetime=date(Date))  # adds date to datetime
# hour(G_2147_tide$datetime) <- hour(G_2147_tide$Time)  # Adds hour to datetime. Removes minutes from all hours
# G_2147_tide$datetime <- as.POSIXct(G_2147_tide$datetime)  # change time type of newly created Datetime
# 
# #Aggregate hourly
# G_2147_tide_agg <- G_2147_tide %>%
#   group_by(datetime) %>%
#   summarise(tide_ft = mean(Prediction))




## Aggregate Rain Data Hourly ##

#Create datetime column
G_2147_rain <- read_xlsx('G-2147_T.xlsx', 'Rain')
G_2147_rain <- mutate(G_2147_rain, datetime=date(Date))
hour(rain$datetime) <- hour(G_2147_rain$Date) # adds date to datetime
G_2147_rain$datetime <- as.POSIXct(G_2147_rain$datetime)  # change time type of newly created Datetime

#Aggregate hourly
G_2147_rain_agg <- G_2147_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(seq(as.POSIXct("2007-10-10 00:00:00"), as.POSIXct("2018-06-08 09:00:00"), "hour"))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_2147 <- time_seq %>%
  left_join(G_2147_well_agg, by='datetime') %>%
  #left_join(G_2147_tide_agg, by='datetime') %>%
  left_join(G_2147_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_G_2147))

#Only need to impute for well_ft and tide_ft
df_G_2147 <- df_G_2147 %>%
  mutate(well_ft = na.approx(well_ft, rule=2))
         #tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_G_2147))

df_G_2147$Well = "G-2147"
################################# Well G-3549 ###################################

G_3549_well <- read_xlsx('G-3549.xlsx', 'Well')
G_3549_well <- mutate(G_3549_well, datetime=date(date))  # adds date to datetime
hour(G_3549_well$datetime) <- hour(G_3549_well$time) # Adds hour to datetime. Removes minutes from all hours
G_3549_well$datetime <- as.POSIXct(G_3549_well$datetime)  # change time type of newly created Datetime

#Aggregate Well Data by hour
G_3549_well_agg <- G_3549_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data Hourly ##

#Create datetime column
# G_3549_tide <- read_xlsx('G-3549.xlsx', 'Tide')
# G_3549_tide <- mutate(G_3549_tide, datetime=date(Date))  # adds date to datetime
# hour(G_3549_tide$datetime) <- hour(G_3549_tide$Time) # Adds hour to datetime. Removes minutes from all hours
# G_3549_tide$datetime <- as.POSIXct(G_3549_tide$datetime)  # change time type of newly created Datetime
# 
# #Aggregate hourly
# G_3549_tide_agg <- G_3549_tide %>%
#   group_by(datetime) %>%
#   summarise(tide_ft = mean(Tide_ft))




## Aggregate Rain Data Hourly ##

#Create datetime column
G_3549_rain <- read_xlsx('G-3549.xlsx', 'Rain')
G_3549_rain <- mutate(G_3549_rain, datetime=date(Date))
hour(G_3549_rain$datetime) <- hour(G_3549_rain$Date) # adds date to datetime
G_3549_rain$datetime <- as.POSIXct(G_3549_rain$datetime)  # change time type of newly created Datetime

#Aggregate hourly
G_3549_rain_agg <- G_3549_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(seq(as.POSIXct("2007-10-01 00:00:00"), as.POSIXct("2018-06-12 23:00:00"), "hour"))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_3549 <- time_seq %>%
  left_join(G_3549_well_agg, by='datetime') %>%
  #left_join(G_3549_tide_agg, by='datetime') %>%
  left_join(G_3549_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_G_3549))

# Only need to impute for well_ft
df_G_3549 <- df_G_3549 %>%
  mutate(well_ft = na.approx(well_ft, rule=2))

colSums(is.na(df_G_3549))

df_G_3549$Well = "G-3549"
####################################### Well PB-1680 ##############################################

##FOLLOW UP ON TIDE DATA, EXTERNAL FILE HAS NO VALUES##
PB_1680_well <- read_xlsx('PB-1680_T.xlsx', 'Well')
PB_1680_well <- mutate(PB_1680_well, datetime=date(date))  # adds date to datetime
hour(PB_1680_well$datetime) <- hour(PB_1680_well$time) # Adds hour to datetime. Removes minutes from all hours
PB_1680_well$datetime <- as.POSIXct(PB_1680_well$datetime)  # change time type of newly created Datetime

#Aggregate Well Data by hour
PB_1680_well_agg <- PB_1680_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data Hourly ##

#Create datetime column
# PB_1680_tide <- fread('station_8722802.csv')
# PB_1680_tide <- mutate(PB_1680_tide, 
#                Time = paste(Date, Time),
#                datetime=date(Date))  # adds date to datetime
# hour(PB_1680_tide$datetime) <- hour(PB_1680_tide$Time)  # Adds hour to datetime. Removes minutes from all hours
# PB_1680_tide$datetime <- as.POSIXct(PB_1680_tide$datetime)  # change time type of newly created Datetime
# 
# #Aggregate hourly
# PB_1680_tide_agg <- PB_1680_tide %>%
#   group_by(datetime) %>%
#   summarise(tide_ft = mean(Prediction))




## Aggregate Rain Data Hourly ##

#Create datetime column
PB_1680_rain <- read_xlsx('PB-1680_T.xlsx', 'Rain')
PB_1680_rain <- mutate(PB_1680_rain, datetime=date(Date))
hour(PB_1680_rain$datetime) <- hour(PB_1680_rain$Date) # adds date to datetime
PB_1680_rain$datetime <- as.POSIXct(PB_1680_rain$datetime)  # change time type of newly created Datetime

#Aggregate hourly
PB_1680_rain_agg <- PB_1680_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(seq(as.POSIXct("2007-10-01 01:00:00"), as.POSIXct("2018-02-08 09:00:00"), "hour"))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_PB_1680 <- time_seq %>%
  left_join(PB_1680_well_agg, by='datetime') %>%
  #left_join(PB_1680_tide_agg, by='datetime') %>%
  left_join(PB_1680_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_PB_1680))

#Only need to impute for well_ft and tide_ft
df_PB_1680 <- df_PB_1680 %>%
  mutate(well_ft = na.approx(well_ft, rule=2))
         #tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_PB_1680))


df_PB_1680$Well = "PB-1680"


############################# Merge All Wells ###################################

Well_merge = Reduce(function(x, y) merge(x, y, all=TRUE), list(df_F_45, df_F_179, df_F_319, df_G_561, df_G_580A, df_G_852, df_G_860, df_G_1220, df_G_1260, df_G_2147, df_G_2866, df_G_3549, df_PB_1680))

sum(is.na(Well_merge$tide_ft))

############################### Export to csv or SAS ########################################
fwrite(df, '../Outputs/combined_well.csv')

##############################Create text files to import into SAS###########################

library(foreign)


#write.foreign(df, "C:/Users/senor/Documents/Time_Series/timeseries2_hw2.txt", "C:/Users/senor/Documents/Time_Series/timeseries2_hw2.sas", package = "SAS")

