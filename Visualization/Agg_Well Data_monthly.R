
library(readxl)
library(lubridate)
library(zoo)
library(dplyr)
library(data.table)

setwd('/Users/anniecooper/Documents/MSA_2019/VIsualization/Well Data/')

################################# Well G-2866 ###################################

#Create Date Time Object
G_2866_well <- read_xlsx('G-2866_T.xlsx', 'Well')
  
G_2866_well <- G_2866_well %>% # adds date to datetime
  mutate(datetime=as.Date(date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate Well Data by month
G_2866_well_agg <- G_2866_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))

## Aggregate Tide Data ##

#Create datetime column
G_2866_tide <- fread('station_8722859.csv')
G_2866_tide <- G_2866_tide %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))


#Aggregate Monthly
G_2866_tide_agg <- G_2866_tide %>%
  group_by(datetime) %>%
  summarise(tide_ft = mean(Prediction))


## Aggregate Rain Data Monthly ##

#Create datetime column
G_2866_rain <- read_xlsx('G-2866_T.xlsx', 'Rain')
G_2866_rain <- G_2866_rain %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))


#Aggregate hourly
G_2866_rain_agg <- G_2866_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))

## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(as.yearmon(seq(as.POSIXct("2007-10-01"), as.POSIXct("2018-06-08"), "month")))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_2866 <- time_seq %>%
  left_join(G_2866_well_agg, by='datetime') %>%
  left_join(G_2866_rain_agg, by='datetime') %>%
  left_join(G_2866_tide_agg, by='datetime')


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

## Aggregate Well Data monthly ##

#Create Date Time Object for Well F-45
F_45_well <- read_xlsx('F-45.xlsx', 'Well')
F_45_well <- F_45_well %>% # adds date to datetime
  mutate(datetime=as.Date(date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate Well Data by month
F_45_well_agg <- F_45_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))

## Aggregate Tide Data monthly ##

#Create datetime column
F_45_tide <- read_xlsx('F-45.xlsx', 'Tide')
F_45_tide <- F_45_tide %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
F_45_tide_agg <- F_45_tide %>%
  group_by(datetime) %>%
  summarise(tide_ft = mean(Tide_ft))

## Aggregate Rain Data Monthly ##

#Create datetime column
F_45_rain <- read_xlsx('F-45.xlsx', 'Rain')
F_45_rain <- F_45_rain %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
F_45_rain_agg <- F_45_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(`RAIN _FT`*12))

## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(as.yearmon(seq(as.POSIXct("2007-10-01"), as.POSIXct("2018-03-26"), "month")))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_F_45 <- time_seq %>%
  left_join(F_45_well_agg, by='datetime') %>%
  left_join(F_45_tide_agg, by='datetime') %>%
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
F_179_well <- F_179_well %>% # adds date to datetime
  mutate(datetime=as.Date(date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate Well Data by month
F_179_well_agg <- F_179_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))

## Aggregate Tide Data monthly ##

#Create datetime column
F_179_tide <- read_xlsx('F-179.xlsx', 'Tide')
F_179_tide <- F_179_tide %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

# #Aggregate monthly
F_179_tide_agg <- F_179_tide %>%
  group_by(datetime) %>%
  summarise(tide_ft = mean(Tide_ft))

## Aggregate Rain Data Monthly ##

#Create datetime column
F_179_rain <- read_xlsx('F-179.xlsx', 'Rain')
F_179_rain <- F_179_rain %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
F_179_rain_agg <- F_179_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))

## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(as.yearmon(seq(as.POSIXct("2007-10-01"), as.POSIXct("2018-06-04"), "month")))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_F_179 <- time_seq %>%
  left_join(F_179_well_agg, by='datetime') %>%
  left_join(F_179_tide_agg, by='datetime') %>%
  left_join(F_179_rain_agg, by='datetime')

## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_F_179))

# Only need to impute for well_ft and tide_ft
df_F_179 <- df_F_179 %>%
  mutate(well_ft = na.approx(well_ft, rule=2),
         tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_F_179))

df_F_179$Well = "F-179"

################################# Well F-319 ###################################

F_319_well <- read_xlsx('F-319.xlsx', 'Well')
F_319_well <- F_319_well %>% # adds date to datetime
  mutate(datetime=as.Date(date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))


#Aggregate Well Data by hour
F_319_well_agg <- F_319_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))


## Aggregate Tide Data Hourly ##

#Create datetime column
F_319_tide <- read_xlsx('F-319.xlsx', 'Tide')
F_319_tide <- F_319_tide %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate hourly
F_319_tide_agg <- F_319_tide %>%
  group_by(datetime) %>%
  summarise(tide_ft = mean(Tide_ft))

## Aggregate Rain Data Monthly ##

#Create datetime column
F_319_rain <- read_xlsx('F-319.xlsx', 'Rain')
F_319_rain <- F_319_rain %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
F_319_rain_agg <- F_319_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))


## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(as.yearmon(seq(as.POSIXct("2007-10-01"), as.POSIXct("2018-04-09"), "month")))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_F_319 <- time_seq %>%
  left_join(F_319_well_agg, by='datetime') %>%
  left_join(F_319_tide_agg, by='datetime') %>%
  left_join(F_319_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_F_319))

# Only need to impute for well_ft and tide_ft
df_F_319 <- df_F_319 %>%
  mutate(well_ft = na.approx(well_ft, rule=2),
         tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_F_319))

df_F_319$Well = "F-319"

################################# Well G-561 ###################################

G_561_well <- read_xlsx('G-561_T.xlsx', 'Well')
G_561_well <- G_561_well %>% # adds date to datetime
  mutate(datetime=as.Date(date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate Well Data by Month
G_561_well_agg <- G_561_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))

## Aggregate Tide Data Monthy ##

#Create datetime column
G_561_tide <- fread('station_8722956.csv')
G_561_tide <- G_561_tide %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate hourly
G_561_tide_agg <- G_561_tide %>%
  group_by(datetime) %>%
  summarise(tide_ft = mean(Prediction))


## Aggregate Rain Data Monthly ##

#Create datetime column
G_561_rain <- read_xlsx('G-561_T.xlsx', 'Rain')
G_561_rain <- G_561_rain %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
G_561_rain_agg <- G_561_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))

## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(as.yearmon(seq(as.POSIXct("2007-10-05"), as.POSIXct("2018-06-12"), "month")))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_561 <- time_seq %>%
  left_join(G_561_well_agg, by='datetime') %>%
  left_join(G_561_tide_agg, by='datetime') %>%
  left_join(G_561_rain_agg, by='datetime')


## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_G_561))

# Only need to impute for well_ft and tide_ft
df_G_561 <- df_G_561 %>%
  mutate(well_ft = na.approx(well_ft, rule=2),
         tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_G_561))

df_G_561$Well = "G-561"

################################# Well G-580A ###################################

G_580A_well <- read_xlsx('G-580A.xlsx', 'Well')
G_580A_well <- G_580A_well %>% # adds date to datetime
  mutate(datetime=as.Date(date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate Well Data by Month
G_580A_well_agg <- G_580A_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))


## Aggregate Tide Data Monthly ##

#Create datetime column
G_580A_tide <- read_xlsx('G-580A.xlsx', 'Tide')
G_580A_tide <- G_580A_tide %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate hourly
G_580A_tide_agg <- G_580A_tide %>%
  group_by(datetime) %>%
  summarise(tide_ft = mean(Tide_ft))


## Aggregate Rain Data Monthly ##

#Create datetime column
G_580A_rain <- read_xlsx('G-580A.xlsx', 'Rain')
G_580A_rain <-  G_580A_rain %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate hourly
G_580A_rain_agg <- G_580A_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))


## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(as.yearmon(seq(as.POSIXct("2007-10-01"), as.POSIXct("2018-04-09"), "month")))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_580A <- time_seq %>%
  left_join(G_580A_well_agg, by='datetime') %>%
  left_join(G_580A_tide_agg, by='datetime') %>%
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
G_852_well <- G_852_well %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate Well Data by month
G_852_well_agg <- G_852_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))


## Aggregate Tide Data Hourly ##

#Create datetime column
G_852_tide <- read_xlsx('G-852.xlsx', 'Tide')
G_852_tide <- G_852_tide %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

## Aggregate monthly
G_852_tide_agg <- G_852_tide %>%
group_by(datetime) %>%
summarise(tide_ft = mean(Tide_ft))


## Aggregate Rain Data Monthly ##

#Create datetime column
G_852_rain <- read_xlsx('G-852.xlsx', 'Rain')
G_852_rain <- G_852_rain %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate hourly
G_852_rain_agg <- G_852_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))


## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(as.yearmon(seq(as.POSIXct("2007-10-01"), as.POSIXct("2018-04-09"), "month")))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_852 <- time_seq %>%
  left_join(G_852_well_agg, by='datetime') %>%
  left_join(G_852_tide_agg, by='datetime') %>%
  left_join(G_852_rain_agg, by='datetime')


## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_G_852))


# Only need to impute for well_ft and tide_ft CHECK ON TIDE VALUES
df_G_852 <- df_G_852 %>%
  mutate(well_ft = na.approx(well_ft, rule=2)) %>%
  mutate(tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_G_852))

df_G_852$Well = "G-852"
################################# Well G-860 ###################################

G_860_well <- read_xlsx('G-860.xlsx', 'Well')
G_860_well <- G_860_well %>% # adds date to datetime
  mutate(datetime=as.Date(date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate Well Data by month
G_860_well_agg <- G_860_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))


## Aggregate Tide Data Monthly ##

#Create datetime column
G_860_tide <- read_xlsx('G-860.xlsx', 'Tide')
G_860_tide <- G_860_tide %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
G_860_tide_agg <- G_860_tide %>%
  group_by(datetime) %>%
  summarise(tide_ft = mean(Tide_ft))


## Aggregate Rain Data Monthly ##

#Create datetime column
G_860_rain <- read_xlsx('G-860.xlsx', 'Rain')
G_860_rain <- G_860_rain %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
G_860_rain_agg <- G_860_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))


## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(as.yearmon(seq(as.POSIXct("2007-10-01"), as.POSIXct("2018-06-04"), "month")))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_860 <- time_seq %>%
  left_join(G_860_well_agg, by='datetime') %>%
  left_join(G_860_tide_agg, by='datetime') %>%
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
G_1220_well <- G_1220_well %>% # adds date to datetime
  mutate(datetime=as.Date(date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate Well Data by month
G_1220_well_agg <- G_1220_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))


## Aggregate Tide Data monthly ##

#Create datetime column
G_1220_tide <- fread('station_8722956.csv')
G_1220_tide <- G_1220_tide %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
G_1220_tide_agg <- G_1220_tide %>%
  group_by(datetime) %>%
  summarise(tide_ft = mean(Prediction))




## Aggregate Rain Data monthly ##

#Create datetime column
G_1220_rain <- read_xlsx('G-1220_T.xlsx', 'Rain')
G_1220_rain <- G_1220_rain %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
G_1220_rain_agg <- G_1220_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(as.yearmon(seq(as.POSIXct("2007-10-01"), as.POSIXct("2018-04-21"), "month")))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_1220 <- time_seq %>%
  left_join(G_1220_well_agg, by='datetime') %>%
  left_join(G_1220_tide_agg, by='datetime') %>%
  left_join(G_1220_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_G_1220))

# Only need to impute for well_ft and tide_ft
df_G_1220 <- df_G_1220 %>%
  mutate(well_ft = na.approx(well_ft, rule=2),
         tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_G_1220))

df_G_1220$Well = "G-1220"
################################# Well G-1260 ###################################


G_1260_well <- read_xlsx('G-1260_T.xlsx', 'Well')
G_1260_well <- G_1260_well %>% # adds date to datetime
  mutate(datetime=as.Date(date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate Well Data by month
G_1260_well_agg <- G_1260_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data monthly ##

#Create datetime column
G_1260_tide <- fread('station_8722802.csv')
G_1260_tide <- G_1260_tide %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
G_1260_tide_agg <- G_1260_tide %>%
  group_by(datetime) %>%
  summarise(tide_ft = mean(Prediction))


## Aggregate Rain Data monthly ##

#Create datetime column
G_1260_rain <- read_xlsx('G-1260_T.xlsx', 'Rain')
G_1260_rain <- G_1260_rain %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
G_1260_rain_agg <- G_1260_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(as.yearmon(seq(as.POSIXct("2007-10-01"), as.POSIXct("2018-06-08"), "month")))
names(time_seq) <- c("datetime")



#Merge Left Outer join of time sequence on well data
df_G_1260 <- time_seq %>%
  left_join(G_1260_well_agg, by='datetime') %>%
  left_join(G_1260_tide_agg, by='datetime') %>%
  left_join(G_1260_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_G_1260))

#Only need to impute for well_ft and tide_ft
df_G_1260 <- df_G_1260 %>%
  mutate(well_ft = na.approx(well_ft, rule=2),
         tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_G_1260))

df_G_1260$Well = "G-1260"

################################# Well G-2147 ###################################



G_2147_well <- read_xlsx('G-2147_T.xlsx', 'Well')
G_2147_well <- G_2147_well %>% # adds date to datetime
  mutate(datetime=as.Date(date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate Well Data by month
G_2147_well_agg <- G_2147_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data monthly ##

#Create datetime column
G_2147_tide <- fread('station_8722859.csv')
G_2147_tide <- G_2147_tide %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
G_2147_tide_agg <- G_2147_tide %>%
  group_by(datetime) %>%
  summarise(tide_ft = mean(Prediction))


## Aggregate Rain Data monthly ##

#Create datetime column
G_2147_rain <- read_xlsx('G-2147_T.xlsx', 'Rain')
G_2147_rain <- G_2147_rain %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
G_2147_rain_agg <- G_2147_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))

## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(as.yearmon(seq(as.POSIXct("2007-10-10"), as.POSIXct("2018-06-08"), "month")))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_2147 <- time_seq %>%
  left_join(G_2147_well_agg, by='datetime') %>%
  left_join(G_2147_tide_agg, by='datetime') %>%
  left_join(G_2147_rain_agg, by='datetime')

## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_G_2147))

#Only need to impute for well_ft and tide_ft
df_G_2147 <- df_G_2147 %>%
  mutate(well_ft = na.approx(well_ft, rule=2),
         rain_in = na.approx(rain_in, rule=2),
         tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_G_2147))

df_G_2147$Well = "G-2147"
################################# Well G-3549 ###################################

G_3549_well <- read_xlsx('G-3549.xlsx', 'Well')
G_3549_well <- G_3549_well %>% # adds date to datetime
  mutate(datetime=as.Date(date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate Well Data by month
G_3549_well_agg <- G_3549_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data monthly ##

#Create datetime column
G_3549_tide <- read_xlsx('G-3549.xlsx', 'Tide')
G_3549_tide <- G_3549_tide %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
G_3549_tide_agg <- G_3549_tide %>%
  group_by(datetime) %>%
  summarise(tide_ft = mean(Tide_ft))


## Aggregate Rain Data monthly ##

#Create datetime column
G_3549_rain <- read_xlsx('G-3549.xlsx', 'Rain')
G_3549_rain <- G_3549_rain %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate hourly
G_3549_rain_agg <- G_3549_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(as.yearmon(seq(as.POSIXct("2007-10-01"), as.POSIXct("2018-06-12"), "month")))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_G_3549 <- time_seq %>%
  left_join(G_3549_well_agg, by='datetime') %>%
  left_join(G_3549_tide_agg, by='datetime') %>%
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
PB_1680_well <- PB_1680_well %>% # adds date to datetime
  mutate(datetime=as.Date(date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate Well Data by month
PB_1680_well_agg <- PB_1680_well %>%    # summarizes to hourly data and  
  group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected))



## Aggregate Tide Data monthly ##

#Create datetime column
PB_1680_tide <- fread('station_8722802.csv')
PB_1680_tide <- PB_1680_tide %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
PB_1680_tide_agg <- PB_1680_tide %>%
  group_by(datetime) %>%
  summarise(tide_ft = mean(Prediction))



## Aggregate Rain Data monthly ##

#Create datetime column
PB_1680_rain <- read_xlsx('PB-1680_T.xlsx', 'Rain')
PB_1680_rain <- PB_1680_rain %>% # adds date to datetime
  mutate(datetime=as.Date(Date)) %>%
  mutate(datetime=paste(year(datetime),'-',month(datetime),sep='')) %>%
  mutate(datetime=as.yearmon(datetime))

#Aggregate monthly
PB_1680_rain_agg <- PB_1680_rain %>%
  group_by(datetime) %>%
  summarise(rain_in = sum(RAIN_FT*12))



## Merge all data ##

# create full sequence of dates
time_seq <- as.data.frame(as.yearmon(seq(as.POSIXct("2007-10-01"), as.POSIXct("2018-02-08"), "month")))
names(time_seq) <- c("datetime")

#Merge Left Outer join of time sequence on well data
df_PB_1680 <- time_seq %>%
  left_join(PB_1680_well_agg, by='datetime') %>%
  left_join(PB_1680_tide_agg, by='datetime') %>%
  left_join(PB_1680_rain_agg, by='datetime')



## Impute Missing Values ##

# print missing values by column
colSums(is.na(df_PB_1680))

#Only need to impute for well_ft and tide_ft
df_PB_1680 <- df_PB_1680 %>%
  mutate(well_ft = na.approx(well_ft, rule=2),
         tide_ft = na.approx(tide_ft, rule=2))

colSums(is.na(df_PB_1680))


df_PB_1680$Well = "PB-1680"


############################# Merge All Wells ###################################

Well_merge = Reduce(function(x, y) merge(x, y, all=TRUE), list(df_F_45, df_F_179, df_F_319, df_G_561, df_G_580A, df_G_852, df_G_860, df_G_1220, df_G_1260, df_G_2147, df_G_2866, df_G_3549, df_PB_1680))

#Well_merge = Well_merge %>% filter(Well_merge$datetime > "Oct 2014")

# ############################### Export to csv or SAS ########################################

write.csv(Well_merge,'combined_well_monthly.csv',row.names=FALSE)