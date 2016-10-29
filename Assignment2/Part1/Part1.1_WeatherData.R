# libraries used
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

#install.packages('weatherData')
library(weatherData)

installed.packages('zoo')
library(zoo)

# reading first six months data
rawdata1 <- read.csv(file="C:\\Users\\Ankur\\Desktop\\ADS\\ADS_Assignments\\Assignment2\\Part1\\rawData1.csv", stringsAsFactors = FALSE)
# reading second six months data
rawdata2 <- read.csv(file="C:\\Users\\Ankur\\Desktop\\ADS\\ADS_Assignments\\Assignment2\\Part1\\rawData2.csv", stringsAsFactors = FALSE)

# combining both the above data
rawdata <- rbind(rawdata1, rawdata2)

# filter on channel for selecting 'MILDRED SCHOOL 1', separating Dates as
# month, day and year, transforming columns into rows
rawdata_new_2 <- rawdata %>% 
    filter(Channel == 'MILDRED SCHOOL 1') %>% 
    separate(Date, c("month", "day", "year"), "/", remove = FALSE) %>%
    gather(Minute_Range, Value, -(Account:Units))

# convert minutes into numeric format
rawdata_new_2$Minute_Range <- str_sub(rawdata_new_2$Minute_Range, 2)
rawdata_new_2$Minute_Range <- as.numeric(rawdata_new_2$Minute_Range)    
    
# data tranformation and data wrangling
rawdata_new_3 <- rawdata_new_2 %>% 
    arrange(month, day, Minute_Range) %>%
    group_by(month, day) %>%
    mutate(hour = rep(0:23, each = 12), Date = mdy(Date)) %>%
    group_by(Account, Date, month, day, year, hour) %>%
    summarise(kWh = sum(Value, na.rm = TRUE)) %>%
    mutate(Day.of.Week = wday(Date) - 1, 
           Weekday = ifelse(Day.of.Week %in% c(0,6), 0, 1), 
           Peakhour = ifelse(hour %in% c(7:18), 1, 0))

# write partial data into csv
write.csv (rawdata_new_3,"C:\\Users\\Ankur\\Desktop\\ADS\\ADS_Assignments\\Assignment2\\Part1\\partialData.csv")

# Fetching weatherdata using 'WeatherData' package
weatherdata <- getWeatherForDate("KBOS", "2014-01-01","2014-12-31",opt_detailed =TRUE, opt_custom_columns=TRUE,custom_columns=c(1,2,3,4,5,6,7,8,12,13))

# copy the weatherdata into other data frame
weatherdata_copy <- weatherdata

# separate Time into Date and hour
weatherdata_copy1 <- weatherdata_copy %>% separate(Time, c("Date","hour"), " ")
write.csv (weatherdata_copy1,"C:\\Users\\Ankur\\Desktop\\ADS\\ADS_Assignments\\Assignment2\\Part1\\weatherdata.csv")

weatherdata_copy1[grep(":54",weatherdata_copy1$Date)]

# creating vector for particular hour values
hour_data <- c('00:54:00' ,
               '01:54:00' ,
               '02:54:00' ,
               '03:54:00' ,
               '04:54:00' ,
               '05:54:00' ,
               '06:54:00' ,
               '07:54:00' ,
               '08:54:00' ,
               '09:54:00' ,
               '10:54:00' ,
               '11:54:00' ,
               '12:54:00' ,
               '13:54:00' ,
               '14:54:00' ,
               '15:54:00' ,
               '16:54:00' ,
               '17:54:00' ,
               '18:54:00' ,
               '19:54:00' ,
               '20:54:00' ,
               '21:54:00' ,
               '22:54:00' ,
               '23:54:00' )

# taking weather data only for values present in hour_data vector 
# (considering the latest value)
w_data <- weatherdata_copy1[weatherdata_copy1$hour %in% hour_data, ]
write.csv (w_data,"C:\\Users\\Ankur\\Desktop\\ADS\\ADS_Assignments\\Assignment2\\Part1\\weatherdatafull.csv")

# taking out the hour value from Time
names(w_data)[1]<-"Date"
w_data$hour[w_data$hour == "00:54:00"] <- '0'
w_data$hour[w_data$hour == "01:54:00"] <- '1'
w_data$hour[w_data$hour == "02:54:00"] <- '2'
w_data$hour[w_data$hour == "03:54:00"] <- '3'
w_data$hour[w_data$hour == "04:54:00"] <- '4'
w_data$hour[w_data$hour == "05:54:00"] <- '5'
w_data$hour[w_data$hour == "06:54:00"] <- '6'
w_data$hour[w_data$hour == "07:54:00"] <- '7'
w_data$hour[w_data$hour == "08:54:00"] <- '8'
w_data$hour[w_data$hour == "09:54:00"] <- '9'
w_data$hour[w_data$hour == "10:54:00"] <- '10'
w_data$hour[w_data$hour == "11:54:00"] <- '11'
w_data$hour[w_data$hour == "12:54:00"] <- '12'
w_data$hour[w_data$hour == "13:54:00"] <- '13'
w_data$hour[w_data$hour == "14:54:00"] <- '14'
w_data$hour[w_data$hour == "15:54:00"] <- '15'
w_data$hour[w_data$hour == "16:54:00"] <- '16'
w_data$hour[w_data$hour == "17:54:00"] <- '17'
w_data$hour[w_data$hour == "18:54:00"] <- '18'
w_data$hour[w_data$hour == "19:54:00"] <- '19'
w_data$hour[w_data$hour == "20:54:00"] <- '20'
w_data$hour[w_data$hour == "21:54:00"] <- '21'
w_data$hour[w_data$hour == "22:54:00"] <- '22'
w_data$hour[w_data$hour == "23:54:00"] <- '23'

# Renaming the Temperature Column
names(w_data)[names(w_data) == 'TemperatureF'] <- 'Temperature'

# merge partial data and weather data based on Date and hour columns
finalData<-merge(rawdata_new_3,w_data, by= c("Date","hour"), all.x = TRUE)

# Data Cleaning (droping unwanted columns, removing duplicates)
drops <- c("TimeEST","TimeEDT")
finalData<-finalData[ , !(names(finalData) %in% drops)]
finalData<-subset(finalData, !duplicated(subset(finalData, select=c(Date, hour))))

# data with 'NA' values
write.csv (finalData,"C:\\Users\\Ankur\\Desktop\\ADS\\ADS_Assignments\\Assignment2\\Part1\\finalData.csv")

# Handling 'NA' values using locf function
clean_data <- na.locf(finalData)

# Write final clean data into csv file
write.csv (clean_data,"C:\\Users\\Ankur\\Desktop\\ADS\\ADS_Assignments\\Assignment2\\Part1\\cleanData.csv")
