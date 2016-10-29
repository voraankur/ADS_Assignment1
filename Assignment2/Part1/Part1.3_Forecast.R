library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(chron)
library(forecast)

# reading ForecastData.csv into dataframe
forecast_Input <- read.csv(file="C:\\Users\\Ankur\\Desktop\\ADS\\ADS_Assignments\\Assignment2\\Part1\\forecastData.csv", stringsAsFactors = FALSE)

# Separte Time into Date and Time, further separate Date into year, month and day
mid_data <- forecast_Input %>% 
  separate(Time, c("Date", "Time1"), " ", remove = TRUE) %>%
  separate(Date, c("year", "month", "day"), "-", remove = FALSE)

# separting hour from Time  
mid_data1 <- mid_data %>% data.frame(hour = str_sub(mid_data$Time1, -8, -7))  
  
# Assigning values for Day of Week, weekday and peakhour based on criteria
mid_data2 <- mid_data1 %>%  
  group_by(Date, month, day, year, hour) %>%
  mutate('Day of Week' = wday(Date) - 1, 
         Weekday = ifelse('Day of Week' %in% c(0,6), 0, 1), 
         Peakhour = ifelse(hour %in% c(7:18), 1, 0))

# drop unwanted columns
drops <- c("X","TimeEDT","DateUTC","Gust_SpeedMPH","PrecipitationIn","Events")
almost_data<-mid_data2[ , !(names(mid_data2) %in% drops)] 

# rearrange columns to match with ForecastInput.csv
forecast_data <- almost_data[, c(1,3,4,2,15,16,17,18,6,7,8,9,10,11,12,13,14)]

# Removing 0's from Sigle Digit values
forecast_data$hour <- lapply(forecast_data$hour, function(y) sub('^0+(?=[0-9])', '', y, perl=TRUE)) 

# Renaming columns
names(forecast_data)[names(forecast_data) == 'TemperatureF'] <- 'Temperature'
names(forecast_data)[names(forecast_data) == 'Day of Week'] <- 'Day.of.Week'

# Changing the data type as numeric
forecast_data$hour <- as.numeric(forecast_data$hour)
forecast_data$month <- as.numeric(forecast_data$month)
forecast_data$day <- as.numeric(forecast_data$day)

# Reading the clean data we received from 1st part
cleanData <- read.csv(file="C:\\Users\\Ankur\\Desktop\\ADS\\ADS_Assignments\\Assignment2\\Part1\\cleanData.csv", stringsAsFactors = FALSE)

# predction using backward selection
bkwrd.lm= step(lm(kWh~hour + month +year+ day+Day.of.Week+Weekday+
                    Peakhour+Temperature+Dew_PointF+Humidity+Sea_Level_PressureIn+
                    VisibilityMPH+Wind_Direction+Wind_SpeedMPH+WindDirDegrees+
                    Conditions, data = cleanData), direction = 'backward')
new.df <- forecast_data
forecast_output <- predict(stepWise.lm, new.df)

# Adding columns for kWh to our existing data frame
forecast_data["kWh"] <- NA
forecast_data$kWh <- forecast_output

write.csv(forecast_data, sprintf("C:\\Users\\Ankur\\Desktop\\ADS\\ADS_Assignments\\Assignment2\\Part1\\forecastOutput_%s.csv", cleanData$Account[1]))
  