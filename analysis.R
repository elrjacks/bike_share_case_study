#Required installations 
install.packages("tidyverse")
install.packages("lubridate")
install.packages("readxl")
#Load packages
library(tidyverse)  #helps clean data 
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library("readxl")
#Set working directory 
getwd() 
setwd("C:/Users/18124/Documents/bike_share_case_study/Data/Unzip")  

#Gather data sets from the past 12 months June 2022 - May 2023 
june22 <- read_csv("202206-divvy-tripdata.csv")
july22 <- read_csv("202207-divvy-tripdata.csv")
august22 <- read.csv("202208-divvy-tripdata.csv")
sept22 <- read_csv("202209-divvy-tripdata.csv")
oct22 <- read_csv("202210-divvy-tripdata.csv")
nov22 <- read_csv("202211-divvy-tripdata.csv")
dec22 <- read_csv("202212-divvy-tripdata.csv")
jan23 <- read_csv("202301-divvy-tripdata.csv")
feb23 <- read_csv("202302-divvy-tripdata.csv")
march23 <- read_csv("202303-divvy-tripdata.csv")
april23 <- read_csv("202304-divvy-tripdata.csv")
may23 <- read_csv("202305-divvy-tripdata.csv")

#Check column names and ensure they are consistent for join 
colnames(june22)
colnames(july22)
colnames(august22)
colnames(sept22)
colnames(oct22)
colnames(nov22)
colnames(dec22)
colnames(jan23)
colnames(feb23)
colnames(march23)
colnames(april23)
colnames(may23)

#august data start and end date/time was coming as a character data type 
#we want to match it with all other data frames 
#convert to date time data type 
august22 <- mutate(august22, started_at = as.POSIXct(started_at))
august22 <- mutate(august22, ended_at = as.POSIXct(ended_at))

str(august22)

#Combine data into one data set 
all_trips <- bind_rows(june22, july22, august22, sept22, oct22, nov22, dec22, jan23, feb23, march23, april23, may23)


#Inspection of data 
#List of column names
colnames(all_trips) 
#How many rows total
nrow(all_trips) 
#Dimensions of the data frame 
dim(all_trips)  
#See first 6 rows of data frame 
head(all_trips) 
#See list of columns and data types (numeric, character, etc)
str(all_trips)  
#Statistical summary of data
summary(all_trips)  

#ensure that we only have two types of data 
#and see how many of each 
table(all_trips$member_casual)

# Add columns that list the date, month, day, year, and day of week of each ride
# This will allow us to aggregate ride data for each month, day, or year 
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

View(all_trips)

#calculate ride length (end time - start time (duration))
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#inspect the structure of the new columns 
str(all_trips) 

# Convert "ride_length" to numeric so we can run calculations on the data
is.numeric(all_trips$ride_length) #FALSE 
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few entries when ride_length was negative
all_trips_v2 <- all_trips[!(all_trips$ride_length<0),]
View(all_trips_v2)

# Descriptive analysis on ride_length (all figures in seconds)
#total ride length / rides
mean(all_trips_v2$ride_length)
#midpoint of ride lengths 
median(all_trips_v2$ride_length) 
#longest ride 
max(all_trips_v2$ride_length) 
#shortest ride 
min(all_trips_v2$ride_length) 

#info condensed 
summary(all_trips_v2$ride_length)

#compare members and casual users
#casual users are typically using the bike for longer trips 
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
#casual riders are riding more on the weekends (sat and sun)
#while members have about the same ride time throughout the week (a bit higher on the weekends)
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


#analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by user type and weekday
  summarise(number_of_rides = n()							#calculates the number of rides 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

#visualize the number of rides by user type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#avg duration by user type 
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'C:/Users/18124/Documents/bike_share_case_study/avg_ride_length.csv')

