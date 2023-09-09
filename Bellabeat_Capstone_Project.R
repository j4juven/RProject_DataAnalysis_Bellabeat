### Installing packages and load it ###

install.packages("tidyverse")
library(tidyverse)
install.packages("here")
library(here)
install.packages("skimr")
library(skimr)
install.packages("janitor")
library(janitor)
install.packages("dplyr")
library(dplyr)
### Importing datasets ###

daily_activity <- read_csv("Desktop/Google Capstone Project/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
daily_calories <- read_csv("Desktop/Google Capstone Project/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
daily_intensities <- read_csv("Desktop/Google Capstone Project/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
daily_steps <- read_csv("Desktop/Google Capstone Project/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
heartrate_seconds <- read_csv("Desktop/Google Capstone Project/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
sleep_day <- read_csv("Desktop/Google Capstone Project/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight_log_info <- read_csv("Desktop/Google Capstone Project/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

### Checking hourly activities ###
hourly_calories <- read_csv("Desktop/Google Capstone Project/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
hourly_intensities <- read_csv("Desktop/Google Capstone Project/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
hourly_steps <- read_csv("Desktop/Google Capstone Project/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")

### Merging dataframes ###
hourly_activity1 <- merge(x = hourly_calories, y = hourly_intensities, by = c("Id", "ActivityHour"))
hourly_activity <- merge(x= hourly_activity1, y = hourly_steps, by = c("Id", "ActivityHour"))

### Remove dataframes from environment ###
rm(hourly_calories, hourly_intensities, hourly_steps)

### Formating correct date ###
library(lubridate)
daily_activity$ActivityDate <- as.POSIXct(daily_activity$ActivityDate, format = "%m/ %d/ %Y")
daily_calories$ActivityDay <- as.POSIXct(daily_calories$ActivityDay, format = "%m/ %d/ %Y")
daily_intensities$ActivityDay <- as.POSIXct(daily_intensities$ActivityDay, format = "%m/ %d/ %Y")
daily_steps$ActivityDay <- as.POSIXct(daily_steps$ActivityDay, format = "%m/ %d/ %Y")
heartrate_seconds$Time <- strptime(heartrate_seconds$Time, format = "%m/ %d/ %Y %I:%M:%S %p")
hourly_activity$ActivityHour <- strptime(hourly_activity$ActivityHour, format = "%m/ %d/ %Y %I:%M:%S %p")
sleep_day$SleepDay <- as.POSIXct(sleep_day$SleepDay, format = "%m/ %d/ %Y %I:%M:%S %p")
weight_log_info$Date <- strptime(weight_log_info$Date, format = "%m/ %d/ %Y %I:%M:%S %p")

### look at all the dataset once ###
glimpse(daily_activity)
glimpse(daily_calories)
glimpse(daily_intensities)
glimpse(daily_steps)
glimpse(heartrate_seconds)
glimpse(hourly_activity)
glimpse(sleep_day)
glimpse(weight_log_info)

### Consistency check ###
n_distinct(daily_activity$Id)
n_distinct(daily_calories$Id)
n_distinct(daily_intensities$Id)
n_distinct(daily_steps$Id)
n_distinct(heartrate_seconds$Id)
n_distinct(hourly_activity$Id)
n_distinct(sleep_day$Id)
n_distinct(weight_log_info$Id)

### drop weight_log_info, since only 8 unique ID ##

### check for duplicate entries ###
sum(duplicated(daily_activity))
sum(duplicated(daily_calories))
sum(duplicated(daily_intensities))
sum(duplicated(daily_steps))
sum(duplicated(heartrate_seconds))
sum(duplicated(hourly_activity))
sum(duplicated(sleep_day))

### remove all duplicated entries ###
library(dplyr)
sleep_day_unique <- distinct(sleep_day)
sum(duplicated(sleep_day_unique))

### trying to find missing values in datasets ###
sum(is.na(daily_activity))
sum(is.na(daily_calories))
sum(is.na(daily_intensities))
sum(is.na(daily_steps))
sum(is.na(heartrate_seconds))
sum(is.na(hourly_activity))
sum(is.na(sleep_day_unique))

### Now comes Data Transformation ###
library(dplyr)
library(lubridate)
### finding days for each date ###
daily_activity <- transform(daily_activity, Days_Weeks = weekdays(ActivityDate))
daily_calories <- transform(daily_calories, Days_Weeks = weekdays(ActivityDay))
daily_intensities <- transform(daily_intensities, Days_Weeks = weekdays(ActivityDay))
daily_steps <- transform(daily_steps, Days_Weeks = weekdays(ActivityDay))
sleep_day_unique <- transform(sleep_day_unique, Days_Weeks = weekdays(SleepDay))
hourly_activity <- transform(hourly_activity, Days_Weeks = weekdays(ActivityHour))

### for heartrate_seconds, I need to convert in Days as data was in seconds ###
### each unique Id will have several days ###
heartrate_seconds_final <- heartrate_seconds %>% 
  mutate(date_col = date(Time)) %>% 
  group_by(Id, date_col) %>% 
  summarize(Value = sum(Value)) 

### find days for each date in heartrate_seconds_final ###
heartrate_seconds_final <- transform(heartrate_seconds_final, Days_Weeks = weekdays(date_col))

### merging several datasets in order to analyze properly ###
steps_and_calories <- merge(daily_calories, daily_steps, by = c("Id", "ActivityDay"))

steps_and_calories_final <- steps_and_calories %>% 
  select(-Days_Weeks.y) %>% 
  rename(days_weeks = Days_Weeks.x)
  

rename_with(steps_and_calories_final, tolower)

intensities_and_calories <- merge(daily_calories, daily_intensities, by = c("Id", "ActivityDay"))

intensities_and_calories_final <- intensities_and_calories %>% 
  select(-Days_Weeks.y) %>% 
  rename(days_weeks = Days_Weeks.x)

### In order to merge data with sleep and calories, we need to rename ActivityDay and SleepDay colums into days ###
daily_calories_1 <- daily_calories %>% 
  rename(days = ActivityDay)

sleep_day_unique_1 <- sleep_day_unique %>% 
  rename(days = SleepDay)

sleep_and_calories <- merge(daily_calories_1, sleep_day_unique_1, by = c("Id", "days"))

sleep_and_calories_final <- sleep_and_calories

sleep_and_calories_final <- sleep_and_calories_final %>% 
  select( - Days_Weeks.y)
sleep_and_calories_final <- sleep_and_calories_final %>% 
  rename(days_weeks = Days_Weeks.x)

hourly_activity_final <- hourly_activity  

daily_activity_final <- daily_activity


### Now we have the final datasets for visualization and for proper analyzing ###
daily_activity ### since it has already given metrics like calories and other ###
steps_and_calories_final
intensities_and_calories_final
hourly_activity_final
sleep_and_calories_final ### total unique Id is 24 because it only took those Id which were present in two datasets ###
heartrate_seconds_final

### final time checking if any missing values are present in final datasets ###
sum(is.na(daily_activity_final))
sum(is.na(steps_and_calories_final))
sum(is.na(intensities_and_calories_final))
sum(is.na(hourly_activity_final))
sum(is.na(sleep_and_calories_final))
sum(is.na(heartrate_seconds_final))

### final time checking for unique Id's in all datasets ###
n_distinct(daily_activity_final$Id)
n_distinct(steps_and_calories_final$Id)
n_distinct(intensities_and_calories_final$Id)
n_distinct(hourly_activity_final$Id)
n_distinct(sleep_and_calories_final$Id)
n_distinct(heartrate_seconds_final$Id)

### Share with visualization ###
### first visualization Showing days with activities and usage ###
ggplot(data = daily_activity_final) +
  geom_bar(mapping = aes (x = Days_Weeks, fill = Days_Weeks,)) +
  labs(title = " Days with Activities and Usage", caption = "Fitbit Fitness Tracker Data by Mobius")


### second visualtization between Total Steps and Calories ###
ggplot(data = steps_and_calories_final) +
  geom_point(mapping = aes(x = Calories, y = StepTotal, alpha = Calories), color = "dark green") +
  geom_smooth(mapping = aes(x = Calories, y = StepTotal), color = "maroon") +
  labs(title = " Total Steps vs Calories", caption = "Fitbit Fitness Tracker Data by Mobius")

### third visualization between Very Active Minutes and Calories ###
ggplot(data = intensities_and_calories_final) +
  geom_point(mapping = aes(x = Calories, y = VeryActiveMinutes, alpha = Calories), color = "black") +
  geom_smooth(mapping = aes(Calories, y = VeryActiveMinutes), color = "maroon") + 
  labs(title = " Very Active Minutes vs Calories", caption = "Fitbit Fitness Tracker Data by Mobius")

### fouth visualization between Very Active Distance and Calories with facet_wrap ###
ggplot(data = intensities_and_calories_final) +
  geom_point(mapping = aes(x = Calories, y = VeryActiveDistance, alpha = Calories), color = "maroon") +
  geom_smooth(mapping = aes(Calories, y = VeryActiveDistance), color = "blue") +
  facet_wrap(~days_weeks) +
  labs(title = " Very Active Distance vs Calories", caption = "Fitbit Fitness Tracker Data by Mobius")

### fifth visualization among Very Active Distance and Very Active Minutes ###
ggplot(data = intensities_and_calories_final) +
  geom_smooth( mapping = aes(x = VeryActiveDistance, y = VeryActiveMinutes, alpha = VeryActiveDistance), color = "navy blue") +
  labs(title = " Very Active Distance vs Very Active Minutes", caption = "Fitbit Fitness Tracker Data by Mobius")

### sixth visualization showing daily intensities
ggplot(data = hourly_activity_final) +
  geom_point(mapping = aes(x = Calories, y = AverageIntensity), color = "purple") +
  geom_smooth(mapping = aes(x = Calories, y = AverageIntensity), color = "black") +
  facet_grid(~Days_Weeks) +
  labs(title = " Hourly Average Intersity vs Hourly Calories", caption = "Fitbit Fitness Tracker Data by Mobius")

### seventh visualization showing relationship between total minutes asleep and calories ###
ggplot(data = sleep_and_calories_final) +
  geom_point(mapping = aes(x = Calories, y = TotalMinutesAsleep), color = " navy blue") +
  geom_smooth(mapping = aes(x = Calories, y = TotalMinutesAsleep), color = "maroon") +
  labs(title = " Total Minutes Asleep vs Calories", caption = "Fitbit Fitness Tracker Data by Mobius")

### eight visualization to check relationship between total minutes asleep and total time in bed ###
ggplot(data = sleep_and_calories_final) +
  geom_point(mapping = aes(x = TotalTimeInBed, y = TotalMinutesAsleep), color = " orange") +
  geom_smooth(mapping = aes(x = TotalTimeInBed, y = TotalMinutesAsleep), color = "blue") +
  labs(title = " Total Minutes Asleep vs Total Time In Bed", caption = "Fitbit Fitness Tracker Data by Mobius")

### last visualization among hearrate_seconds ### 
ggplot(data = heartrate_seconds_final) +
  geom_bar(mapping = aes(x = Days_Weeks, fill = Days_Weeks)) +
  labs(title = "Heart rate Values per Day", caption = "Fitbit Fitness Tracker Data by Mobius")


getwd()
