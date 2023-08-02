library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)

June_2022 <- read.csv("C:/Users/angad/Downloads/Capstone Project Data Analysis/Original Data/Jun_2022_Trip_Data.csv")
July_2022 <- read.csv("C:/Users/angad/Downloads/Capstone Project Data Analysis/Original Data/Jul_2022_Trip_Data.csv")
August_2022 <- read.csv("C:/Users/angad/Downloads/Capstone Project Data Analysis/Original Data/Aug_2022_Trip_Data.csv")
September_2022 <- read.csv("C:/Users/angad/Downloads/Capstone Project Data Analysis/Original Data/Sep_2022_Trip_Data.csv")
October_2022 <- read.csv("C:/Users/angad/Downloads/Capstone Project Data Analysis/Original Data/Oct_2022_Trip_Data.csv")
November_2022 <- read.csv("C:/Users/angad/Downloads/Capstone Project Data Analysis/Original Data/Nov_2022_Trip_Data.csv")
December_2022 <- read.csv("C:/Users/angad/Downloads/Capstone Project Data Analysis/Original Data/Dec_2022_Trip_Data.csv")
January_2023 <- read.csv("C:/Users/angad/Downloads/Capstone Project Data Analysis/Original Data/Jan_2023_Trip_Data.csv")
February_2023 <- read.csv("C:/Users/angad/Downloads/Capstone Project Data Analysis/Original Data/Feb_2023_Trip_Data.csv")
March_2023 <- read.csv("C:/Users/angad/Downloads/Capstone Project Data Analysis/Original Data/Mar_2023_Trip_Data.csv")
April_2023 <- read.csv("C:/Users/angad/Downloads/Capstone Project Data Analysis/Original Data/Apr_2023_Trip_Data.csv")
May_2023 <- read.csv("C:/Users/angad/Downloads/Capstone Project Data Analysis/Original Data/May_2023_Trip_Data.csv")

# To check consistency of the datasets

colnames(June_2022)
colnames(July_2022)
colnames(August_2022)
colnames(September_2022)
colnames(October_2022)
colnames(November_2022)
colnames(December_2022)
colnames(January_2023)
colnames(February_2023)
colnames(March_2023)
colnames(April_2023)
colnames(May_2023)


str(June_2022)
str(July_2022)
str(August_2022)
str(September_2022)
str(October_2022)
str(November_2022)
str(December_2022)
str(January_2023)
str(February_2023)
str(March_2023)
str(April_2023)
str(May_2023)

# Combining all dataframes

all_trips <- bind_rows(June_2022,July_2022,August_2022,September_2022,October_2022,November_2022,December_2022,January_2023,February_2023,March_2023,April_2023,May_2023)

# Inspecting combined dataframe

colnames(all_trips)
nrow(all_trips)
head(all_trips)
summary(all_trips)

# To Check all unique values inside the column

table(all_trips$member_casual)

# Splitting for easy aggregation

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# New Ride Duration Column

all_trips$ride_duration <- difftime(all_trips$ended_at,all_trips$started_at)

all_trips$ride_duration <- as.numeric(all_trips$ride_duration)

is.numeric(all_trips$ride_duration)

# The dataframe includes a few hundred entries where ride_duration is negative
# We will create a new version of the dataframe (v2) since data is being removed

all_trips_v2 <- all_trips[!(all_trips$ride_duration < 0),]


# Descriptive analysis on ride_duration
summary(all_trips_v2$ride_duration)

# Compare members and casual users
all_trips_v2 %>%
  group_by(member_casual) %>%
  summarise(mean(ride_duration),max(ride_duration),min(ride_duration))


all_trips_v2 %>%
  group_by(member_casual,day_of_week) %>%
  summarise(mean(ride_duration))

# Fixing the unordered days of the week
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

all_trips_v2 %>%
  group_by(member_casual,day_of_week) %>%
  summarise(mean(ride_duration))

# Comparing usage during each month
all_trips_v2 %>%
  group_by(month) %>%
  summarise(Members = sum(member_casual == "member"), 
            Casuals = sum(member_casual == "casual")
  )

# Comparing bike types
all_trips_v2 %>%
  group_by(member_casual) %>%
  summarise(Electric = sum(rideable_type == "electric_bike"),
            Classic = sum(rideable_type == "classic_bike"), 
            Docked = sum(rideable_type == "docked_bike"),  
            Classic_Percentage = paste0(round(Classic / nrow(all_trips_v2) * 100, 2), "%"),
            Ebike_Percentage = paste0(round(Electric / nrow(all_trips_v2) * 100, 2), "%"),
            Docked_Percentage = paste0(round(Docked / nrow(all_trips_v2) * 100, 2), "%")
  )		

