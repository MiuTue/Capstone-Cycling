library(tidyverse)
library(conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")


colnames(q1_2019)
colnames(q1_2020)

# Rename columns to make them consistent with q1_2020 (as this will be the supposed going-forward table design for Divvy)
(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
))

# Inspect the dataframes and look for incongruencies
str(q1_2019)
str(q1_2020)

# Convert ride_id and rideable_type to character so that they can stack correctly
q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))


# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q1_2019, q1_2020)#, q3_2019)#, q4_2019, q1_2020)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "tripduration"))

# Inspect the new table that has been created
colnames(all_trips) #List of column names
nrow(all_trips) #How many rows are in data frame?
dim(all_trips) #Dimensions of the data frame?
head(all_trips) #See the first 6 rows of data frame. Also tail(all_trips)
str(all_trips) #See list of columns and data types (numeric, character, etc)
summary(all_trips) #Statistical summary of data. Mainly for numerics

library(dplyr)

# Consolidating labels
all_trips <- all_trips %>%
  mutate(member_casual = case_when(
    member_casual %in% c("member", "Subscriber") ~ "Member",
    member_casual %in% c("Customer", "casual") ~ "Casual",
    TRUE ~ member_casual
  ))

library(lubridate)

# Assuming your date column is named "date"
all_trips <- all_trips %>%
  mutate(date = as.numeric(difftime(ended_at, started_at, units = "hours")),  # Calculate duration in hours
         hours = floor(date),  # Extract hours
         minutes = floor(60 * (date - hours)),  # Extract minutes
         seconds = round(3600 * (date - hours - minutes/60)),  # Extract seconds
         ride_length = sprintf("%02d:%02d:%02d", hours, minutes, seconds)) %>%  # Format as HH:MM:SS
  select(-c(date, hours, minutes, seconds))

all_trips <- all_trips %>%
  filter(ride_length >= 0)
#Convert Subscriber to member, customer to casual
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

all_trips <- all_trips %>%
  mutate(ride_date = as.Date(ended_at),  # Extract date of each ride
         ride_month = month(ride_date),  # Extract month of each ride
         ride_day = day(ride_date),      # Extract day of the month of each ride
         ride_year = year(ride_date))
#format column
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks andchecked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0), ]


