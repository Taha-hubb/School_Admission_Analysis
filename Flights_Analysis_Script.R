# Install packages ----
#install.packages("readr") # Install packages "it's like buying the packages"
# install.packages(c("here","dplyr","ggplot2")) # Install several packages

# Load packages it's like open the packages ----

library(readr) # for importing data
library(here)  
library(ggplot2) # for data visualisation
library(dplyr) # for manipulating data

# Import the data ----

# Import the data using Here function "I don't know why he use it" and store it in variables
flights <- read_csv(here("data/raw/flights.csv"))
airports <- read_csv(here("data/raw/airports.csv"))
airlines <- read_csv(here("data/raw/airlines.csv"))
planes <- read_csv(here("data/raw/planes.csv"))
weather <- read_csv(here("data/raw/weather.csv"))


# View my data ----
View(read_csv("data/raw/flights.csv")) 

# Basic data exploration ----
glimpse(flights) # Gives you a detailled overview of the dataset (inside the deplyr package)
summary(flights) # it gives you some statistics about your dataset

# Data manipulation with the dplyr package ----

# > filter: for filtering the data ----

# flights to Dallas
filter(flights, dest == "DFW")

# Flights that lasted more than 1 hour and 30 minutes
filter(flights, air_time > 90)

# Planes with less than 10 seats 
filter(planes, seats <= 10)

# Planes with more than 100 seats built by PIPER
filter(planes, seats > 100, manufacturer == "PIPER")

# All summer flights (between June and August) that departed from JFK and that lasted at least 3 hours.
filter(flights, month >= 6, month <= 8, origin == "JFK", air_time >= 120)

# All flights operated by UA or AA
filter(flights, carrier == "UA", carrier == "AA") # This is wrong! No flight can be operated by two carriers at the same time

# We need to use the OR operator: | or the IN operator: %in%
filter(flights, carrier == "UA" | carrier == "AA")
filter(flights, carrier %in% c("UA", "AA"))

# All summer flights (between June and August) that departed from JFK and that lasted at least 3 hours (with the %in% operator).
filter(flights, month %in% c(6, 7, 8), origin == "JFK", air_time >= 180)

# Weather conditions around Newark or LaGuardia in the winter months (december to february) 
# when the temperature is below 40 degree and the wind speed is at least 15 miles per hour
filter(weather, origin %in% c("EWR","LGA"), month %in% c(12,1,2), temp < 40, wind_speed >= 15)
filter(weather, origin == "EWR" | origin == "LGA", month == 1 | month == 2 | month == 12, temp < 40, wind_speed >= 15)

# The AND operator is: &

# All airports with an altitude NOT greater 100
filter(airports, !alt > 100)

# All flights that did not depart from either JFK or EWR
View(filter(flights, !origin %in% c("JFK", "EWR")))

# > arrange: for rearranging data ----------

# Reorder the dataset from the smallest to highest number of seats
arrange(planes, desc(seats))

# Reorder the dataset from the highest to smallest number of seats
planes_seats <- arrange(planes, desc(seats))
arrange(planes, -(seats)) # the same result with a different method "the minus method doesn't work with a text"
View(planes_seats)

# Reorder the airlines dataset by carrier code in alphabetical order
arrange(airlines, carrier)

# Reorder the flights dataset by scheduled departure time in chronological order
arrange(flights, sched_dep_time)
# > filter + arrange ----

# Which flights operated by United Airlines (UA) in the month of December arrived in Dallas the earliest?
ua_dfw_12 <- filter(flights, carrier == "UA", month == 12, dest == "DFW")
arrange(ua_dfw_12, arr_time)

# Which carrier between AA and DL had the slowest flight departing from JFK on March 31?
ua_aa_jfk <- filter(flights, carrier %in% c("AA", "UA"), origin == "JFK", month == 3, day == 31)
arrange(ua_aa_jfk, desc(air_time))

# What is the highest wind speed around LaGuardia (LGA) in the first week of September when the temperature is above 75 degrees?
lga_9 <- filter(weather, origin == "LGA", month == 9, day <= 7, temp > 75)
arrange(lga_9, desc(wind_speed))

# What is the highest wind speed around Laguardiya (LGA) in the first week of september when the temperature is above 75 degrees?
filter(weather, origine == "LGA")
# > mutate: for creating new data -------

# Convert air_time in the flights dataset in hours
mutate(flights, air_time = air_time/60)

# Convert air_time in the flights dataset in hours and put it in a new column
mutate(flights, air_time_hours = air_time/60)

# Convert the distance from miles to kilometers
mutate(flights, distance = distance * 1.60934)
mutate(flights, distance_Km = distance * 1.60934)

# Find the delay time "you need to store it in a variable in order to save your mutation"
flights2 <- mutate(flights, dep_delay = as.numeric(dep_time - sched_dep_time) / 60) # as.numeric convert the time to a numeric value

# Compute several delay metrics
mutate(flights,
       dep_delay = as.numeric(dep_time - sched_dep_time) / 60,
       arr_delay = as.numeric(arr_time - sched_arr_time) / 60,
       sched_air_time = as.numeric(sched_arr_time - sched_dep_time) / 60,
       air_time_made_up = sched_air_time - air_time
  )
# > filter + arrange + mutate -------

# which flight to LAX operated by American  Airlines had the largest departure delay in the month of May ? 
AA4 <- filter(flights, carrier == "AA",dest == "LAX", month == 5)
AA4_mutated <- mutate(AA4, dep_delay = as.numeric(dep_time - sched_dep_time) / 60)
arrange(AA4_mutated, desc(dep_delay))

# Which flight departing from LGA and operated by UA in the first half of the year was the shortest in hours
LGA_UA <- filter(flights, origin == "LGA", carrier == "UA", month <= 6)
LGA_UA_HOURS <- mutate(LGA_UA, air_time = air_time / 60)
arrange(LGA_UA_HOURS, air_time)

# Which flight departing from Newark had the largest total delay? (total delay = departure delay + arrival delay)
arrange(mutate(filter(flights, origin == "EWR"), total_delay = as.numeric(dep_time - sched_dep_time) / 60 + as.numeric(arr_time - sched_arr_time) / 60) , -total_delay) # I made everything in one line



# ~ homework_3_day_7 -------

# Which flight operated by Delta (DL) and departing from JFK in the last quarter of 2013 was completed
# with the highest average speed in miles per hour? (Hint: the distance column is already in miles, but 
#the air_time column is in minutes)
dl_jfk9 <- filter(flights, carrier == "DL", origin == "JFK", month >= 10)
mutate(dl_jfk9, air_timeHour = air_time / 60, avg_speed = distance / air_timeHour) # first method
dl_jdk9_mutated <- mutate(dl_jfk9 , avg_speed = distance / (air_time / 60)) # second method
arrange(dl_jdk9_mutated, -avg_speed)

# Was the highest departure delay in the dataset out of LaGuardia (LGA)?
lga <- filter(flights, origin == "LGA")
lga_mutated <- mutate(lga, dep_delay = as.numeric(dep_time - sched_dep_time) / 60)
arrange(lga_mutated, -dep_delay)

# What was the destination of the flight with the highest arrival delay?
flights3 <- mutate(flights, arr_delay = as.numeric(arr_time - sched_arr_time) / 3600)
arrange(flights3, -arr_delay)

# Which flights departing from Newark in May had a destination within a 500-kilometer radius and a bb departure delay of less than 0.25 hour
dst_Km <- mutate(flights, distance_Km = distance * 1.60934, dep_delay = as.numeric(dep_time - sched_dep_time)/3600)
filter(dst_Km, origin == "EWR", month == 5, distance_Km <= 500, dep_delay < 0.25)

# Which airports have an altitude of at least a kilometer above sea level? (Hint: the alt column is in feet
airports_mut1 <- mutate(airports, alt_Km = alt * 0.0003048)
filter(airports_mut1, alt_Km >= 1)

# Which planes were at most 15 years old in 2013?
planes_mut1 <- mutate(planes, planes_age = 2013-year )
filter(planes_mut1, planes_age <= 15 )

#Perform the following tasks on the weather dataset and create new columns. Perform all tasks with only one call to mutate().
#• convert the wind speed (wind_speed) and gust speed (wind_gust) to kilometers per hour. (Hint: the values are in miles per hour)
#• convert precipitation (precip) to millimeters. (Hint: the values are in inches)
#• convert visibility (visib) to meters. (Hint: the values are in miles)
#• convert temperature (temp) and dewpoint (dewp) to centigrades. (Hint: the values are in fahrenheit)
#• convert sea level pressure (pressure) to pascals. (Hint: the values are in millibars)

mutate(weather, wind_speed_km = wind_speed *1.60934
              , wind_gust_km = wind_gust *1.60934
              , precip_mm = precip * 25.4
              , visib_meters = visib * 1609.344
              , temp_c = (temp - 32) / 1.8
              , dewp_c = (dewp - 32) / 1.8
              , pressure_pa = pressure * 100)

# Perform the following tasks on the flights dataset and create new columns. Perform all tasks with only one call to mutate().
# • convert air_time to hours. (Hint: the values are in minutes)
# • convert distance to kilometers. (Hint: the values are in miles)
# • compute departure delay in minutes.
# • compute arrival delay in minutes.
# • compute scheduled air time in minutes.
# • compute air time made up in minutes.
# • compute average speed in kilometers per hour.
# • compute average speed in miles per hour.
mutate(flights, air_time_hours = air_time / 60
              , distance_km = distance * 1.609344
              , dep_delay = as.numeric(dep_time - sched_dep_time) / 60
              , arr_delay = as.numeric(arr_time - sched_arr_time) / 60
              , sched_air_time = as.numeric(sched_arr_time - sched_dep_time) / 60
              , air_time_made_up = sched_air_time - air_time
              , avg_speed_kmh = distance_km / air_time_hours
              , avg_speed_mph = distance / air_time_hours)







# ~ The pipe operator: %>% --------
# this operator help us to get rid of variables created in the process of searching an answer to some question, we will take the previous homework questions and answer it with the pipe operator

# Which flight operated by Delta (DL) and departing from JFK in the last quarter of 2013 was completed with the highest average speed in miles per hour? (Hint: the distance column is already in miles, but the air_time column is in minutes)
dl_jfk9 <- filter(flights, carrier == "DL", origin == "JFK", month >= 10)
dl_jdk9_mutated <- mutate(dl_jfk9 , avg_speed = distance / (air_time / 60)) # second method
arrange(dl_jdk9_mutated, -avg_speed)
# using the pipe operator
flights %>% 
  filter(carrier == "DL", origin == "JFK", month >= 10) %>%
  mutate(avg_speed = distance / (air_time / 60)) %>%
  arrange(-avg_speed)
   
# Was the highest departure delay in the dataset out of LaGuardia (LGA)?
lga <- filter(flights, origin == "LGA")
lga_mutated <- mutate(lga, dep_delay = as.numeric(dep_time - sched_dep_time) / 60)
arrange(lga_mutated, -dep_delay)
# Using the pipe operator and store the final result in a variable called lga

lga <- 
  flights %>%
  filter(origin == "LGA") %>%
  mutate(dep_delay = as.numeric(dep_time - sched_dep_time) / 60) %>%
  arrange(-dep_delay)

# What was the destination of the flight with the highest arrival delay?
flights3 <- mutate(flights, arr_delay = as.numeric(arr_time - sched_arr_time) / 3600)
arrange(flights3, -arr_delay)
# using the pipe operator
flights %>%
  mutate(arr_delay = as.numeric(arr_time - sched_arr_time) / 3600) %>%
  arrange(-arr_delay)
  

# ~ a few shortcuts in R -------
# Ctrl/Cmd + Enter: Run code and move pointer to the next line
# Alt + Enter: Run code and leave pointer on the same line
# Ctrl/Cmd + L: Clear console
# Ctrl/Cmd + Shift + C: Comment/uncomment text
# Ctrl/Cmd + I: Format code
# Ctrl/Cmd + Shift + R: New code section
# Ctrl + 1: Move to script
# Ctrl + 2: Move to console
# Ctrl/Cmd + Shift + M: pipe operator (%>%)

# > summarize: for summarizing data -----

# Compute the total number of seats of all planes
planes %>%
  summarize(total_seats = sum(seats))

# Average temperature in NYC (all 3 weather stations)
weather %>%
  summarize(avg_temp = mean(temp, na.rm = TRUE))

# Highest humidity level in NYC (all 3 weather stations)
weather %>%
  summarize(max_humid = max(humid, na.rm = TRUE))

# Lowest and highest airports and the height between them
airports %>%
  summarize(max_alt = max(alt, na.rm = TRUE),
            min_alt = min(alt, na.rm = TRUE)) %>%
  mutate(height = max_alt - min_alt,
         height_m = height * 0.3048)

# Useful summary functions: sum(), prod(), min(), max(), mean(), median(), var(), sd()


# > group_by: for grouping variables ----

# Group planes by manufacturer
planes %>%
  group_by(manufacturer)

# Group weather by origin
weather %>%
  group_by(origin)


# > group_by + summarize: for computing grouped summaries ----

# Average temperature for each weather station
weather %>%
  filter(origin == "EWR") %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE))

weather %>%
  filter(origin == "LGA") %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE))

weather %>%
  filter(origin == "JFK") %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE))

weather %>%
  group_by(origin) %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE)) %>%
  arrange(desc(mean_temp))

# Highest, lowest and average wind speed in each month
weather %>%
  group_by(month) %>%
  summarize(max_wind_speed = max(wind_speed, na.rm = TRUE),
            min_wind_speed = min(wind_speed, na.rm = TRUE),
            mean_wind_speed = mean(wind_speed, na.rm = TRUE)) %>%
  arrange(mean_wind_speed)

# Highest, lowest and average monthly wind speed in each weather station
weather %>%
  group_by(month, origin) %>%
  summarize(max_wind_speed = max(wind_speed, na.rm = TRUE),
            min_wind_speed = min(wind_speed, na.rm = TRUE),
            mean_wind_speed = mean(wind_speed, na.rm = TRUE))

# Number of weather recordings for each weather station
weather %>%
  group_by(origin) %>%
  summarize(count = n())

# How many planes did each manufacturer make in the 1990s and 2000s?
planes %>%
  filter(year >= 1990, year < 2010) %>%
  mutate(decade = ifelse(year < 2000, "1990s", "2000s")) %>%
  group_by(decade, manufacturer) %>%
  summarize(plane_count = n())

# Compute the average speed for each carrier for each destination
flights %>%
  mutate(avg_speed = distance / (air_time/60)) %>%
  group_by(carrier, dest) %>%
  summarize(avg_speed = mean(avg_speed, na.rm = TRUE))

# Which carrier operated the most flights out of EWR
flights %>%
  filter(origin == "EWR") %>%
  group_by(carrier) %>%
  summarize(n_flights = n()) %>%
  arrange(desc(n_flights)) %>%
  mutate(pct_flights = (n_flights / sum(n_flights)) * 100)

# ~ homework_4_day_9 --------

# a- What is the time zone of:
# • the airport of highest altitude?
airports %>%
  arrange(desc(alt)) 

# • the airport of lowest altitude?
airports %>%
  arrange(alt)

# b- Compute the average airport altitude for each time zone, then determine:
# • the time zone which contains the highest average airport altitude?
# • the time zone which contains the lowest average airport altitude?
tz_avg_alt <- airports %>%
  group_by(tzone) %>%
  summarize(avg_alt = mean(alt, na.rm = TRUE))

# Time zone with the highest average altitude
tz_avg_alt %>%
  arrange(desc(avg_alt)) 

# Time zone with the lowest average altitude
tz_avg_alt %>%
  arrange(avg_alt)

# c- Compute the number of airports in each time zone, then find out:
# • the time zone with the most airports
# • the time zone with the least airports

# Compute the number of airports in each time zone
tz_airport_count <- airports %>%
  group_by(tzone) %>%
  summarize(count = n())
# Time zone with the most airports
tz_airport_count %>%
  arrange(desc(count))

# d- For each time zone, compute the following altitude summary statistics: the maximum, the minimum, the
# median and the standard deviation (you are encouraged to read up on the concepts of standard deviation and
# variance). Determine which time zone has:
# • the highest airport altitude (does it correspond to your answer in question a?)
# • the lowest airport altitude (does it correspond to your answer in question a?)
# • the most mountainous terrain
# • the flattest terrain

# Compute summary statistics
alt_summarize <- airports %>% 
  group_by(tzone) %>% 
  summarize(max_alt = max(alt, na.rm = TRUE),
            min_alt = min(alt, na.rm = TRUE),
            avg_alt = mean(alt, na.rm = TRUE),
            standard_dev = var(alt, na.rm = TRUE)) 

# Time zone with highest airport altitude
alt_summarize %>% 
  arrange(desc(max_alt))

# Time zone with the lowest airport altitude
alt_summarize %>% 
  arrange(min_alt)

# The most mountainous terrain
alt_summarize %>%
  arrange(desc(standard_dev))

# The flattest terrain
alt_summarize %>%
  arrange(standard_dev)

# e.1 Add two columns to the weather dataset: one in which the temperature data is converted to centigrades
# and another one in which the wind speed data is converted to kilometers per hour.
# e.2 Choose the measurement units of your choice for this. What is the temperature threshold below which
# you consider it to be cold?1 What is the wind speed threshold above which you consider it to be windy?
# Create two new columns with the following specifications. Let the first column be called cold_status with
# the value "cold" if the temperature is below your temperature threshold and "not cold" otherwise. Let the
# second column be called windy_status with the value "windy" when the wind speed is above your threshold
# and "not windy" otherwise.
weather_extra <- weather %>% 
  mutate(temp_cent = (temp - 32) / 1.8,
         wind_speed_Km = wind_speed * 1.609344,
         cold_status = ifelse(temp_cent < 25, "cold", "not cold"),
         windy_status = ifelse(wind_speed_Km > 16, "windy", "not windy"))

# e.3 Which weather station has the lowest average humidity when it is windy?

weather_extra %>%
  filter(windy_status == "windy") %>% 
  group_by(origin) %>% 
  summarize(avg_humid = mean(humid, na.rm =TRUE)) %>%
    arrange(avg_humid)

# e.4 What is the most common combination of windy and cold statuses? (e.g. not cold and windy)
weather_extra %>% 
  group_by(cold_status,windy_status) %>% 
  summarize(count = n())

# e.5 Which weather station between JFK and EWR has the highest average wind gust in the summer months (from June to August)?
weather %>% 
  filter(origin == "JFK" | origin == "EWR", month <= 8, month >= 6) %>% 
  group_by(origin) %>% 
  summarize(avg_wind_gust = mean(wind_gust, na.rm = TRUE)) %>% 
  arrange(-avg_wind_gust)

# f. A very influential business guru recently moved to New York City and hired your services as a data scientist in order to help him/her make smart choices for his/her frequent trips.
# f.1 Perform the following tasks on the flights dataset and create new columns. Perform all tasks with only one call to mutate().
# • convert air_time to hours. (Hint: the values are in minutes)
# • convert distance to kilometers. (Hint: the values are in miles)
# • compute departure delay in minutes.
# • compute arrival delay in minutes.
# • compute scheduled air time in minutes.
# • compute air time made up in minutes.
# • compute average speed in kilometers per hour.
# • compute average speed in miles per hour.

flights_mut <- flights %>% 
               mutate(
         air_time_hours = air_time / 60
       , distance_km = distance * 1.609344
       , dep_delay = as.numeric(difftime(dep_time, sched_dep_time, units = "mins"))
       , arr_delay = as.numeric(difftime(arr_time, sched_arr_time, units = "mins"))
       , sched_air_time = as.numeric(difftime(sched_arr_time, sched_dep_time, units = "mins"))
       , air_time_made_up = sched_air_time - air_time
       , avg_speed_kmh = distance_km / air_time_hours
       , avg_speed_mph = distance / air_time_hours
       
       )

# f.2 Compute the average departure delay for each carrier. Which carrier is, on average, more reliable for flying out of New York City "JFK"?
flights_mut %>%
  filter(origin == "JFK") %>% 
  group_by(carrier) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  arrange(avg_dep_delay)
# f.3 Compute the average departure delay for each airport of origin. Which airport is, on average, the most efficient
flights_mut %>% 
  group_by(origin) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  arrange(avg_dep_delay)

# f.4 The business guru is planning to make several trips out of New York City in the months of March, June and September. He is planning several trips to Dallas (DFW) in March, to Atlanta (ATL) 
# in June and to Houston (IAH) in September. Compute the monthly average departure delay in each airport and recommend an airport or origin for his flights in each of these months

flights_bymonth <- flights_mut %>% 
  filter(month %in% c(3,6,9)) %>% 
  group_by(month,origin,dest) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE))

# trips to Dallas (DFW) in March "JFK is better"
flights_bymonth %>% 
  filter(month == 3, dest == "DFW") %>% 
  arrange(avg_dep_delay)

# trips Atlanta (ATL) in June "LGA is better"
flights_bymonth %>% 
  filter(month == 6, dest == "ATL") %>% 
  arrange(avg_dep_delay)

# trips to Houston (IAH) in September "LGA is better"
flights_bymonth %>% 
  filter(month == 9, dest == "IAH") %>% 
  arrange(avg_dep_delay)

# f.5 Compute the daily average departure delay. What was the longest departure delay in the year 2013? Make some research on the internet to find out the probable reason for that very long average delay.
flights_mut %>% 
  group_by(month,day) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  arrange(desc(avg_dep_delay))

# f.6 On average, which carriers fly the slowest to Houston (IAH) in February or April?
flights_mut %>% 
  filter(month %in% c(2,4), dest == "IAH") %>% 
  group_by(month, carrier) %>% 
  summarize(avg_speed = mean(avg_speed_kmh, na.rm = TRUE))

# f.7 Which carrier has pilots who care the most about making up time in their flights when there is a departure delay?  
flights_mut %>%
  filter(dep_delay > 0) %>%
  group_by(carrier) %>%
  summarize(avg_time_made_up = mean(air_time_made_up, na.rm = TRUE)) %>%
  arrange(desc(avg_time_made_up))

# > count: for count summaries ------

# How many flights were operated by each carrier
flights %>% 
  group_by(carrier) %>% 
  summarize(numb_flights = n()) %>% 
  arrange(desc(numb_flights))

# Another simple way to answer the above question
count(flights, carrier, sort = TRUE, name = "numb_flights")

# What is the most popular destination of flights operated by AA out of LGA "the answer is ORD"
flights %>% 
  filter(carrier == "AA", origin == "LGA") %>% 
  group_by(dest) %>%
  summarize(numb_flights = n()) %>%
  arrange(desc(numb_flights))

# The same result with count function
flights %>% 
  filter(carrier == "AA", origin == "LGA") %>% 
  count(dest,sort = TRUE, name = "numb_flights")

# Compute the flight count of each carrier out of each airport
flights %>% 
  group_by(carrier, origin) %>% 
  summarize(numb_flights = n())

# The same result with count function
flights %>% 
  count(carrier, origin, name = "numb_flights")

# What's the most popular destination in December
flights %>% 
  filter(month == 12) %>% 
  # group_by(dest) %>% 
  # summarize(numb_flights = n()) %>% 
  # arrange(desc(numb_flights))
  count(dest, sort = TRUE, name = "numb_flights")

# How many planes were manufactured by each manufacturer
planes %>% 
  count(manufacturer, sort = TRUE, name = "numb_plane")

# How many seats were made by each manufacturer
planes %>% 
  group_by(manufacturer) %>% 
  summarize(numb_seats = sum(seats, na.rm = TRUE)) %>% 
  arrange(-numb_seats)

# The same result using the count function
planes %>% 
  count(manufacturer, wt = seats, sort = TRUE, name = "numb_seats")

# How many miles flown by each carrier
flights %>% 
  # group_by(carrier) %>% 
  # summarize(miles_flown = sum(distance, na.rm = TRUE) ) %>% 
  # arrange(desc(miles_flown))
  count(carrier, wt = distance,sort = TRUE, name = "miles_flown" )

# > ungroup(): for ungrouping variables (opposite of group_by)------

# Compute the flight count and percentage of each carrier out of each airport
flights %>% 
  group_by(carrier, origin) %>% 
  summarize(numb_flights = n()) %>% 
  mutate(perct_flights = numb_flights / sum(numb_flights)* 100) %>% 
  arrange(carrier, desc(perct_flights))
# here the sum(numb_flights) will add up all the data in each carrier, so it will give us the 
# percentage per carrier and not per the whole data 

# Compute count and overall percentages of flights operated by each carrier out of each airports
flights %>% 
  group_by(carrier, origin) %>%
  summarize(numb_flights = n()) %>% 
  ungroup() %>% # to have percentage per the whole data
  mutate(percent_flights = numb_flights / sum(numb_flights)*100) %>% 
  arrange(-percent_flights)

  


# > homework_5_day_10 -----

flights_mut <- flights %>% 
  mutate(
    air_time_hours = air_time / 60
    , distance_km = distance * 1.609344
    , dep_delay = as.numeric(difftime(dep_time, sched_dep_time, units = "mins"))
    , arr_delay = as.numeric(difftime(arr_time, sched_arr_time, units = "mins"))
    , sched_air_time = as.numeric(difftime(sched_arr_time, sched_dep_time, units = "mins"))
    , air_time_made_up = sched_air_time - air_time
    , avg_speed_kmh = distance_km / air_time_hours
    , avg_speed_mph = distance / air_time_hours
    
  )
# b- Compute the number of flights operated by each airplane. (Hint: tailnum is the column that designates each airplane)
airplanes_count <- flights %>% 
  # group_by(tailnum) %>%
  # summarize(numb_flights = n()) %>%
  # arrange(-numb_flights)
  count(tailnum, name = "numb_flights")
  
# b.1- Which airplane was used the most for flights?
flights %>% 
  count(tailnum,sort = TRUE, name = "numb_flights")

# b.2- Which airplane was used the least for flights?
airplanes_count %>% 
  arrange(numb_flights)

# b.3- What are the top 10 most used airplanes?   
airplanes_count %>% 
  top_n(10) %>% 
  arrange(desc(numb_flights))

# c- Compute the number of flights operated by each carrier.
numbFlights_byCarrier <- flights %>% 
  # group_by(carrier) %>% 
  # summarize(numb_flights = n()) %>% 
  # arrange(-numb_flights) %>% 
  count(carrier, sort = TRUE, name = "numb_flights")

# c-1. Which carrier has operated the most flights?
numbFlights_byCarrier %>% 
  top_n(1)

# c-2. Which carrier has operated the least flights?
numbFlights_byCarrier %>% 
  top_n(-1)

# c-3. What are the bottom 10 carriers in terms of flight count?
numbFlights_byCarrier %>% 
  top_n(-10) %>% 
  arrange(numb_flights)

# d- Which of the 3 airports has the most departures?
flights %>% 
  group_by(origin) %>% 
  summarize(numb_depart = n()) %>% 
  arrange(-numb_depart)

# e- Which carrier has the most departures out of:
# e-1. JFK?  
flights %>% 
  filter(origin == "JFK") %>% 
  # group_by(carrier) %>% 
  # summarize(numb_deprtJFK = n()) %>% 
  count(carrier, name = "numb_deprtJFK") %>% 
  top_n(1)

# e-2. LGA?
flights %>% 
  filter(origin == "LGA") %>% 
  count(carrier, name = "numb_deprtJFK") %>% 
  top_n(1)

# e-3. EWR?
flights %>% 
  filter(origin == "EWR") %>% 
  count(carrier, name = "numb_deprtJFK") %>% 
  top_n(1)

# > select(): for selecting specific columns ----
# if you want to show specific columns from your data 
flights %>% 
  select(year, month, day)

# specify columns we do not want
flights %>% 
  select(-arr_time, -sched_arr_time, -month)

# specify a sequence of columns
flights %>% 
  select(day:carrier) # select all the columns from day to carrier

# 1:100 will give you the sequance of numbers from 1 to 100
1:100

# Discard a sequence of columns
flights %>% 
  select(-(day:carrier)) # discard all the columns from day to carrier and show the other columns

# Select columns that contains a specific word "string of characters"
flights %>% 
  select(contains("time"))

flights %>% 
  select(matches("time"))

# Select columns that contains a specific word AND another sequence of columns
flights %>% 
  select(contains("time"), year:day)

# Select columns that contains a specific word AND discard one columns even though it contains the string
flights %>% 
  select(contains("time"), -air_time)

# Select columns that START/END with a specific string of characters
flights %>% 
  select(starts_with("dep"))

flights %>% 
  select(ends_with("me"))

# Select some columns first then select everything else after them "Reorder the columns"
flights %>% 
  select(origin, dest,distance, everything())

# Select some columns first then select everything else after them except columns that contains "time
flights %>% 
  select(origin, dest, distance, everything(), -contains("time"))

# > case_when(): for more than one conditions----

# When we have one condition
flights %>% 
  select(distance) %>% 
  mutate(distance_label = ifelse(distance > 1000, "FAR", "CLOSE")) # if distance > 1000 write FAR, else write CLOSE

# When we have more than one conditions "Split flight distance into 4 categories"
distance_type <- flights %>% 
  select(distance) %>% 
  mutate(distance_label = case_when(
    distance <= 500 ~ "close",
    distance > 500 & distance <= 2000 ~ "fairly close",
    distance > 2000 & distance <= 5000 ~ "far",
    distance > 5000 ~ "very far"
  )) 

# let's say we want to count the number of flights by their closeness
distance_type %>% 
  # group_by(distance_label) %>% 
  # summarize(numb_flights = n())
  count(distance_label, name = "numb_flights")

# Label other things that you don't specify
weather %>% 
  select(temp) %>% 
  mutate(cold_level = case_when(
    temp < 30 ~ "very cold",
    temp >= 30 & temp < 50 ~ "cold",
    temp >= 50 & temp < 65 ~ "bearables",
    TRUE ~ "Others" # for everything else that you don't specify "above 65 degrees" 
    )) %>% 
  count(cold_level, sort = TRUE, name = "numb_times")

# Use if else function and case when

weather %>% 
  select(temp, wind_speed) %>% 
  mutate(
    cold_status = ifelse(temp < 50, "cold", "not cold"),
    wind_status = ifelse(wind_speed > 16, "Windy", "not windy"),
    state_of_life = case_when(
      cold_status == "not cold" & wind_status == "not windy" ~ "life's good",
      cold_status == "cold" & wind_status == "Windy" ~ "life's not good",
      TRUE ~ "It's Okaaay"
    )
    
  )


# > practicing joining data ------

# How many passengers traveled out of NYC in the year of 2013
left_join(flights, planes, by = "tailnum") %>% 
  select(tailnum, origin , seats) %>% 
  summarize(numb_passengers = sum(seats, na.rm = TRUE)) # you can use inner_join to get rid of na 

# Which carrier transported the most passengers in December ?
flights %>% 
  left_join(planes, by = "tailnum") %>% 
  filter(month == 12) %>% 
  select(carrier, month, seats) %>% 
  # group_by(carrier) %>% 
  # summarize(numb_pass_byCarrier = sum(seats, na.rm = TRUE)) %>% 
  # arrange(-numb_pass_byCarrier)
  count(carrier, wt = seats, sort = TRUE, name = "numb_pass_byCarrier" )

# Which planes were not used to fly people out of NYC ?
anti_join(planes, flights, by = "tailnum") # all the planes were used

# which flights used planes doesn't exists in the plan dataset "tailnumn exist in flights dataset and doesn't exist in planes dataset
# and how many planes there are ?
anti_join(flights, planes, by = "tailnum") %>% 
  select(carrier, tailnum) %>% 
  group_by(tailnum) %>% %>% 
  summarize(n())

# ~ homework_6_day_14 -----------

# a. Create new variables
# a-1- Perform the following tasks on the flights dataset and create new columns. Perform all tasks with
# only one call to mutate().
# • convert air_time to hours. (Hint: the values are in minutes)
# • convert distance to kilometers. (Hint: the values are in miles)
# • compute departure delay in minutes.
# • compute arrival delay in minutes.
# • compute scheduled air time in minutes.
# • compute air time made up in minutes.
# • compute average speed in kilometers per hour.
# • compute average speed in miles per hour.

flights_mut <- flights %>% 
  mutate(
      air_time_hours = air_time / 60
    , distance_km = distance * 1.609344
    , dep_delay = as.numeric(difftime(dep_time, sched_dep_time, units = "mins"))
    , arr_delay = as.numeric(difftime(arr_time, sched_arr_time, units = "mins"))
    , sched_air_time = as.numeric(difftime(sched_arr_time, sched_dep_time, units = "mins"))
    , air_time_made_up = sched_air_time - air_time
    , avg_speed_kmh = distance_km / air_time_hours
    , avg_speed_mph = distance / air_time_hours
  )

# a-2- Calculate the age of each plane in the planes dataset and add the data as a new column.
planes_mut <- planes %>% 
  mutate(planes_age = 2013 - year)

# b- Make a dataset of all planes, the carriers that own them, the planes’ manufacturers as well as the planes’
# ages in 2013. This will require you to merge the two datasets you created in a.1 and a.2. To make the
# task simpler, select the tailnum, age, manufacturer and seats columns from the planes data set and the
# tailnum and carrier columns from the flights dataset as you perform in the merging process (use can you
# the left_join() or right_join() functions). Also, don’t forget to use the distinct() function in order to
# avoid repetitions. Your resulting dataset should be similar to the following:
planes_carrier <- planes_mut %>% 
  left_join(flights_mut, by = "tailnum") %>% 
  select(tailnum, manufacturer, planes_age, seats, carrier)

# b.1- What is the number of rows of (i.e. the number of planes in) the data set you made in a.2? What about
# the data set you made in b? Technically, they should have the same number of rows; however, they do not.
# Can you explain why there is a difference?
planes_mut %>% 
  summarise(n())# 3322

planes_carrier %>% 
  summarize(numb_planes = n()) # 284170 "because there are planes that make more than one flight"

planes_carrier %>% 
  distinct(tailnum) %>% 
  summarize(numb_planes = n()) # 3322

# b.2- Which carrier owns the largest fleet (i.e. the most planes)? "DL"
planes_carrier %>%
  group_by(carrier) %>% 
  distinct(tailnum) %>% 
  summarize(numb_planes = n()) %>% 
  arrange(-numb_planes)

# b.3- Which carrier owned the newest planes on average in 2013?

planes_carrier %>% 
  group_by(carrier) %>% 
  distinct() %>% 
  summarize(avg_age = mean(planes_age, na.rm = TRUE)) %>% 
  arrange(avg_age)

# b.4- The Department of Aviation (I just made that up!) just made a decision that all planes older than 
# 20 years must stop flying for the safety of passengers. Which carrier will be hit the most by this decision
# (i.e. which carrier will lose the most planes)?
planes_carrier %>% 
  filter(planes_age >= 20) %>% 
  group_by(carrier) %>% 
  distinct() %>% 
  summarize(numb_old_planes = n()) %>% 
  arrange(desc(numb_old_planes))


# b.5- Now, let’s find out which carrier was hit the most financially. Let’s make the following assumption: each
# plane is valued at 15 bitcoins seat. Compute the bitcoin value of each plane, then compute the value in US
# dollars (you will need to look up the dollar value of a Bitcoin online). Which carrier was hit the most by the
# decision?  
planes_carrier %>% 
  filter(planes_age >= 20) %>% 
  mutate(value_bitcoin = seats *  15,
         value_dollar = value_bitcoin * 9244.74 ) %>% 
  group_by(carrier) %>% 
  distinct() %>% 
  summarize(financial_lose = sum(value_dollar)) %>% 
  arrange(desc(financial_lose))

# b.6- Using the dataset you made in b, create another dataset containing the number of planes that each
# manufacturer made for each carrier.
carrier_manufacturer <- planes_carrier %>% 
  group_by(manufacturer, carrier) %>%
  summarize(n_planes = n())

# b.7- Which manufacturer has the most clients (i.e. carriers)?
carrier_manufacturer %>% 
  group_by(manufacturer) %>% 
  summarise(numb_clients = n()) %>% 
  arrange(-numb_clients)

# b.8- Which carrier has the most diverse fleet (i.e. planes from several manufacturers)?
carrier_manufacturer %>% 
  group_by(carrier) %>% 
  summarise(numb_manufacturer = n()) %>% 
  top_n(1)

# c- Do weather conditions partially explain departure delays? Finding an answer to this question is our goal
# in this section. You can perform the tasks in c.1, c.2 and c.3 in the same chain of operations using the
# pipe operator (i.e. %>%).

# c.1- Create two new columns in the weather dataset for daily average temperatures and daily wind speed for
# each weather station (i.e. origin).
# c.2- Add a new column for categorizing the wind speeds with the following specifications:
# • strictly less than 5 MPH: slow wind
# • greater than or equal to 5 MPH and stricly less than 10 MPH: normal wind
# • greater than or equal to 10MPH and strictly less than 20 MPH: fast wind
# • greater than or equal to 20 MPH: very fast wind
# c.3- Add another column for categorizing average temperature with the following specifications:
# • strictly less than 40 degrees: very cold
# • greater than or equal to 40 degrees and strictly less than 55 degrees: cold
# • greater than or equal to 55 degrees and strictly less than 70 degrees: slightly cold
# • greater than or equal to 70 degrees and strictly less than 90 degrees: warm
# • greater than or equal to 90 degrees: hot
# c1
weather_ext <- weather %>% 
  group_by(origin, month, day) %>% 
  summarize(avg_daily_temp = mean(temp, na.rm = TRUE),
            avg_daily_wind = mean(wind_speed, na.rm = TRUE)) %>% 
# c2  
  mutate(wind_speed_status = case_when(
    avg_daily_wind < 5 ~ "slow wind",
    avg_daily_wind >= 5 & avg_daily_wind < 10 ~ "normal wind",
    avg_daily_wind >= 10 & avg_daily_wind < 20 ~ "fast wind",
    avg_daily_wind >= 20 ~ "very fast wind",
  )) %>% 
  
# c3
  mutate(temp_status = case_when(
    avg_daily_temp < 40 ~ "very cold",
    avg_daily_temp >= 40 & avg_daily_temp < 55 ~ "cold",
    avg_daily_temp >= 55 & avg_daily_temp < 70 ~ "slightly cold",
    avg_daily_temp >= 70 & avg_daily_temp < 90 ~ "warm",
    avg_daily_temp >= 90 ~ "hoooot"
    ))

# c.4- Merge the extended flights dataset (created in a.1) with the dataset you just created above. Select
# the month, day, dep_delay and origin columns from the extended flights dataset before the merging
# operation (use can you the left_join() or right_join() functions). 
flight_weather <- flights_mut %>% 
  select(month, day, dep_delay, origin) %>% 
  right_join(weather_ext, by = c("month", "day", "origin"))

# c.5- Compute the average departure delay for each temperature level. Is there any pattern? For example, do
# we experience more departure delay the colder/hotter it gets?

flight_weather %>% 
  group_by(temp_status) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  arrange(avg_dep_delay)

# c.6- Compute the average departure delay for each wind speed level. Is there any pattern? For example, do
# we experience more departure delay when the wind is faster?
flight_weather %>% 
  group_by(wind_speed_status) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  arrange(avg_dep_delay)

# d- You asked a company dealing in data collection to get you data on all airports in the US - they provided
# you with the airports dataset.

# d.1- You are curious to know which airports do not have direct flights to and from the New York City area.
# Use the anti_join() function, the extended flights dataset and the airports dataset to find them.

airports %>% 
  anti_join(flights_mut, by = c("faa" = "dest"))

  
  



# ~ Homework 6 worked again -------


# a1- create new columns in the flight and planes dataset 
flights_mut <- flights %>% 
  mutate(
    air_time_hours = air_time / 60
    , distance_km = distance * 1.609344
    , dep_delay = as.numeric(difftime(dep_time, sched_dep_time, units = "mins"))
    , arr_delay = as.numeric(difftime(arr_time, sched_arr_time, units = "mins"))
    , sched_air_time = as.numeric(difftime(sched_arr_time, sched_dep_time, units = "mins"))
    , air_time_made_up = sched_air_time - air_time
    , avg_speed_kmh = distance_km / air_time_hours
    , avg_speed_mph = distance / air_time_hours
  )

# store your data in a csv file
write_csv(flights_mut, path = here("data/prepped/flights_mut.csv"))

planes_ext <- planes %>% 
  mutate(age = 2013 - year)

write_csv(planes_ext, here("data/prepped/planes_extended.csv"))

# a2- Merge the flights and planes datasets keeping the columns of interests 

# b- Make a dataset of all planes, the carriers that own them, the planes’ manufacturers as well as the planes’
# ages in 2013. This will require you to merge the two datasets you created in a.1 and a.2. To make the
# task simpler, select the tailnum, age, manufacturer and seats columns from the planes data set and the
# tailnum and carrier columns from the flights dataset as you perform in the merging process (use can you
# the left_join() or right_join() functions). Also, don’t forget to use the distinct() function in order to
# avoid repetitions. Your resulting dataset should be similar to the following:

planes_flights <- left_join(
  planes_ext %>% select(tailnum, age, manufacturer, seats),
  flights_mut %>% select(tailnum, carrier),
  by = "tailnum"
) %>% 
  distinct()# To avoid repetitions, because there are many flights with same planes "unique combination of tailnumand carrier"

# b.1- What is the number of rows of (i.e. the number of planes in) the data set you made in a.2? What about
# the data set you made in b? Technically, they should have the same number of rows; however, they do not.
# Can you explain why there is a difference?

planes_ext %>% 
  summarize(n()) # 3322

planes_flights %>% 
  summarize(n()) # 3339 "because there are some planes owned by more than one carrier

planes_flights %>% 
  group_by(tailnum, carrier)

# which planes are owned by more than one carrier

planes_flights %>% 
  group_by(tailnum) %>% 
  summarize(numb_planes = n()) %>% 
  filter(numb_planes > 1)

# Another way to answer the question 
planes_flights %>% 
  filter(duplicated(tailnum))

# b.2- Which carrier owns the largest fleet (i.e. the most planes)?
planes_flights %>% 
  group_by(carrier) %>% 
  summarize(numb_flights = n()) %>% 
  arrange(-numb_flights)

# b.3- Which carrier owned the newest planes on average in 2013?
planes_flights %>% 
  group_by(carrier) %>% 
  summarize(avg_age = mean(age, na.rm = TRUE)) %>% 
  arrange(avg_age)

# all planes older than than 20 years must stop flying for the safety of passengers. Which carrier will be hit the most by this decision (i.e. which carrier will lose the most planes)
planes_flights %>% 
  filter(age > 20) %>% 
  count(carrier, sort = TRUE, name = "numb_planes" )
  # group_by(carrier) %>% 
  # summarize(numb_planes = n()) %>% 
  # arrange(-numb_planes)

# b.5- Now, let’s find out which carrier was hit the most financially. Let’s make the following assumption: each
# plane is valued at 15 bitcoins seat. Compute the bitcoin value of each plane, then compute the value in US
# dollars (you will need to look up the dollar value of a Bitcoin online). Which carrier was hit the most by the
# decision?

planes_flights %>% 
  filter(age > 20) %>% 
  mutate(
    planes_value = seats * 15
  ) %>% 
  group_by(carrier) %>% 
  summarize(financial_loss = sum(planes_value)) %>%
  arrange(-financial_loss)

# b.6- Using the dataset you made in b, create another dataset containing the number of planes that each manufacturer made for each carrier. 
manufacturer_carrier <- planes_flights %>% 
  group_by(manufacturer, carrier) %>% 
  summarize(numb_planes = n())

# b.7- Which manufacturer has the most clients (i.e. carriers)
manufacturer_carrier %>% 
  count(manufacturer, sort = TRUE, name = "numb_clients")

# b.8- Which carrier has the most diverse fleet (i.e. planes from several manufacturers)?
planes_flights %>%
  group_by(carrier) %>% 
  distinct(manufacturer) %>% 
  summarize(numb_manufacture = n()) %>% 
  arrange(-numb_manufacture)

# c- Do weather conditions partially explain departure delays? Finding an answer to this question is our goal in this section. You can perform the tasks in c.1, c.2 and c.3 
# in the same chain of operations using the pipe operator (i.e. %>%).

# c.1- Create two new columns in the weather dataset for daily average temperatures and daily wind speed for each
# weather station (i.e. origin)
# c.2- Add a new column for categorizing the wind speeds with the following specifications:
# • strictly less than 5 MPH: slow wind
# • greater than or equal to 5 MPH and stricly less than 10 MPH: normal wind
# • greater than or equal to 10MPH and strictly less than 20 MPH: fast wind
# • greater than or equal to 20 MPH: very fast wind

# c.3- Add another column for categorizing average temperature with the following specifications:
# • strictly less than 40 degrees: very cold
# • greater than or equal to 40 degrees and strictly less than 55 degrees: cold
# • greater than or equal to 55 degrees and strictly less than 70 degrees: slightly cold
# • greater than or equal to 70 degrees and strictly less than 90 degrees: warm
# • greater than or equal to 90 degrees: hot

avg_weather <- weather %>% 
  group_by(origin, month, day) %>%
  summarize(avg_daily_temp = mean(temp, na.rm = TRUE),
            avg_daily_wind = mean(wind_speed, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    wind_speed_status = case_when(
      avg_daily_wind < 5 ~ "Slow wind",
      avg_daily_wind >= 5 & avg_daily_wind < 10 ~ "normal wind",
      avg_daily_wind >= 10 & avg_daily_wind < 20 ~ "fast wind",
      avg_daily_wind > 20 ~ "very fast wind",
      TRUE ~ "Other"
      
    ),
    temp_status = case_when(
      avg_daily_temp < 40 ~ "Very cold",
      avg_daily_temp >= 40 & avg_daily_temp < 55 ~ "cold",
      avg_daily_temp >= 55 & avg_daily_temp < 70 ~ "slightly cold",
      avg_daily_temp >= 70 & avg_daily_temp < 90 ~ "warm",
      avg_daily_wind > 90 ~ "hot",
      TRUE ~ "Other"
      
    )
  )

# c.4- Merge the extended flights dataset (created in a.1) with the dataset you just created above. Select
# the month, day, dep_delay and origin columns from the extended flights dataset before the merging
# operation (use can you the left_join() or right_join() functions).
flights_weather <- flights_mut %>% 
  select(month, day, dep_delay, origin ) %>% 
  # left_join(avg_weather, by = c("month", "day", "origin"))
  left_join(avg_weather)

# c.5- Compute the average departure delay for each temperature level. Is there any pattern? For example, do
# we experience more departure delay the colder/hotter it gets?
flights_weather %>% 
  group_by(temp_status) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE))

# c.6- Compute the average departure delay for each wind speed level. Is there any pattern? For example, do
# we experience more departure delay when the wind is faster?
flights_weather %>% 
  group_by(wind_speed_status) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE))

# d- You asked a company dealing in data collection to get you data on all airports in the US - they provided
# you with the airports dataset.

# d.1- You are curious to know which airports do not have direct flights to and from the New York City area.
# Use the anti_join() function, the extended flights dataset and the airports dataset to find them.

# First let's see how can you join tables with different column names
x <- planes %>% 
  rename(planes_id = tailnum)

x %>% 
  inner_join(flights_mut, by = c("planes_id" = "tailnum"))

no_direct_flights <- anti_join(
  airports,
  flights_mut,
  by = c("faa" = "dest")
)

# d.2- Compute the number of airports with no direct flights to and from NYC in each timezon
no_direct_flights %>% 
  group_by(tzone) %>% 
  summarize(numb_flights = n()) %>% 
  arrange(desc(numb_flights))

# d.2- Even though the company that provided you with the data is a reputable one, you still want to double
# check whether they gave you what you asked (and paid) for. Check if all destinations in the extended flights
# dataset are also in the airports dataset. To achieve that you can also use the anti_join() function.

anti_join(
  flights_mut,
  airports,
  by = c("dest" = "faa")
)

