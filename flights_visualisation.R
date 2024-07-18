
# Load packages -----------------------------------------------------------

library(readr) # for importing data
library(here)  
library(ggplot2) # for data visualization "grammar of graphics"
library(dplyr) # for manipulating data
library(scales) # utilities for plotting
library(tidyr)
# Import the Data ---------------------------------------------------------

flights <- read_csv(here("data/prepped/flights_mut.csv"))
airports <- read_csv(here("data/raw/airports.csv"))
airlines <- read_csv(here("data/raw/airlines.csv"))
planes <- read_csv(here("data/prepped/planes_extended.csv"))
weather <- read_csv(here("data/raw/weather.csv"))



# Data visualization with the ggplot2 packages ----------------------------

# Compare departures across all 3 airports
dep_count <- flights %>% 
  count(origin, sort = TRUE, name = "n_departure")

p <- ggplot(data = dep_count) +
  geom_col(mapping = aes(x = origin, y = n_departure), width = 0.6) +
  labs(x = "Airport of origin", y = "Number of departures",
       title = "Flights traffic out of new york city area",
       subtitle = "In the year 2013", caption = "Data source: nycflights13 R package") +
  theme_linedraw()

p # the plot saved in the p variable   

# save your visualization as a jpeg, png or jpg format
ggsave(here("visualizations/origin_dep_count.jpeg"), plot = p, scale = 2)

# Do carriers prefer to fly out of airports with less delays? "No there's no observable correlation"
flights %>% 
  group_by(origin) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE),
            numb_departures = n()) %>% 
  ggplot() +
  geom_point(mapping = aes(x = origin, y = numb_departures, size = avg_dep_delay))

# Visualize Monthly average temperatures
monthly_temp <- weather %>% 
  group_by(month) %>% 
  summarize(avg_temp = mean(temp, na.rm = TRUE))

ggplot(data = monthly_temp) +
  geom_line(mapping = aes(x = month, y = avg_temp)) +
  scale_x_continuous(breaks = 1:12) + # X axis scale 
  labs(x ="Month", y ="Average monthly temeratures")

# Is there any association between temperatures and wind speed?

weather %>% 
  filter(origin == "EWR", month == 12, hour == 07) %>% 
  ggplot() +
  geom_point(mapping = aes(x = temp, y = wind_speed), size = 0.5) 

# Various graphic properties
# alpha "opacity"
# Do pilots fly faster over longer trips?
flights %>% 
  filter(month == 5) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = distance, y = avg_speed_kmh), size = 0.9, alpha = 0.1)

# Visualize average departure delay for each carrier
flights %>% 
  group_by(carrier) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot() + 
  geom_col(mapping = aes(x = carrier, y = avg_dep_delay))

# Visualize Monthly average departure delay for each carrier
flights %>% 
  group_by(month, carrier) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = month, y = avg_dep_delay, color = carrier))

# Daily average departures delay by origin
flights %>% 
  select(origin, year, month, day, dep_delay) %>% 
  mutate(date = ISOdate(year = year, month = month, day = day)) %>% 
  group_by(origin, date) %>% 
  summarize(daily_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(mapping = aes(x = date, y = daily_delay, color = origin)) + 
  geom_point() +
  geom_smooth() + # draw a line showing the trend of the data
  facet_wrap(~origin) + # split the graph to multiple graphes by origine
  labs(x = "date in 2013", y = "daily departure delay", color = "Origin of the flights")

# Most top 10 popular AA destinations out of NYC
flights %>% 
  filter(carrier == "AA") %>% 
  count(dest, sort = TRUE, name = "numb_flights") %>% 
  top_n(10) %>%
  # mutate(dest = reorder(dest, -numb_flights)) %>%  # it's like arrange "arrange just show you the arranged data, but reorder changed the order of data in the source"
  ggplot() + 
  geom_col(mapping = aes(x = reorder(dest, -numb_flights ) , y = numb_flights)) +
  labs(x = "Destination of the flights", y = "Number of flights made") +
  coord_flip() + # flip the Y and X axis
  scale_y_continuous(labels = comma_format()) + # adding separate comma to the y-axis labels using the scales library
  scale_y_continuous(labels = dollar_format(prefix = "DH"))  # adding dollar symbole "-" to the x-axis labels
  
  
# install.packages(c("scales", "tidyr"))

  
  
