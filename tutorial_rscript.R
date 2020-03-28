### Prepping data for analysis 
# This file is for exploring the crime scene report data from the reclues package. 
# The reclues package uses data from the Knight Lab at Northwestern Univ.
# https://github.com/NUKnightLab/sql-mysteries


## load libraries
library(tidyverse)
library(devtools)
#devtools::install_github("sciencificity/reclues")
library(DBI)
library(reclues)
library(lubridate)

## load data

crime_report <- crime_scene_report # dataset comes with the reclues package

View(crime_report)

## To do list:
# 1. Convert date to Date format
# 2. Clean up the type column - any weird typos?
# 3. Ditto for cities - do we need to worry about anything?
# 4. What's going on with Alice in Wonderland??

# Fix the date column
crime_report$date <- ymd(crime_report$date)
hist(crime_report$date, breaks = 7) # just looking for crime sprees

# Types
unique(crime_report$type) # this looks fine 

# Cities
unique(crime_report$city) # woah, there are a ton. will come back to this later.

# Mentions of Alice
crime_report$alice_words <- if_else(str_detect(string = crime_report$description, 
                                         pattern = "Alice|Queen|Hatter|Caterpillar|Cheshire"),
                              true = 1,
                              false = 0)

# Alice over time
alice_over_time <- ggplot(data = crime_report, aes(x = date, y = alice_words)) +
  stat_summary(fun = sum, geom = "point", alpha = 0.3)

alice_over_time

# Ok, no pattern by time, let's try type of crime?
alice_crime <- crime_report %>% 
  group_by(type) %>% 
  summarise(alice_words = sum(alice_words),
            total_crimes = n()) %>% 
  arrange(-alice_words)

alice_crime # no real spikes here, generally tracks with total_crimes

# What about city?
alice_city <- crime_report %>% 
  group_by(city) %>% 
  summarise(alice_city = sum(alice_words),
            total_crimes = n()) %>% 
  arrange(-alice_city)

alice_city # again, evenly distributed.

## Ah well, no luck. For a much better mystery, try solving the murder outlined 
## in the reclues package! https://github.com/sciencificity/reclues
