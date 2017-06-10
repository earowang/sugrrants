# Loading libraries -----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(sugrrants)

# Pedestrian data
ped_counts <- read_csv("data-raw/pedestrian.csv")

pedestrian <- ped_counts %>% 
  filter(Sensor_ID %in% c(18, 13, 6, 3, 9, 25, 30)) %>% 
  filter(Year > 2015) %>% 
  mutate(
    Date_Time = dmy_hm(Date_Time),
    Month = month(Date_Time, label = TRUE, abbr = FALSE),
    Day = wday2(Date_Time, label = TRUE, abbr = FALSE),
  ) %>% 
  select(-ID)

devtools::use_data(pedestrian, overwrite = TRUE)
