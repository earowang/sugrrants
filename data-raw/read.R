# Loading libraries -----------------------------------------------------------
library(dplyr)
library(tibble)
library(lubridate)

# Pedestrian data
ped_counts <- readr::read_csv("data-raw/Pedestrian_Counts_Oct16.csv")

pedestrian <- ped_counts %>% 
  filter(Sensor_ID %in% c(18, 13, 6, 3, 9, 4, 25, 30)) %>% 
  filter(Year == 2015) %>% 
  select(Date_Time, Sensor_ID, Sensor_Name, Hourly_Counts) %>% 
  mutate(Date_Time = dmy_hm(Date_Time))

devtools::use_data(pedestrian, overwrite = TRUE)

# Sweden pedestrian data
sweden_temp <- as_tibble(readRDS("data-raw/sweden.stn.loc.melt.rds"))
# Assign location index from north to south
lat_level <- sort(unique(sweden_temp$lat), decreasing = TRUE)
sweden_temp <- sweden_temp %>% 
  select(-time) %>% 
  filter(!is.na(temp)) %>% 
  mutate(date = as_date(date)) %>% 
  filter(date >= ymd("1998-01-01")) %>% 
  mutate(locations = as.factor(as.integer(factor(lat, levels = lat_level))))

devtools::use_data(sweden_temp, overwrite = TRUE)
