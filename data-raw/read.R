library(dplyr)
library(lubridate)
ped_counts <- readr::read_csv("data-raw/Pedestrian_Counts_Oct16.csv")

pedestrian <- ped_counts %>% 
  filter(Sensor_ID %in% c(18, 13, 6, 3, 9, 4, 25, 30)) %>% 
  filter(Year == 2015) %>% 
  select(Date_Time, Sensor_ID, Sensor_Name, Hourly_Counts) %>% 
  mutate(Date_Time = dmy_hm(Date_Time))

devtools::use_data(pedestrian, overwrite = TRUE)
