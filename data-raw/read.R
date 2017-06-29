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
    Date = as_date(Date_Time),
    Month = month(Date, label = TRUE, abbr = FALSE),
    Day = wday2(Date, label = TRUE, abbr = FALSE),
  ) %>% 
  select(
    Date_Time, Date, Year, Month, Mdate, Day, Time, 
    Sensor_ID, Sensor_Name, Hourly_Counts
  )

devtools::use_data(pedestrian, overwrite = TRUE)

# BoM data
# devtools::install_github("toowoombatrio/bomrang")
library(bomrang)
sydney <- get_current_weather("Sydney Airport Amo")
melbourne <- get_current_weather("Melbourne Airport")
brisbane <- get_current_weather("Brisbane Aero")
perth <- get_current_weather("Perth Airport")
adelaide <- get_current_weather("Adalaide Airport")
hobart <- get_current_weather("Hobart Airport")
canberra <- get_current_weather("Canberra Airport")
darwin <- get_current_weather("Darwin Airport")
au_weather <- bind_rows(
  sydney, melbourne, brisbane, perth, adelaide, hobart, canberra, darwin
 )
