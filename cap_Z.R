

#call packages
library(tidyverse)
library(lubridate) 
library(readr)

#read sites
DATA_TS_A4_20240106 <- read_csv("Data/Timeseries_Data/DATA_TS_A4_20240106.csv")
DATA_TS_C3_20240106 <- read_csv("Data/Timeseries_Data/DATA_TS_C3_20240106.csv")
DATA_TS_D2_20240106 <- read_csv("Data/Timeseries_Data/DATA_TS_D2_20240106.csv")
DATA_TS_E1_20240106 <- read_csv("Data/Timeseries_Data/DATA_TS_E1_20240106.csv")


#Rename sites
Site_A4 <- DATA_TS_A4_20240106
Site_C3 <- DATA_TS_C3_20240106
Site_D2 <- DATA_TS_A4_20240106
Site_E1 <- DATA_TS_E1_20240106


#Give each site it's own identifying column
Site_A4 <- Site_A4 |>
  mutate(Site = "A4")

Site_C3 <- Site_C3 |>
  mutate(Site = "C3")

Site_D2 <- Site_D2 |>
  mutate(Site = "D2")

Site_E1 <- Site_E1 |>
  mutate(Site = "E1")

tot_site <- bind_rows(Site_A4, Site_C3, Site_D2, Site_E1)

#test sites are in one df
tot_site |>
  distinct(Site)



                #tot_site1 <- tot_site |>
               # mutate(Year = year(timestamp))
                  #dates <- as.Date(tot_site$timestamp, format = "%d-%m-%Y")
                  #tot_site$timestamp <- format(dates, "%Y-%m-%d")


#tttt_sites <- parse_datetime(tot_site, format = "", na = na, local = default_locale(), trim_ws =  TRUE)


ggplot(tot_site, aes(x = ))
