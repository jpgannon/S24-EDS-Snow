

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
                  #mutate(Year = year(timestamp))
                  #dates <- as.Date(tot_site$timestamp, format = "%d-%m-%Y")
                  #tot_site$timestamp <- format(dates, "%Y-%m-%d")


                    #tttt_sites <- parse_datetime(tot_site, format = "", na = na, local = default_locale(), trim_ws =  TRUE)


                    #ggplot(tot_site, aes(x = ))
#renamed columns to more direct titles
avg_hr_allsite_1 <- averages_by_hour_allsites |>
  rename( air_temp = Tair_C , Sen_temp = Tinternal_C )

avg_hr_allsite_1 <- tot_site |>
  rename( air_temp = Tair_C , Sen_temp = Tinternal_C, Rel_hum = RH_p, Dis_m = distance_m )


#merge all columns
Site_A4 <- Site_A4 |>
  rename( air_temp = Tair_C , Sen_temp = Tinternal_C, Rel_hum = RH_p, Dis_m = distance_m )
Site_C3 <- Site_C3 |>
  rename( air_temp = Tair_C , Sen_temp = Tinternal_C, Rel_hum = RH_p, Dis_m = distance_m )
Site_D2 <- Site_D2 |>
  rename( air_temp = Tair_C , Sen_temp = Tinternal_C, Rel_hum = RH_p, Dis_m = distance_m )
Site_E1 <- Site_E1 |>
  rename( air_temp = Tair_C , Sen_temp = Tinternal_C, Rel_hum = RH_p )

all <- bind_rows(avg_hr_allsite, Site_A4, Site_C3, Site_D2, Site_E1)
all_2 <-bind_rows(Site_A4, Site_C3, Site_D2, Site_E1) 
#change to datetime format
all$timestamp <- as.POSIXct(all$timestamp, format = c("%m/%d/%y %H:%M", "%m.%d.%y %H:%M"))
all_2$timestamp <- as.POSIXct(all_2$timestamp, format = c("%m/%d/%y %H:%M", "%m.%d.%y %H:%M"))

#sum by hours
all_2$hour <- floor_date(all_2$timestamp, "hour")

all_2 <- all_2 %>%
  group_by(hour) %>%
  summarise(across(starts_with("T"), mean, na.rm = TRUE), 
            Site = first(Site) )

all_2$hour <- format(all_2$hour, "%m/%d/%y %H:%M")



#remove timestamp column
avg_hour <- avg_hr_allsite_1 |>
  subset(select = -c(timestamp))


tot_site$timestamp <- as.POSIXct(tot_site$timestamp, format = c("%m/%d/%y %H:%M", "%m.%d.%y %H:%M"))

#Rounding down the timestamp to the nearest hour
tot_site$hour <- floor_date(tot_site$timestamp, "hour")

#Grouping by hour and calculating the average for each variable
  #(removes distance)
result <- tot_site %>%
  group_by(hour) %>%
  summarise(across(starts_with("T"), mean, na.rm = TRUE), 
            Site = first(Site) )


#removing old time stamp column
avg_hour <- tot_site |>
  subset(select = -c(timestamp))

avg_hr_allsite_2 <- avg_hr_allsite_1 |>
  subset(select = -c(timestamp))



#averaging
tot <- tot_site |>
  filter(!duplicated(hour))

tot <- tot |>
  rename(Site_Name = Site)

write_csv(tot, "averages_by_hour_allsites_1.csv")
