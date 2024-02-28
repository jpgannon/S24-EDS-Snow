#Loading in packages
library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)
library(raster)
library(terra)
library(jsonlite)
library(sp)
library(leaflet)


#Reading in the data
data <- read.csv("Data/Timeseries_Data/DATA_TS_E1_20240106.csv", stringsAsFactors = FALSE)

#Formatting the timestamp column in the data
data$timestamp <- as.POSIXct(data$timestamp, format = c("%m/%d/%y %H:%M", "%m.%d.%y %H:%M"))

#Rounding down the timestamp to the nearest hour
data$hour <- floor_date(data$timestamp, "hour")

#Grouping by hour and calculating the average for each variable
result <- data %>%
  group_by(hour) %>%
  summarise(across(starts_with("T"), mean, na.rm = TRUE), 
            Site_Name = first(Site_Name) )

#Formatting the hour column to match the original time format
result$hour <- format(result$hour, "%m/%d/%y %H:%M")

#Save the result for individual averages
write_csv(result, "averages_by_hour_E1.csv")


#Reading in the files to merge
data1 <- read.csv("averages_by_hour_A4.csv", header = TRUE)
data2 <- read.csv("averages_by_hour_C3.csv", header = TRUE)
data3 <- read.csv("averages_by_hour_D2.csv", header = TRUE)
data4 <- read.csv("averages_by_hour_E1.csv", header = TRUE)

#Merging the files 
combined_data <- bind_rows(data1, data2, data3, data4)

#Writing the combined averages to a new CSV
write_csv(combined_data, "averages_by_hour_allsites.csv")

#Testing for getting only New England States
fiftystatesCAN <- read.csv("fiftystatesCAN.csv") #From https://github.com/gpilgrim2670/SwimMap/tree/master repo
region <- fiftystatesCAN %>% filter(GeoRegion == "NewEngland")



