library(dplyr)
library(lubridate)

data <- read.csv("Data/Timeseries_Data/DATA_TS_A4_20240106.csv", stringsAsFactors = FALSE)

data$timestamp <- as.POSIXct(data$timestamp, format = c("%m/%d/%y %H:%M", "%m.%d.%y %H:%M"))

# Round down the timestamp to the nearest hour
data$hour <- floor_date(data$timestamp, "hour")

# Group by hour and calculate the average for each variable
result <- data %>%
  group_by(hour) %>%
  summarise(across(starts_with("T"), mean, na.rm = TRUE), 
            Site_Name = first(Site_Name))

# Format the hour column to match the specified format
result$hour <- format(result$hour, "%m/%d/%y %H:%M")

# Print the result
print(result)

#Save the result
write_csv(result, "averages_by_hour_A4.csv")