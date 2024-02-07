

#call packages
library(tidyverse)
library(lubridate) 

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

tot_site |>
  distinct(Site)
