################################################################################
# Snow Depth App for New Hampshire
#
# Virginia Tech EDS Capstone
################################################################################

library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)

#Import Data
A4Data <- read.csv("Data/Timeseries_Data/DATA_TS_A4_20240106.csv")
C3Data <- read.csv("Data/Timeseries_Data/DATA_TS_C3_20240106.csv")
D2Data <- read.csv("Data/Timeseries_Data/DATA_TS_D2_20240106.csv")
E1Data <- read.csv("Data/Timeseries_Data/DATA_TS_E1_20240106.csv")

#Might use later
#Events <- ordered(BigTop100$Event, levels = c("50 Free", "100 Free", "200 Free", "500 Free", "1000 Free", "1650 Free", "100 Fly", "200 Fly", "100 Back", "200 Back", "100 Breast", "200 Breast", "100 IM", "200 IM", "400 IM", "200 Free Relay", "400 Free Relay", "800 Free Relay", "200 Medlay Relay", "400 Medlay Relay"))

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# UI Code Start
ui <- fluidPage(
  
  # Navbar structure for UI
  navbarPage("SNOW DEPTH NEW HAMPSHIRE", theme = shinytheme("lumen"),
             tabPanel("Snow Depth By Station", fluid = TRUE, icon = icon("snowflake"),
                      tags$style(button_color_css)
             ),
             
             # Sidebar layout for main page
             sidebarLayout(
               sidebarPanel(
                 
                 #Turn on/off the sites
                 titlePanel("Site Characteristics"),
                 fluidRow(column(3,
                                 
                                 # Select the sites to plot
                                 checkboxGroupInput(inputId = "SiteDisplay",
                                                    label = "Sites of Interest:",
                                                    choices = c("A4" = "A4", "C3" = "C3", "D2" = "D2", "E1" = "E1"),
                                                    selected = "A4"),
                                 
                                 # Select which Division(s) to plot
                                 checkboxGroupInput(inputId = "DivisionFinder",
                                                    label = "Select Division(s):",
                                                    choices = c("DI", "DII", "DIII"),
                                                    selected = "DI")
                 )) 
                 
             ))
             
  ) #End of Navbar
) #End of UI


# Define server
server <- function(input, output, session) {
  
}

# Run the application
shinyApp(ui = ui, server = server)
