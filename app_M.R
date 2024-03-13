####################################################################################
# Snow Depth App for New Hampshire
#
# Virginia Tech EDS Capstone
####################################################################################

library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
#library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)
library(terra)
library(leaflet)

#setwd("C:/Users/Cliff Johnson/OneDrive/Desktop/S24-EDS-Snow-main/S24-EDS-Snow-leaflet")

#Reading in Data
dataSites <- read.csv("averages_by_hour_allsites_1.csv")
SiteA4 <- read.csv("averages_by_hour_A4.csv")
SiteC3 <- read.csv("averages_by_hour_C3.csv")
SiteD2 <- read.csv("averages_by_hour_D2.csv")
SiteE1 <- read.csv("averages_by_hour_E1.csv")
sites <- ordered(dataSites$Site_Name, levels = c("A4", "C3", "D2", "E1"))
Nov15Feb2 <- rast("Data/Rasters/SnowChange_11_15_2_2.tif")
Nov15Nov30 <- rast("Data/Rasters/SnowChange_11_15_11_30.tif")
Nov30Feb2 <- rast("Data/Rasters/SnowChange_11_30_2_2.tif")
#palette1 <- colorNumeric(palette = 'Y10rRd', domain = values(raster_data))

#shapefile <- readShapeSpatial("Data/Shapefiles/hbef_ws3_siteLocations/hbef_23-24_UAS_SiteLocations.shp")
#shapefile_df <- fortify(shapefile)


#rgb <- brick(“pathto/rgb.tif”)
#plot(grey)
#plotRGB(rgb)

#timeVars <- ordered(dataSites$Hour, levels = c("A4", "C3", "D2", "E1"))

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
                      tags$style(button_color_css),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Site Characteristics"),
                          fluidRow(
                            column(3,
                                   checkboxGroupInput(inputId = "SiteDisplay",
                                                      label = "Sites of Interest:",
                                                      choices = c("A4" = "A4", "C3" = "C3", "D2" = "D2", "E1" = "E1"),
                                                      selected = "A4"),
                                   checkboxGroupInput(inputId = "RasterDisplay",
                                                      label = "Rasters:",
                                                      choices = c("Nov15toNov30" = "Nov15toNov30", "Nov30toFeb2" = "Nov30toFeb2", "Nov15toFeb2" = "Nov15toFeb2"),
                                                      selected = "Nov15toNov30")
                            )
                          )
                        ),
                        mainPanel(
                          leafletOutput("map"),
                          withSpinner(
                            plotOutput(outputId = "scatterplotFinder", click = "click_plotFinder")
                          ),
                          hr(),
                          fluidRow(
                            column(7,
                                   helpText("Tip: Click locations to populate table below with information on schools in a specific area")
                            ),
                            column(width = 2, offset = 2, conditionalPanel(
                              condition = "output.schoolstableFinder",
                              actionButton(inputId = "FinderClear", label = "Clear Table")
                            ))
                          ),
                          br(),
                          fluidRow(
                            withSpinner(
                              dataTableOutput(outputId = "schoolstableFinder")
                            )
                          )
                        )
                      )
             ),
             
             # Tab Panel for Temporal Data from data sites
             tabPanel("Temporal Data",
                      fluid = TRUE,
                      icon = icon("line-chart"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Sites"),
                          fluidRow(
                            column(3,
                                   checkboxGroupInput(inputId = "Sites",
                                                      label = "Temporal Data from Sites",
                                                      choices = c("A4" = "A4", "C3" = "C3", "D2" = "D2", "E1" = "E1"),
                                                      selected = "A4"),
                                   checkboxGroupInput(inputId = "Site Variables",
                                                      label = "Site Variables:",
                                                      choices = c("hour" = "hour"),
                                                      selected = "hour")
                            )
                          )
                        ),
                        mainPanel(
                          plotOutput(outputId = "scatterplotTemporal")
                        )
                      )
             )
  ) #End of navbar
) #End of UI Code

# Define server
server <- function(input, output, session) {
  
  BigTop100_finder <- reactive({
    req(input$SiteDisplay)
  })
  
  # Map display
  output$map <- renderLeaflet({
    
    selected_raster <- switch(input$RasterDisplay, "November 15th to November 30th" = Nov15Nov30, 
                              "November 30th to February 2nd", = Nov30Feb2,
                              "November 15th to February 2nd" = Nov15Feb2)
    
    #if()
    
      leaflet(selected_raster) %>% 
      addTiles() %>% 
      addRasterImage(raster_data, opacity = 0.8)
  })
  
  # Scatter plot for Temporal Data
  output$scatterplotTemporal <- renderPlot({
    # Filter data based on selected site
    selected_site <- switch(input$Sites, "A4" = SiteA4, "C3" = SiteC3, "D2" = SiteD2, "E1" = SiteE1)
    
    # Check if any site variable is selected
    if ("hour" %in% input$`Site Variables`) {
      # Your scatter plot code here
      ggplot(selected_site, aes(x = hour, y = Tair_C, color = Site_Name)) +
        geom_point() +
        theme_minimal()
    }
  })
}



#server <- function(input, output, session) {

#BigTop100_finder <- reactive({
# req(input$SiteDisplay)
#  })

#Map display
# output$map <- renderLeaflet({

# leaflet(raster_data) %>% 
#  addTiles() %>% 
#    addRasterImage(raster_data, opacity = 0.8)

#  })

# Scatter plot for Temporal Data
# output$scatterplotTemporal <- renderPlot({
# Your scatter plot code here
# ggplot(SiteA4, aes(x = hour, y = Tair_C, color = Site_Name)) +
#  geom_point() +
#  theme_minimal()
# })

#}

# Run the application
shinyApp(ui = ui, server = server)


