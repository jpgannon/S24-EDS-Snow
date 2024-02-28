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


#Reading in Data
dataSites <- read.csv("averages_by_hour_allsites_1.csv")
sites <- ordered(dataSites$Site_Name, levels = c("A4", "C3", "D2", "E1"))
raster_data <- rast("Data/Rasters/SnowChange_11_15_2_2.tif")
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
                      tags$style(button_color_css)
             ),
             
             # Sidebar layout for main page
             sidebarLayout(
               sidebarPanel(
                 
                 # Turn on/off the sites
                 titlePanel("Site Characteristics"),
                 fluidRow(
                   column(3,
                          # Select the sites to plot
                          checkboxGroupInput(inputId = "SiteDisplay",
                                             label = "Sites of Interest:",
                                             choices = c("A4" = "A4", "C3" = "C3", "D2" = "D2", "E1" = "E1"),
                                             selected = "A4"),
                          checkboxGroupInput(inputId = "RasterDisplay",
                                             label = "Rasters:",
                                             choices = c("Nov15toNov30" = "Nov15toNov30", "Nov3toFeb2" = "Nov3toFeb2"),
                                             selected = "Nov15toNov30"),
                   ) #End of column 3
                 ) #End of FLuid Row
                 ),#End of sidebar panel
                 
                
               
               
                 #Main Panel
                 mainPanel(
                   
                   leafletOutput("map"),
                   
                   #Spinner
                   withSpinner(
                     plotOutput(outputId = "scatterplotFinder", click = "click_plotFinder")
                   ),
                   hr(),
                   fluidRow(
                     column(7,
                            helpText("Tip: Click locations to populate table below with information on schools in a specific area")
                            #actionButton(inputId = "draw", label = "Input Event and Times")
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
                     ) #End of dataTableOutput
                   ) #End of br() fluid row
                 ) #End of Main Pannel
               ) #End of sidebar layout
             ) #End of navbar
  ) #End of UI Code
  
  # Define server
  server <- function(input, output, session) {
    
    BigTop100_finder <- reactive({
      req(input$SiteDisplay)
    })
    
    #Map display
    output$map <- renderLeaflet({
        
        leaflet(raster_data) %>% 
          addTiles() %>% 
          addRasterImage(raster_data, opacity = 0.8) %>%
          setView(0, 0, zoom = 2)
      
      
      
    })
    
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)
  
