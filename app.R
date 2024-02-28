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
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)
library(terra)
#library(raster)
library(jsonlite)
library(sp)
library(sf)
library(plotly)

#Reading in Data
dataSites <- read.csv("averages_by_hour_allsites_1.csv")
sites <- ordered(dataSites$Site_Name, levels = c("A4", "C3", "D2", "E1"))
fiftystatesCAN <- read.csv("fiftystatesCAN.csv") #From https://github.com/gpilgrim2670/SwimMap/tree/master repo
region <- fiftystatesCAN %>% filter(GeoRegion == "NewEngland")

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
                   fluidRow(
                     column(3, offset = 9,
                            radioButtons(inputId = "show_NamesFinder",
                                         label = "Display:",
                                         choices = c("School Names", "City Names", "Neither"),
                                         selected = "School Names")
                   )),
                   
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
    output$scatterplotFinder <- renderPlot({
      isolate({
        ggplot() +
          #Displays New England Shape on Map
          geom_polygon(data = region, aes(x = long, y = lat, group = group), color = "white", fill = "grey") +
          #geom_raster(data = raster_df, aes(x = x, y = y, fill = value)) + scale_fill_gradientn(colours = terrain.colors(10)) +
          #(geom_sf(data = shapefile, aes(fill = "red")) + coord_sf()) +
          #geom_polygon(data = shapefile_df, aes(x = longitude, y = latitude, group = group), color = "black", fill = "white") + 
        
          #Display sites
          #geom_point(data = region, aes(x = long, y = lat, alpha = 0.8)) + 
          
          coord_quickmap() +
          guides(fill = "none") +
          theme_void() +
          theme(axis.text = element_blank(), axis.ticks = element_blank()) +
          theme(plot.title = element_text(hjust=0.5, face = "bold")) +
          theme(plot.background = element_rect(fill = "white"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
          guides(alpha = "none") +
          theme(legend.text = element_text(size = 12),
                legend.title = element_text(size = 15)) +
          theme(plot.background = element_rect(
            color = "white"
          ))
        
        
      })
      
      
      
    })
    
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)
  
