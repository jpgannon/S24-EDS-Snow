# Load required libraries
library(shiny)
library(leaflet)
library(raster)
library(ggplot2)
library(dplyr)
library(DT)
library(ggrepel)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)
library(terra)
library(leaflet)
library(readr)
library(sf)
library(plotly)
library(RColorBrewer)
#plotly package
#plotly, ggplotly

####################################################################################################################################### 
#Read in temporal data
dataSites <- read_csv("Data/TemporalData/tot_avg_hour.csv", locale = locale(encoding = "latin1")) #Fix for encoding error on CSV

#Order Sites
sites <- ordered(dataSites$Site, levels = c("A4", "C3", "D2", "E1"))

#Read in Rasters
Nov15 <- rast('Data/Rasters/SnowDEMs/HBWS3_V70_11_15_2023_baseline_DEM_1m_TIN.tif')
Nov30 <- rast('Data/Rasters/SnowDEMs/HBWS3_V70_2023_11_30_snow_DEM_1m.tif')
Feb2 <- rast('Data/Rasters/SnowDEMs/HBWS3_V70_02_02_2024_snow_DEM_1m.tif')
Feb20 <- rast('Data/Rasters/SnowDEMs/HBWS3_V70_2024_02_20_snow_DEM_1m.tif')
Mar06 <- rast('Data/Rasters/SnowDEMs/HBWS3_V70_2024_03_06_snow_DEM_1m.tif')
#Read in watersheds, streams, and site locations
watershedBoundaries <- st_read('Data/Shapefiles/hbef_watershed3_boundary/WS_WS3.shp') %>%
  st_transform(4326)
siteLocations <- st_read('Data/Shapefiles/hbef_ws3_siteLocations/HBEFSiteLocations.shp') %>%
  st_transform(4326)
streams <- st_read('Data/Shapefiles/hbef_streams/WSstreams.shp') %>%
  st_transform(4326)

#Read in Hillshades
Hillshade_Nov15 <-rast('Data/Rasters/Hillshade/hillshade2.tif')
Hillshade_Nov30 <- rast('Data/Rasters/Hillshade/Hillshade_Nov30.tif')
Hillshade_Feb02 <- raster('Data/Rasters/Hillshade/Hillshade_Feb02.tif')
Hillshade_Feb20 <- rast('Data/Rasters/Hillshade/Hillshade_Feb20.tif')
Hillshade_Mar06 <- rast('Data/Rasters/Hillshade/Hillshade_Mar06.tif')


#######################################################################################################################################

# Get unique site names
unique_sites <- unique(dataSites$Site)
# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .small-image {
        max-width: 100%;
        height: 1000px;
      }
    "))
  ),
  # Create NavBar
  navbarPage(
    "VT Capstone App",
    theme = shinytheme("cerulean"), # Set navbar theme to cerulean
    
  # Create Intro Tab  
    tabPanel(
      "Introduction",
      fluidRow(
        column(
          12,
          h2("Welcome to our Snow Depth Analysis App!"),
          p("This app is designed to help the research group at the Hubbard Brook Experimental Forest track changes in snow depth and analyze trends using temporal data over time.  
            This data was collected from four different sensors over the span of four months.  Due to frequent time intervals, we averaged the data by hour for each site and combined them into one dataset."),
          p("Our map tab allows you to subtract raster files on an interactive map to see changes in snow depth over time.  We have included streams, site locations, the watershed boundary, and a hillshade to provide some context to the area.  In our temporal plot tab, you can select different variables and create three different plots to visualize and compare data.  We have an interactive date range function to set a time range, but we also have included the functionality to zoom and scroll on the plots themselves to adjust time."),
          p("Please use the tabs above to navigate through different sections of the app."),
        )
      )
    ),
  # Create Map Tab 
    tabPanel(
      "Map",
      icon = icon("snowflake"), 
      sidebarLayout(
        sidebarPanel(
          width = 5,
          div(
            style = "display: flex; align-items: center; justify-content: space-between;",
            div(
              style = "flex: 1;",
              # Select input 1
              selectInput(
                "rasterfile1",
                "Choose Primary Snow Depth",
                choices = c("Snow Free Baseline Elevation" = "Snow Free Baseline Elevation", "November 30 (Snow On)" = "November 30 (Snow On)",
                            "February 2 (Snow On)" = "February 2 (Snow On)", "February 20 (Snow On)" = "February 20 (Snow On)",
                            "March 6 (Snow on)" = "March 6 (Snow On)")
              )
            ),
            #Format and add subtraction symbol
            HTML('<span style="font-size: 20px; margin: 0 10px;">-</span>'),  # Center and increase font size
            div(
              style = "flex: 1;",
              # Select input 2
              selectInput(
                "rasterfile2",
                "Choose Snow Depth to Subtract",
                choices = c("Snow Free Baseline Elevation" = "Snow Free Baseline Elevation", "November 30 (Snow On)" = "November 30 (Snow On)",
                            "February 2 (Snow On)" = "February 2 (Snow On)", "February 20 (Snow On)" = "February 20 (Snow On)",
                            "March 6 (Snow on)" = "March 6 (Snow On)")
              )
            )
          ),
          br(),  # Line break
          div(
            style = 'margin-top: -10px; margin-left: 265px', # center action button between DEMS
            actionButton("subtract", "Subtract Rasters"),
          ),
          br(),
          p(style = "font-size: 12px;", "*The primary elevation will be displayed until the Subtract Rasters function is performed, then the map will display the result of the subtraction"),
          selectInput("hillshade_option", "Select Hillshade",
                      choices = c("None", "November 15 (Snow off)", "November 30", "February 02", "February 20", "March 06"),
                      selected = "None"),
          checkboxInput("show_sites", "Show Site Locations", value = TRUE)
        ),
        mainPanel(leafletOutput("map", height = "800px"))
      )
    ),
    tabPanel(
      "Temporal Plot",
      icon = icon("line-chart"), # Assign line graph icon
      sidebarLayout(
        sidebarPanel(
          # Selection options for Plot 1
          selectInput(
            "y_var1",
            "Select Y Variable (Plot 1)",
            choices = c(
              "Air Temperature" = "air_temp",
              "Sensor Temperature" = "Sen_temp",
              'Relative Humidity' = 'Rel_hum',
              'Distance from Sensor' = "Dis_m",
              'Temp (100 CM above ground)' = 'T100_C',
              'Temp (50 CM above ground)' = 'T50_C',
              'Temp (25 CM above ground)' = 'T25_C',
              'Temp (10 CM above ground)' = 'T10_C',
              'Temp (5 CM above ground)' = 'T5_C',
              'Temp (Ground level)' = 'T0_C',
              'Temp (5 CM below ground)' = 'T5s_C',
              'Temp (10 CM below ground)' = 'T10s_C',
              'Temp (25 CM below ground' = 'T25s_C',
              'Temp (50 CM below ground)' = 'T50s_C'
            )
          ),
          # Selection options for Plot 2
          selectInput(
            "y_var2",
            "Select Y Variable (Plot 2)",
            choices = c(
              "Air Temperature" = "air_temp",
              "Sensor Temperature" = "Sen_temp",
              'Relative Humidity' = 'Rel_hum',
              'Distance from Sensor' = "Dis_m",
              'Temp (100 CM above ground)' = 'T100_C',
              'Temp (50 CM above ground)' = 'T50_C',
              'Temp (25 CM above ground)' = 'T25_C',
              'Temp (10 CM above ground)' = 'T10_C',
              'Temp (5 CM above ground)' = 'T5_C',
              'Temp (Ground level)' = 'T0_C',
              'Temp (5 CM below ground)' = 'T5s_C',
              'Temp (10 CM below ground)' = 'T10s_C',
              'Temp (25 CM below ground' = 'T25s_C',
              'Temp (50 CM below ground)' = 'T50s_C'
            )
          ),
          # Selection options for Plot 3
          selectInput(
            "y_var3",
            "Select Y Variable (Plot 3)",
            choices = c(
              "Air Temperature" = "air_temp",
              "Sensor Temperature" = "Sen_temp",
              'Relative Humidity' = 'Rel_hum',
              'Distance from Sensor' = "Dis_m",
              'Temp (100 CM above ground)' = 'T100_C',
              'Temp (50 CM above ground)' = 'T50_C',
              'Temp (25 CM above ground)' = 'T25_C',
              'Temp (10 CM above ground)' = 'T10_C',
              'Temp (5 CM above ground)' = 'T5_C',
              'Temp (Ground level)' = 'T0_C',
              'Temp (5 CM below ground)' = 'T5s_C',
              'Temp (10 CM below ground)' = 'T10s_C',
              'Temp (25 CM below ground' = 'T25s_C',
              'Temp (50 CM below ground)' = 'T50s_C'
            )
          ),
          dateRangeInput(
            "date_range",
            "Date Range",
            start = as.Date("2023-11-17"),
            end = as.Date("2023-11-20"),  # Set end date to December 20th, 2023
            min = as.Date("2023-11-17"),  # Set min date to November 16th, 2023
            max = as.Date("2024-02-20")    # Set max date to February 21st, 2024
          ),
          
          checkboxGroupInput("sites", "Select Sites", 
                             choices = unique_sites,
                             selected = "A4")  
        ),
        mainPanel(
          plotlyOutput("plot1"),
          plotlyOutput("plot2"),
          plotlyOutput("plot3")
        )
      )
    )
  )
)

#######################################################################################################################################

# Define server logic (continued)
server <- function(input, output, session) {
  
  # Convert 'hour' column to POSIXct format
  dataSites$hour <- as.POSIXct(dataSites$hour, format = "%m/%d/%y %H:%M")
  

#######################################################################################################################################
  
  # Selecting raster 1
  selected_raster1 <- reactive({
    switch(input$rasterfile1,
           "Snow Free Baseline Elevation" = Nov15,
           "November 30 (Snow On)" = Nov30,
           "February 2 (Snow On)" = Feb2,
           "February 20 (Snow On)" = Feb20,
           "March 1 (Snow on)" = Mar01,
           "March 6 (Snow On)" = Mar06,
           "March 26 (Snow On)" = March26)
  })
  # Selecting raster 2
  selected_raster2 <- reactive({
    switch(input$rasterfile2,
           "Snow Free Baseline Elevation" = Nov15,
           "November 30 (Snow On)" = Nov30,
           "February 2 (Snow On)" = Feb2,
           "February 20 (Snow On)" = Feb20,
           "March 1 (Snow on)" = Mar01,
           "March 6 (Snow On)" = Mar06,
           "March 26 (Snow On)" = March26)
  })
  # Select Hillshade to display
  hillshade_raster <- reactive({
    switch(input$hillshade_option,
           "None" = raster(),
           "November 15 (Snow off)" = Hillshade_Nov15,
           "November 30" = Hillshade_Nov30,
           "February 02" = Hillshade_Feb02,
           "February 20" = Hillshade_Feb20,
           "March 06" = Hillshade_Mar06)
  })
  
  # Creating the logic to subtract selected rasters
  subtracted_raster <- eventReactive(input$subtract, {
    raster_data1 <- selected_raster1()
    raster_data2 <- selected_raster2()
    
    # Resample the second raster to match the resolution of the first raster
    raster_data2_resampled <- resample(raster_data2, raster_data1)
    
    # Set negative values to NA for visualization
    result <- raster_data1 - raster_data2_resampled
    result[result < 0] <- NA  
    result
  })
  
  # Function to display raster on map
  observeEvent(input$subtract, {
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addRasterImage(subtracted_raster(), colors = colorRampPalette(c("red", "deepskyblue1", "deepskyblue4"))(255), opacity = 0.7,
                     layerId = "raster_layer")
  })
  
  # Function to update the map with the hillshade raster layer
  observe({
    leafletProxy("map") %>%
      clearImages()  
    
    if (input$hillshade_option != "None" && !is.null(hillshade_raster())) {
      leafletProxy("map") %>%
        addRasterImage(hillshade_raster(),colors = grey.colors(255), opacity = 0.5, layerId = "hillshade_layer")
      
      # Check if the subtracted raster layer exists and add it back if it does
      if (!is.null(subtracted_raster())) {
        leafletProxy("map") %>%
          addRasterImage(subtracted_raster(), colors = colorRampPalette(c("red", "deepskyblue1", "deepskyblue4"))(255), opacity = 0.7,
                         layerId = "raster_layer")
      }
      #Prevents subtracted raster from being removed when the None option is reselected
    } else if (input$hillshade_option == "None" && !is.null(subtracted_raster())) {
      leafletProxy("map") %>%
        addRasterImage(subtracted_raster(), colors = colorRampPalette(c("red", "deepskyblue1", "deepskyblue4"))(255), opacity = 0.7,
                       layerId = "raster_layer")
    }
  })
  observeEvent(input$subtract, {
    subtracted_raster_data <- subtracted_raster()
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearImages() %>%
      addRasterImage(subtracted_raster_data, colors = colorRampPalette(c("red", "deepskyblue1", "deepskyblue4"))(255), opacity = 0.7,
                     layerId = "raster_layer")
    
    # Add or update legend
    pal <- colorNumeric(c("red", "deepskyblue1", "deepskyblue4"), values(subtracted_raster_data))
    leafletProxy("map") %>%
      clearControls() %>%  # Clear existing legend
      addLegend(position = "topright", pal = pal, values = values(subtracted_raster_data), 
                title = "Legend", opacity = 0.7, labFormat = labelFormat(suffix = " (M)"))
    
    # Add watershed boundaries and stream shapefile layers back to the map
    leafletProxy("map") %>%
      addPolygons(data = watershedBoundaries, color = 'black', fillOpacity = 0, weight = 1) %>%
      addPolylines(data = streams, color = 'blue', fillOpacity = 1, weight = 1) %>%
      addCircleMarkers(data = siteLocations, 
                       lng = ~longitude, 
                       lat = ~latitude,
                       radius = 3,  
                       color = "black",  
                       fillOpacity = 1,  
                       popup = ~paste("Site ID: ", siteID, "<br>",
                                      "Elevation: ", elevation_, "m<br>"),
                       layerId = ~siteID)
  })
#######################################################################################################################################
  
  # Rendering plots and filtering data for Plot 1
  observe({
    selected_sites <- input$sites
    filtered_data <- dataSites %>%
      filter(Site %in% selected_sites,
             as.Date(hour) >= input$date_range[1] & as.Date(hour) <= input$date_range[2])
    
    # Create Plotly plot for Plot 1
    output$plot1 <- renderPlotly({
      plot_ly(data = filtered_data, x = ~hour, y = as.formula(paste0("~", input$y_var1)),
              color = ~Site, colors = brewer.pal(4, "Set1"), 
              type = "scatter", mode = "lines+markers", width = 4)
    })
  })
  
  # Rendering plots and filtering data for Plot 2
  observe({
    selected_sites <- input$sites
    filtered_data <- dataSites %>%
      filter(Site %in% selected_sites,
             as.Date(hour) >= input$date_range[1] & as.Date(hour) <= input$date_range[2])
    
    # Create Plotly plot for Plot 2
    output$plot2 <- renderPlotly({
      plot_ly(data = filtered_data, x = ~hour, y = as.formula(paste0("~", input$y_var2)),
              color = ~Site, colors = brewer.pal(4, "Set1"), 
              type = "scatter", mode = "lines+markers", width = 4)
    })
  })
  
  # Rendering plots and filtering data for Plot 3
  observe({
    selected_sites <- input$sites
    filtered_data <- dataSites %>%
      filter(Site %in% selected_sites,
             as.Date(hour) >= input$date_range[1] & as.Date(hour) <= input$date_range[2])
    
    # Create Plotly plot for Plot 3
    output$plot3 <- renderPlotly({
      plot_ly(data = filtered_data, x = ~hour, y = as.formula(paste0("~", input$y_var3)),
              color = ~Site, colors = brewer.pal(4, "Set1"), 
              type = "scatter", mode = "lines+markers", width = 4)
    })
  })
  
#######################################################################################################################################  
    
    # Adding the watersheds to the map
    output$map <- renderLeaflet({
    leaflet() %>%
      addPolygons(data = watershedBoundaries, 
                  color = 'black', 
                  fillOpacity = 0, 
                  weight = 1,
                  dashArray = NULL) %>%
      addTiles() %>%
      addRasterImage(selected_raster1(), colors = terrain.colors(255), opacity = 0.7,
                     layerId = "raster_layer") %>%
      addLegend(pal = colorNumeric(terrain.colors(255), values(selected_raster1())),
                  values = values(selected_raster1()),
                  title = "Legend",
                  opacity = 0.7,
                  labFormat = labelFormat(suffix = " (M)")) %>%
      # Creating site locations for the map 
      addCircleMarkers(data = siteLocations, 
                       lng = ~longitude, 
                       lat = ~latitude,
                       radius = 3,  # Set radius to make markers smaller
                       color = "black",  # Set outline color
                       fillOpacity = 1,# Set fill opacity
                       popup = ~paste("Site ID: ", siteID, "<br>",
                                      "Elevation: ", elevation_, "m<br>"),
                       layerId = ~siteID) %>%
      # Adding streams to the map
      addPolylines(data = streams, 
                   color = 'blue',
                   fillOpacity = 1,
                   weight = 1,
                   dashArray = NULL) %>%
      # Fit map bounds to include all sites  
        fitBounds(lng1 = min(siteLocations$longitude) - 0.00065, 
                  lat1 = min(siteLocations$latitude) - 0.00065,
                  lng2 = max(siteLocations$longitude) + 0.00065,
                  lat2 = max(siteLocations$latitude) + 0.00065) 
  })
  
  # Add the markers to the map
  observeEvent(input$show_sites, {
    if (input$show_sites) {
      leafletProxy("map") %>%
        addCircleMarkers(data = siteLocations, 
                         lng = ~longitude, 
                         lat = ~latitude,
                         radius = 3,  
                         color = "black",  
                         fillOpacity = 1,  
                         popup = ~paste("Site ID: ", siteID, "<br>",
                                        "Elevation: ", elevation_, "m<br>"),
                         layerId = ~
                           siteID,
                         group = "site_locations")  
    } else {
      leafletProxy("map") %>%
        clearMarkers()  # Remove the markers from the map
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)
