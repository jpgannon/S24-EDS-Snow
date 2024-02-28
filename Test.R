library(shiny)
library(ggplot2)
library(raster)
library(sf)

# Define UI
ui <- fluidPage(
  titlePanel("Raster to Polygon Plot"),
  plotOutput("plot")  # Ensure this ID matches the output ID in the server function
)

# Define server logic
server <- function(input, output) {
  # Read the .tif file
  raster_data <- raster("Data/Rasters/SnowChange_11_15_11_30.tif")
  
  # Convert raster to polygons
  raster_polygons <- rasterToPolygons(raster_data, dissolve = TRUE)
  
  # Convert to sf object
  raster_sf <- sf::st_as_sf(raster_polygons)
  
  # Plot raster polygons
  output$plot <- renderPlot({
    ggplot() +
      geom_sf(data = raster_sf) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
