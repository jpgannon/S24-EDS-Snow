library(terra)
library(leaflet)
raster_data <- rast("/Users/micahaxford/Desktop/CapstoneProjects/Data/Rasters/SnowChange_11_15_2_2.tif")


pal <- colorNumeric(
  palette = c("#000000",'#FFFFFF'), 
  domain = NULL)

leaflet(raster_data) |> 
  addTiles() |> 
  addRasterImage(raster_data, colors = pal, opacity = 0.8)

#map <- fitBounds(map,43.96, -71.69, 43.91, -71.81)

map

