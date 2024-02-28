library(terra)
library(leaflet)
raster_data <- rast("/Users/micahaxford/Desktop/CapstoneProjects/Data/Rasters/SnowChange_11_15_2_2.tif")

#raster_ <- as.matrix(raster_data)

leaflet(raster_data) |> 
  addTiles()

map <- fitBounds(map,43.96, -71.69, 43.91, -71.81)

map

