library(terra)
library(leaflet)
raster_data <- rast("Data/Rasters/SnowChange_11_15_2_2.tif")

#raster_ <- as.matrix(raster_data)
palette1 <- colorNumeric(palette = 'Y10rRd', domain = values(raster_data))

leaflet(raster_data) |> 
  addTiles() |> 
  addRasterImage(raster_data, colors = palette1 , opacity = 0.8)

#map <- fitBounds(map,43.96, -71.69, 43.91, -71.81)

map

