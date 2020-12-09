library(tidyverse)
library(rgdal)
library(leaflet)
library(raster)

# map y labels ---------------------------------------------------------------------
map_data <- readOGR("WorldMap/Departamentos_Paraguay.shp", encoding = 'UTF-8')



# test
# leaflet(Deptos,
#         options = leafletOptions(attributionControl=FALSE,
#                                  zoomControl = FALSE)) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(stroke = F)

# save
save(map_data, file =  "DatosIniciales/Map.RData")


