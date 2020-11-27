library(tidyverse)
library(rgdal)
library(leaflet)
library(raster)
library(rgeos)



# shapes 
download.file("https://www.indec.gob.ar/ftp/cuadros/territorio/codgeo/Codgeo_Pais_x_dpto_con_datos.zip", "WorldMap/deptosArg.zip")
unzip(zipfile = "WorldMap/deptosArg.zip", exdir = "WorldMap")
Deptos <- readOGR("WorldMap/pxdptodatosok.shp", encoding = 'UTF-8')
deptosAmba<-c("028","035","091","098","119","126","134","245","252","260","266","270","274","329","364",
                "371","408","410","412","427","441","434","490","497","515","525","539","560","568",
                "638","648","658","749","756","760","778","805","840","861","882")
deptosAmba <- paste0("06",deptosAmba)
map_arg <- aggregate(Deptos)
map_AMBAprov <-Deptos[Deptos@data$link %in% deptosAmba,]
map_06826 <- Deptos[Deptos@data$link=="06826",]
map_06756 <- Deptos[Deptos@data$link=="06756",]
map_03 <-raster::aggregate(Deptos[Deptos@data$link %in% deptosAmba | Deptos@data$codpcia=="02",])
map_07 <-raster::aggregate(Deptos[Deptos@data$link %in% deptosAmba,])
map_02 <-raster::aggregate(Deptos[Deptos@data$codpcia=="02",])
 # leaflet(Deptos,
 #         options = leafletOptions(attributionControl=FALSE,
 #                                  zoomControl = FALSE)) %>%
 #   addProviderTiles(providers$CartoDB.Positron) %>%
 #   addPolygons(stroke = F)


# coords
coords = data.frame(pais = c("ARG_6_826"),
                    lng = -57.935813,
                    lat = -28.983072)


# test
# leaflet(map_07,
#         options = leafletOptions(attributionControl=FALSE,
#                                  zoomControl = FALSE)) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(stroke = F)

# save
save.image(file =  "DatosIniciales/Map.RData")


