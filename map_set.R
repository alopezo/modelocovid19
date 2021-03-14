library(tidyverse)
library(rgdal)
library(leaflet)
library(raster)

# map y labels ---------------------------------------------------------------------
load("DatosIniciales/owd_data.RData")
LA_data <- owd_data %>%
                  group_by(iso_code) %>%
                  dplyr::summarise(cum_cases_millon = round(sum(new_cases,na.rm = T)/max(population)*1e6,2),
                                   cum_deaths_millon = round(sum(new_deaths,na.rm = T)/max(population)*1e6, 2)) %>%
                  mutate(ADM0_A3 = iso_code) %>%
                  as.data.frame()

# shapes países
download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip", "WorldMap/countries.zip")
unzip(zipfile = "WorldMap/countries.zip", exdir = "WorldMap")
World <- readOGR(dsn="WorldMap", layer="ne_10m_admin_0_countries",encoding = 'UTF-8')

LA <- subset(World, ADM0_A3 %in%  c("ARG","BOL","CRI","SLV","ECU","GTM",
                                    "HND","JAM","PAN","PRY","DOM","CHL",
                                    "URY","BRA","PER","MEX","COL", "BHS",
                                    "BRB","BLZ","GUY","HTI","SUR","TTO","VEN"))
LA@data <- LA@data[,c("ADM0_A3","NAME")]

# shapes provs
download.file("https://www.indec.gob.ar/ftp/cuadros/territorio/codgeo/Codgeo_Pais_x_prov_datos.zip", "WorldMap/provinciasArg.zip")
unzip(zipfile = "WorldMap/provinciasArg.zip", exdir = "WorldMap")
Provs <- readOGR("WorldMap/pxpciadatosok.shp", encoding = 'UTF-8')
Provs <- subset(Provs, link %in% c("18","02","50"))



# leaflet(Provs,
#         options = leafletOptions(attributionControl=FALSE,
#                                  zoomControl = FALSE)) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(stroke = F)
Provs@data <- Provs@data[c("link","provincia")]
Provs@data$link = as.character(Provs@data$link)
Provs@data$provincia = as.character(Provs@data$provincia)
Provs@data$provincia[Provs@data$link=="02"] <- "Ciudad Autónoma de Buenos Aires"
Provs@data$provincia[Provs@data$link=="18"] <- "Corrientes"
Provs@data$provincia[Provs@data$link=="50"] <- "Mendoza"
Provs@data$provincia[Provs@data$link=="06"] <- "Buenos Aires"
colnames(Provs@data) <- c("NAME","ADM0_A3")
map_data <- rbind(LA, Provs)
map_data@data$ADM0_A3 = as.character(map_data@data$ADM0_A3)

map_data <- merge(map_data, LA_data, by = "ADM0_A3", all.x=T)
map_data@data$ADM0_A3[map_data@data$ADM0_A3=="Corrientes"] = "ARG_18"
map_data@data$ADM0_A3[map_data@data$ADM0_A3=="Ciudad Autónoma de Buenos Aires"] = "ARG_2"
map_data@data$ADM0_A3[map_data@data$ADM0_A3=="Buenos Aires"] = "ARG_7"
map_data@data$ADM0_A3[map_data@data$ADM0_A3=="Mendoza"] = "ARG_50"

map_data@data[map_data@data$ADM0_A3=="ARG_18",4:5] <- LA_data[LA_data$iso_code=="ARG_18",2:3] 
map_data@data[map_data@data$ADM0_A3=="ARG_2",4:5] <- LA_data[LA_data$iso_code=="ARG_2",2:3]
map_data@data[map_data@data$ADM0_A3=="ARG_7",4:5] <- LA_data[LA_data$iso_code=="ARG_7",2:3]
map_data@data[map_data@data$ADM0_A3=="ARG_50",4:5] <- LA_data[LA_data$iso_code=="ARG_50",2:3]

# coords
coords = data.frame(pais = as.character(map_data@data$ADM0_A3),
                    lng = coordinates(map_data)[,1],
                    lat = coordinates(map_data)[,2])
coords$lng[coords$pais=="ARG_18"] = -57.935813
coords$lat[coords$pais=="ARG_18"] = -28.983072
coords$lng[coords$pais=="ARG_2"] = -58.437710
coords$lat[coords$pais=="ARG_2"] = -34.598576

download.file("https://www.indec.gob.ar/ftp/cuadros/territorio/codgeo/Codgeo_Pais_x_dpto_con_datos.zip", "WorldMap/departamentosArg.zip")
unzip(zipfile = "WorldMap/departamentosArg.zip", exdir = "WorldMap")
Deptos <- readOGR("WorldMap/pxdptodatosok.shp", encoding = 'UTF-8')

deptosAmba<-c(28,35,91,98,119,126,134,245,252,260,266,270,274,329,364,
              371,408,410,412,427,441,434,490,497,515,525,539,560,568,
              638,648,658,749,756,760,778,805,840,861,882)

ambaMap <- aggregate(subset(Deptos, 
                            link %in% paste0("06",str_pad(deptosAmba,3,"left","0")) |
                              codpcia=="02"))

ambaProvMap <- aggregate(subset(Deptos, 
                                link %in% paste0("06",str_pad(deptosAmba,3,"left","0"))))

cabaMap <- aggregate(subset(Deptos, 
                            codpcia=="02"))


# test
# leaflet(map_data,
#         options = leafletOptions(attributionControl=FALSE,
#                                  zoomControl = FALSE)) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(stroke = F)

# save
save(map_data,
     Deptos,
     coords, 
     ambaMap, 
     ambaProvMap, file =  "DatosIniciales/Map.RData")


