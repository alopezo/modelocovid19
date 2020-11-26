library(tidyverse)
library(rgdal)
library(leaflet)

# map y labels ---------------------------------------------------------------------
#load("DatosIniciales/owd_data.RData")
#LA_data <- owd_data %>%
                  group_by(iso_code) %>%
                  dplyr::summarise(cum_cases_millon = round(sum(new_cases,na.rm = T)/max(population)*1e6,2),
                                   cum_deaths_millon = round(sum(new_deaths,na.rm = T)/max(population)*1e6, 2)) %>%
                  mutate(ADM0_A3 = iso_code) %>%
                  as.data.frame()

# shapes países
#download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip", "WorldMap/countries.zip")
#unzip(zipfile = "WorldMap/countries.zip", exdir = "WorldMap")
#World <- readOGR(dsn="WorldMap", layer="ne_10m_admin_0_countries",encoding = 'UTF-8')

#LA <- subset(World, ADM0_A3 %in%  c("ARG","BOL","CRI","SLV","ECU","GTM",
#                                    "HND","JAM","PAN","PRY","DOM","CHL",
#                                    "URY","BRA","PER","MEX","COL", "BHS",
#                                    "BRB","BLZ","GUY","HTI","SUR","TTO","VEN"))
#LA@data <- LA@data[,c("ADM0_A3","NAME")]

# shapes provs
download.file("https://www.indec.gob.ar/ftp/cuadros/territorio/codgeo/Codgeo_Pais_x_dpto_con_datos.zip", "WorldMap/deptosArg.zip")
unzip(zipfile = "WorldMap/deptosArg.zip", exdir = "WorldMap")
Deptos <- readOGR("WorldMap/pxdptodatosok.shp", encoding = 'UTF-8')
deptosAmba<-c(28,35,91,98,119,126,134,245,252,260,266,270,274,329,364,
                371,408,410,412,427,441,434,490,497,515,525,539,560,568,
                638,648,658,749,756,760,778,805,840,861,882)
deptosAmba <- paste0("06",deptosAmba)
map_AMBAprov <-Deptos[Deptos@data$link %in% deptosAmba,]
map_06826 <- Deptos[Deptos@data$link=="06826",]
map_06756 <- Deptos[Deptos@data$link=="06756",]
map_AMBAtot <-Deptos[Deptos@data$link %in% deptosAmba | Deptos@data$codpcia=="02",]


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
# leaflet(map_data,
#         options = leafletOptions(attributionControl=FALSE,
#                                  zoomControl = FALSE)) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(stroke = F)

# save
save.image(file =  "DatosIniciales/Map.RData")


