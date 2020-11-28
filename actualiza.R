#### Actualizar contenido ####
# se actualizan insumos para app, recalculando curvas en base a últimas observaciones
# se utiilzan tres fuentes ppalmente: Ecdc (online), oms (online), owd (online), MSal (descarga manual).
# la funcionalidad de los links de descarga debe ser revisada periódicamente
# una vez organizada la info, se apica función seir.

# librerías
library(tidyverse)
library(readxl)
library(sqldf)
library(readxl)
library(zoo)
library(EpiEstim)

#### países/juris a actualizar ####

hoy <<- diaActualizacion <<- as.Date("2020-11-17")
paises_actualizar <- c( "ARG","BOL","CRI","SLV","ECU","GTM",
                        "HND","JAM","PAN","PRY","DOM","CHL","NIC",
                        "URY","BRA","PER","MEX","COL", "BHS",
                        "BRB","BLZ","GUY","HTI","SUR","TTO","VEN",
                        "ARG_18", "ARG_2", "ARG_7", "ARG_50", "ARG_3", "ARG_6", "ARG_6_826","ARG_6_756")
paises_actualizar <- c("ARG_6_826","ARG_6_756")
##### carga población y oms data  ####
load("DatosIniciales/poblacion_data.RData")
source("oms_data.R", encoding = "UTF-8")

##### descarga ultimos datos de msal  ####
urlMsal <- 'https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.csv'
#download.file(urlMsal, "Covid19Casos.csv")

#### casos/muertes y parámetros para cada país ####
input=list()
for(p in paises_actualizar){
input$pais = p

if (substr(input$pais,1,3)=="ARG"){
  
  dataMsal<-read.csv("Covid19Casos.csv", fileEncoding = "UTF-8")
  dataMsal<-dataMsal %>% filter(clasificacion_resumen=="Confirmado")

  dataMsalARG<-dataMsal
  dataMsalARG$residencia_provincia_id<-"0"
  dataMsalARG$residencia_provincia_nombre<-"Argentina"
  
  dataMsal_6_756<-dataMsal %>% dplyr::filter(residencia_provincia_id==6 & residencia_departamento_id==756)
  dataMsal_6_756$residencia_provincia_id<-"6_756"
  dataMsal_6_756$residencia_provincia_nombre<-"Buenos Aires - Partido de San Isidro"
  
  dataMsal_6_826<-dataMsal %>% dplyr::filter(residencia_provincia_id==6 & residencia_departamento_id==826)
  dataMsal_6_826$residencia_provincia_id<-"6_826"
  dataMsal_6_826$residencia_provincia_nombre<-"Buenos Aires - Partido de Trenque Lauquen"
  
  deptosAmba<-c(28,35,91,98,119,126,134,245,252,260,266,270,274,329,364,
                371,408,410,412,427,441,434,490,497,515,525,539,560,568,
                638,648,658,749,756,760,778,805,840,861,882)
  dataMsalAmba<-dataMsal %>% filter(residencia_provincia_id==2 |
                                      residencia_provincia_id==6 &
                                      residencia_departamento_id %in% (deptosAmba))
  dataMsalAmba$residencia_provincia_id<-as.character(dataMsalAmba$residencia_provincia_id)
  dataMsal$residencia_provincia_id<-as.character(dataMsal$residencia_provincia_id)
  dataMsalAmba$residencia_provincia_id<-"3"
  dataMsalAmba$residencia_provincia_nombre<-"AMBA"
  
  
  dataMsalAmbaPBA<-dataMsal %>% filter(residencia_provincia_id==6 &
                                         residencia_departamento_id %in% (deptosAmba))
  dataMsalAmbaPBA$residencia_provincia_id<-as.character(dataMsalAmbaPBA$residencia_provincia_id)
  dataMsalAmbaPBA$residencia_provincia_id<-"7"
  dataMsalAmbaPBA$residencia_provincia_nombre<-"Argentina - Buenos Aires (Partidos del AMBA)"

  dataMsal<-union_all(dataMsal,dataMsalAmba)
  dataMsal<-union_all(dataMsal,dataMsalAmbaPBA)
  dataMsal<-union_all(dataMsal,dataMsalARG)
  dataMsal<-union_all(dataMsal,dataMsal_6_756)
  dataMsal<-union_all(dataMsal,dataMsal_6_826)
  
  dataMsal<-sqldf('
     select distinct "cases" as tipo,
     fecha_diagnostico as dateRep,
     residencia_provincia_nombre as countriesAndTerritories,
     "ARG_" || residencia_provincia_id as countryterritoryCode,
     sum(case
       when clasificacion_resumen="Confirmado" then 1 else 0 end) as count
     from dataMsal
     where fecha_diagnostico <> "" and residencia_provincia_nombre <> "SIN ESPECIFICAR"
     group by fecha_diagnostico,
     residencia_provincia_nombre
union all
     select distinct "deaths" as tipo,
     fecha_fallecimiento as dateRep,
     residencia_provincia_nombre as countriesAndTerritories,
     "ARG_" || residencia_provincia_id as countryterritoryCode,
     sum(case
       when clasificacion_resumen="Confirmado" then 1 else 0 end and fallecido="SI") as count
     from dataMsal
     where fecha_diagnostico <> "" and residencia_provincia_nombre <> "SIN ESPECIFICAR"
     group by fecha_fallecimiento,
     residencia_provincia_nombre
     ')
   
  
  dataMsal$countryterritoryCode[dataMsal$countryterritoryCode=="ARG_0"] <- "ARG"
  combinaciones=list(unique(dataMsal$tipo),
                   seq(as.Date(first(dataMsal$dateRep)),as.Date(last(dataMsal$dateRep)),by=1),
                   unique(dataMsal$countryterritoryCode))
  
  df_full<-data.frame(expand.grid(combinaciones)) %>% arrange(Var1,Var2) 
  
  colnames(df_full) <- c("tipo","dateRep","countryterritoryCode")
  
  dataMsal<-merge(df_full, dataMsal, all.x = TRUE) %>% arrange(tipo,countriesAndTerritories,dateRep)
  dataMsal<-dataMsal %>% arrange(dateRep, countryterritoryCode)
  dataMsal$count[is.na(dataMsal$count)]<-0
  dataMsal$countriesAndTerritories <- NULL
  dataMsal$tipo <- as.character(dataMsal$tipo) 
  dataMsal$dateRep <- as.character(dataMsal$dateRep) 
  dataMsal$countryterritoryCode <- as.character(dataMsal$countryterritoryCode)
  
  dataMsal<-dataMsal %>% filter(countryterritoryCode == input$pais & dateRep<=diaActualizacion)
  
  dataMsal<-spread(dataMsal, tipo, count) %>% filter(dateRep!="") %>% arrange(countryterritoryCode, dateRep)
  dataMsal$dateRep<-as.Date(dataMsal$dateRep)
  dataMsal[is.na(dataMsal)] <- 0
  dataMsal<-dataMsal %>% group_by(countryterritoryCode) %>% dplyr::mutate(total_cases=cumsum(cases),total_deaths=cumsum(deaths))
  
  dataEcdc<-data.frame(dataMsal %>% dplyr::select(dateRep,countryterritoryCode,new_cases=cases,new_deaths=deaths,total_cases,total_deaths))
  dataEcdc<-dataEcdc[,-2]
  dataEcdc<-dataEcdc %>% dplyr::filter(dateRep>="2020-03-01") 
  rm(dataMsal)
  rm(dataMsalAmba)
  rm(dataMsalAmbaPBA)
  rm(dataMsalARG)
  rm(dataMsal_6_756)
  rm(dataMsal_6_826)
  #   dataEcdc$new_deaths[6:nrow(dataEcdc)-6]<-rollmean(dataEcdc$new_deaths,7)
} else
  
{
  dataEcdc <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                       na.strings = "", fileEncoding = "UTF-8-BOM")

  dataEcdc$dateRep <- as.Date(dataEcdc$dateRep, format = "%d/%m/%Y")
  dataEcdc$dateRep <- format(dataEcdc$dateRep, "%Y-%m-%d")
  dataEcdc<-dataEcdc %>% filter(dateRep<=Sys.Date())
  dataEcdc$dateRep<-as.Date(dataEcdc$dateRep)
  
  dataEcdc <- dataEcdc %>% filter(countryterritoryCode==input$pais)
  dataEcdc <- dataEcdc %>% dplyr::select(fecha=dateRep,countryterritoryCode, cases, deaths)
  seqFecha<-seq(min(as.Date(dataEcdc$fecha)),max(as.Date(dataEcdc$fecha)), by=1 )
  seqFecha<-data.frame(secuencia=seqFecha)
  seqFecha$secuencia<-as.Date(seqFecha$secuencia)
  
  dataEcdc<-sqldf('
      select T1.*, T2.cases,T2.deaths from seqFecha as T1
      left join dataEcdc as T2 on
      T1.secuencia=T2.fecha
      ')
  dataEcdc<-data.frame(dataEcdc %>% dplyr::select(dateRep=secuencia,cases,deaths))
  
  dataEcdc <-
    mutate(dataEcdc, deaths = ifelse(is.na(deaths), 0, deaths))
  
  dataEcdc$cases[is.na(dataEcdc$cases)] <- 0
  
  dataEcdc <- dataEcdc %>%
    mutate(total_cases = cumsum(cases)) %>%
    mutate(total_deaths = cumsum(deaths))
  
  colnames(dataEcdc)[2] <- "new_cases"
  colnames(dataEcdc)[3] <- "new_deaths"
  
  dataEcdc <-
    dataEcdc %>% filter(
      dateRep <= diaActualizacion &
        total_cases > 0
    )
}

#### parametros epidemiológicos####

## Periodo preinfeccioso promedio (días)
periodoPreinfPromedio <- 5.84

## Duración media de la infecciosidad (días)
duracionMediaInf <- 4.8

## Porcentaje de casos severos
porcentajeCasosGraves <- 0.0328

## Porcentaje de casos críticos
porcentajeCasosCriticos <- 0.0054

## Días con síntomas antes de la hospitalización.
diasSintomasAntesHosp <- 7.12

## Días de hospitalización para casos severos.
diasHospCasosGraves <- 5.0

## Días de hospitalización para casos críticos.
diasHospCasosCriticos <- 23.0

## Días de la UCI para casos críticos
diasUCICasosCriticos <- 18.0

## Tasa letalidad
tasaLetalidadAjustada <- 0.0027

## Días desde el primer informe hasta la dinámica de la muerte: nuevo modelo
diasPrimerInformeMuerte <- 7.0

## Tiempo desde el final de la incubación hasta la muerte.
diasIncubacionMuerte <- 3.0

## Retraso para el impacto de la política (días)
retrasoImpactoPolitica <- 3.0

# Camas generales atendidas enfermera por día / número de turnos
camasGeneralesEnfermeraDia <- 2.66667

## Las camas de la UCI atendieron a la enfermera por día / número de turnos
camasUCIEnfermerasDia <- 0.66667

## Camas generales atendidas por día médico / número de turnos
camasGeneralesMedicoDia <- 4.0

## Camas CC atendidas por día médico / número de turnos
camasCCMedicoDia <- 4.0

## Ventiladores por cama crítica
ventiladoresCamaCritica <- 0.654

## Cantidad de días de la proyección
cantidadDiasProyeccion <- 1000

## Día de inicio
diaInicio <- '2020-02-12'

## Expuestos
expuestos <- 1

## Infectados
infectados <- 1

## Recuperados
recuperados <- 0

## Población
poblacion<-as.numeric(poblacion_data$value[which(poblacion_data$indicator=='total' & poblacion_data$pais==input$pais)])

##### Recursos #####
# asigna recursos según país
recursos <- read.csv("recursos.csv",sep=";") %>% filter(pais==input$pais)
camasGenerales <- recursos[,"camasGenerales"]
camasCriticas <- recursos[,"camasCriticas"]
ventiladores <- recursos[,"ventiladores"]
enfermerasCamasGenerales <- recursos[,"enfermerasCamasGenerales"]
enfermerasCamasUCI <- recursos[,"enfermerasCamasUCI"]
medicosCamasGenerales <- recursos[,"medicosCamasGenerales"]
medicosCamasUCI <- recursos[,"medicosCamasUCI"]
porcentajeDisponibilidadCamasCOVID <- recursos[,"porcentajeDisponibilidadCamasCOVID"]


#### Actualizar ####

# obtiene función seir
source("seir.R", encoding = "UTF-8")

# paises con infectados segun porcentaje no detectado
paises_distintos <- c("ARG_18","CRI","SLV","JAM",
                      #"PRY",
                      "ARG_50","BHS","BLZ","BRB",
                       "GUY","HTI","NIC","SUR","TTO","VEN")

# valores por default de intervención. En actualización nunca aplica trigger
default=TRUE
trigger_Porc_crit=60
trigger_R_inter=0.9
Dias_interv=30
trigger_on_app=0
fechaIntervencionesTrigger = c()

# El R0 que ingreso al comienzo no es relevante debido que
# al actualizar se se calcula, siguiendo ritmo de observado
# escenario principal

seir_update <- seir(actualiza = T, vent=ventiladores,
                tipo = ifelse(input$pais %in% paises_distintos,"B","A"),
                hoy_date = hoy, 
                R0_usuario = data.frame(Comienzo=hoy, 
                                     Final=as.Date("2021-10-09"), 
                                     R.modificado=1.2))
modeloSimulado <- seir_update$modeloSimulado

# creo objetos para app
r_cori <- seir_update$r_cori
Rusuario <- data.frame(Comienzo = max(dataEcdc$dateRep),
                       Final = max(dataEcdc$dateRep)+420,
                       R.modificado=r_cori)
resumenResultados <- crea_tabla_rr(modeloSimulado = modeloSimulado)
crea_tabla_inputs()

# escenario hi
seir_update_hi <- seir(actualiza = T, variacion = .25, 
                     tipo = ifelse(input$pais %in% paises_distintos,"B","A"),
                     hoy_date = hoy, 
                     R0_usuario = data.frame(Comienzo=hoy, 
                                             Final=as.Date("2021-10-09"), 
                                             R.modificado=1.2))
modeloSimulado_hi <- seir_update_hi$modeloSimulado

# escenario low
seir_update_low <- seir(actualiza = T, variacion = -.25,
                       tipo = ifelse(input$pais %in% paises_distintos,"B","A"),
                       hoy_date = hoy, 
                       R0_usuario = data.frame(Comienzo=hoy, 
                                               Final=as.Date("2021-10-09"), 
                                               R.modificado=1.2))
modeloSimulado_low <- seir_update_low$modeloSimulado
rm(seir_update)
rm(seir_update_hi)
rm(seir_update_low)

#### guarda conjunto de datos que serán levantados en la app####
save.image(paste0("DatosIniciales/DatosIniciales_",input$pais,".RData"))
print(input$pais)
}

#### update owd_data and mapa ####
source("owd_data.R", encoding = "UTF-8")
source("map_set.R", encoding = "UTF-8")


