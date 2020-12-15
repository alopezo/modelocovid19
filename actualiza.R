#### Actualizar contenido ####
# se actualizan insumos para app, recalculando curvas en base a últimas observaciones
# se utiilzan tres fuentes ppalmente: Ecdc (online), oms (online), owd (online), MSal (descarga manual).
# la funcionalidad de los links de descarga debe ser revisada periódicamente
# una vez organizada la info, se apica función seir.

# librerías
library(tidyverse)
library(tidyr)
library(readxl)
library(sqldf)
library(readxl)
library(zoo)
library(EpiEstim)

#### países/juris a actualizar ####

hoy <<- diaActualizacion <<- as.Date("2020-12-05")
paises_actualizar <- c("ARG","BOL","CRI","SLV","ECU","GTM",
                       "HND","JAM","PAN","PRY","DOM","CHL","NIC",
                       "URY","BRA","PER","MEX","COL", "BHS",
                       "BRB","BLZ","GUY","HTI","SUR","TTO","VEN",
                       "ARG_18", "ARG_2", "ARG_6", "ARG_7", "ARG_50", "ARG_3", "ARG_6_826","ARG_6_756")

paisesEdad <<- c("ARG")

##### carga población y oms data  ####
load("DatosIniciales/poblacion_data.RData")
source("oms_data.R", encoding = "UTF-8")

##### descarga ultimos datos de msal  ####
urlMsal <- 'https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.csv'
download.file(urlMsal, "Covid19Casos.csv")

urlEcdc <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
download.file(urlEcdc, "dataEcdc.csv")

#### casos/muertes y parámetros para cada país ####
input=list()
for(p in paises_actualizar){
input$pais = p

if (substr(input$pais,1,3)=="ARG"){
  
  dataMsal<-read.csv("Covid19Casos.csv", fileEncoding = "UTF-8")
  dataMsal<-dataMsal %>% filter(clasificacion_resumen=="Confirmado" & is.na(edad)==F)
  dataMsal<-dataMsal %>% mutate(grupedad = case_when(
                                              edad_años_meses=="Meses" | 
                                              edad_años_meses=="Años" & edad >= 1 & edad <= 9 ~ "gr_00_09",
                                              edad_años_meses=="Años" & edad >= 10 & edad <= 14 ~ "gr_10_14",
                                              edad_años_meses=="Años" & edad >= 15 & edad <= 19 ~ "gr_15_19",
                                              edad_años_meses=="Años" & edad >= 20 & edad <= 24 ~ "gr_20_24",
                                              edad_años_meses=="Años" & edad >= 25 & edad <= 29 ~ "gr_25_29",
                                              edad_años_meses=="Años" & edad >= 30 & edad <= 34 ~ "gr_30_34",
                                              edad_años_meses=="Años" & edad >= 35 & edad <= 39 ~ "gr_35_39",
                                              edad_años_meses=="Años" & edad >= 40 & edad <= 44 ~ "gr_40_44",
                                              edad_años_meses=="Años" & edad >= 45 & edad <= 49 ~ "gr_45_49",
                                              edad_años_meses=="Años" & edad >= 50 & edad <= 54 ~ "gr_50_54",
                                              edad_años_meses=="Años" & edad >= 55 & edad <= 59 ~ "gr_55_59",
                                              edad_años_meses=="Años" & edad >= 60 & edad <= 64 ~ "gr_60_64",
                                              edad_años_meses=="Años" & edad >= 65 & edad <= 69 ~ "gr_65_69",
                                              edad_años_meses=="Años" & edad >= 70 & edad <= 74 ~ "gr_70_74",
                                              edad_años_meses=="Años" & edad >= 75 & edad <= 79 ~ "gr_75_79",
                                              edad_años_meses=="Años" & edad >= 80 & edad <= 84 ~ "gr_80_84",
                                              edad_años_meses=="Años" & edad >= 85 & edad <= 89 ~ "gr_85_89",
                                              edad_años_meses=="Años" & edad >= 90 & edad <= 150 ~ "gr_90_mas")
  )
  
  dataMsal <- dataMsal %>% filter(is.na(edad)==FALSE)
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
  
dataMsal$spread <- 1
dataMsal <-  spread(dataMsal, grupedad, spread, fill = 0, convert = FALSE, drop = TRUE, sep = NULL)

#hacer count de casos y muertes  
dataMsal <- 
  union_all(
    dataMsal %>% dplyr::filter(clasificacion_resumen=="Confirmado" & fecha_diagnostico>="2020-03-01") %>%
                 dplyr::group_by(dateRep=fecha_diagnostico,countryterritoryCode=paste0("ARG_",residencia_provincia_id))  %>%
                 dplyr::summarise(count=n(),
                                  gr_00_09=sum(gr_00_09),
                                  gr_10_14=sum(gr_10_14),
                                  gr_15_19=sum(gr_15_19),
                                  gr_20_24=sum(gr_20_24),
                                  gr_25_29=sum(gr_25_29),
                                  gr_30_34=sum(gr_30_34),
                                  gr_35_39=sum(gr_35_39),
                                  gr_40_44=sum(gr_40_44),
                                  gr_45_49=sum(gr_45_49),
                                  gr_50_54=sum(gr_50_54),
                                  gr_55_59=sum(gr_55_59),
                                  gr_60_64=sum(gr_60_64),
                                  gr_65_69=sum(gr_65_69),
                                  gr_70_74=sum(gr_70_74),
                                  gr_75_79=sum(gr_75_79),
                                  gr_80_84=sum(gr_80_84),
                                  gr_85_89=sum(gr_85_89),
                                  gr_90_  =sum(gr_90_mas)
                                  ) %>%
                 dplyr::mutate(tipo="cases")%>%
                 dplyr::arrange(countryterritoryCode,dateRep),
    
    dataMsal %>% dplyr::filter(fallecido=="SI" & clasificacion_resumen=="Confirmado" & fecha_diagnostico>="2020-03-01") %>%
                 dplyr::group_by(dateRep=fecha_fallecimiento,countryterritoryCode=paste0("ARG_",residencia_provincia_id))  %>%
                 dplyr::summarise(count=n(),
                                  gr_00_09=sum(gr_00_09),
                                  gr_10_14=sum(gr_10_14),
                                  gr_15_19=sum(gr_15_19),
                                  gr_20_24=sum(gr_20_24),
                                  gr_25_29=sum(gr_25_29),
                                  gr_30_34=sum(gr_30_34),
                                  gr_35_39=sum(gr_35_39),
                                  gr_40_44=sum(gr_40_44),
                                  gr_45_49=sum(gr_45_49),
                                  gr_50_54=sum(gr_50_54),
                                  gr_55_59=sum(gr_55_59),
                                  gr_60_64=sum(gr_60_64),
                                  gr_65_69=sum(gr_65_69),
                                  gr_70_74=sum(gr_70_74),
                                  gr_75_79=sum(gr_75_79),
                                  gr_80_84=sum(gr_80_84),
                                  gr_85_89=sum(gr_85_89),
                                  gr_90_  =sum(gr_90_mas)) %>%
      dplyr::mutate(tipo="deaths")%>%
      dplyr::arrange(countryterritoryCode,dateRep)
  )

  dataMsal$countryterritoryCode[dataMsal$countryterritoryCode=="ARG_0"] <- "ARG"
  combinaciones=list(unique(dataMsal$tipo),
                   seq(as.Date(min(dataMsal$dateRep)),as.Date(max(dataMsal$dateRep)),by=1),
                   unique(dataMsal$countryterritoryCode))
  

  
  df_full<-data.frame(expand.grid(combinaciones)) %>% arrange(Var1,Var2) 
  
  colnames(df_full) <- c("tipo","dateRep","countryterritoryCode")
  
  dataMsal<-merge(df_full, dataMsal, all.x = TRUE)
  dataMsal<-dataMsal %>% arrange(dateRep, countryterritoryCode)
  dataMsal$count[is.na(dataMsal$count)]<-0
  dataMsal$tipo <- as.character(dataMsal$tipo) 
  dataMsal$dateRep <- as.character(dataMsal$dateRep) 
  dataMsal$countryterritoryCode <- as.character(dataMsal$countryterritoryCode)
  dataMsal<-dataMsal %>% filter(countryterritoryCode == input$pais & dateRep<=diaActualizacion)
  dataMsal<- merge(dataMsal %>% filter(tipo=="cases"),dataMsal %>% filter(tipo=="deaths"), by=c("dateRep"))
  cn <- str_replace(colnames(dataMsal),'.x','_cases')
  cn <- str_replace(cn,'.y','_deaths')
  colnames(dataMsal) <- cn
  dataMsal$tipo_cases <- NULL
  dataMsal$tipo_deaths <- NULL
  dataMsal$count_deathsterritoryCode_deaths <- NULL 
  colnames(dataMsal)[2] <-"countryterritoryCode" 
  dataMsal$count_deathsterritoryCode.y <- NULL
  dataMsal$cases <- dataMsal$count_cases
  dataMsal$deaths <- dataMsal$count_deaths
  dataMsal$count_cases <- NULL
  dataMsal$count_deaths <- NULL
  
  #dataMsal<-spread(dataMsal, tipo, count) %>% filter(dateRep!="") %>% arrange(countryterritoryCode, dateRep)
  dataMsal$dateRep<-as.Date(dataMsal$dateRep)
  dataMsal[is.na(dataMsal)] <- 0
  dataMsal<-dataMsal %>% dplyr::group_by(countryterritoryCode) %>% dplyr::mutate(total_cases=cumsum(cases),total_deaths=cumsum(deaths))
  
  dataEcdc<-data.frame(dataMsal %>% dplyr::mutate(new_cases=cases,new_deaths=deaths))
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

  dataEcdc <- read.csv("dataEcdc.csv", fileEncoding = "UTF-8-BOM")

  
  
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

# 
# library(ggplot2)
# data <- data.frame(fecha=rep(modeloSimulado$fecha,3),
#                    edad=c(rep("Menores de 20",nrow(modeloSimulado)),
#                           rep("Menores de 20 a 59",nrow(modeloSimulado)),
#                           rep("Menores de 60 y más",nrow(modeloSimulado))),
#                    casos=c(modeloSimulado$incid_00_19,modeloSimulado$incid_20_59,modeloSimulado$incid_60_mas))
# data$casos[is.na(data$casos)==T] <- 0
# 
# plot <-
# ggplot(data, aes(x=fecha, y=casos, fill=edad)) +
#   geom_area() + geom_vline(xintercept=hoy)
# library(plotly)
# ggplotly(plot)
# 
# view(modeloSimulado$i_5d_ma)
