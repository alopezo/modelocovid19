#### ACTUALIZACION ####
# se actualizan insumos para app, recalculando curvas en base a últimas observaciones
# se utiilzan tres fuentes ppalmente: Ecdc (online), oms (online), owd (online), MSal (descarga manual).
# la funcionalidad de los links de descarga debe ser revisada periódicamente
# una vez organizada la info, se apica función seir.

# librerías
library(tidyr)
library(tidyverse)
library(readxl)
library(sqldf)
library(readxl)
library(zoo)
library(EpiEstim)


##### DESCARGA DATOS DE OWD ####
dataEcdcFull <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv", 
                         na.strings = "", 
                         fileEncoding = "UTF-8-BOM")



#### DEFINE DIA DE ACTUALIZACION Y PAISES ####

hoy <<- diaActualizacion <<- as.Date("2021-04-12")
paises_actualizar <- c(
                       "BOL","CRI","SLV","ECU","GTM","HND",
                       "JAM","PAN","PRY","DOM","CHL","NIC",
                       "URY","BRA","PER","MEX","COL","BHS",
                       "BRB","BLZ","GUY","HTI","SUR","TTO",
                       "VEN","ARG","ARG_18", "ARG_2", "ARG_6", 
                       "ARG_7", "ARG_50", "ARG_3", "ARG_6_826","ARG_6_756"
                       )

##### CARGA POBLACIONES Y DATOS DE OMS  ####
load("DatosIniciales/poblacion_data.RData")
source("oms_data.R", encoding = "UTF-8")

##### DESCARGA DATOS DE ARGENTINA  ####
urlMsal <- 'https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.zip'
download.file(urlMsal, "Covid19Casos.zip")
unzip("Covid19Casos.zip")

##### PREPARA DF DE CADA PAIS #####
input=list()
for(p in paises_actualizar){
input$pais = p

##### SI ES ARGENTINA #####
if (substr(input$pais,1,3)=="ARG"){

  # identifica deptos del amba
  deptosAmba <- c(28,35,91,98,119,126,134,245,252,260,266,270,274,329,364,371,408,410,412,427,441,434,490,497,515,525,539,560,568,638,648,658,749,756,760,778,805,840,861,882)
  
  # lee dataset, selecciona confirmados y columnas necesarias y crea df para provincias
  dataMsal <- read.csv("Covid19Casos.csv", fileEncoding = "UTF-8") %>% dplyr::filter(clasificacion_resumen=="Confirmado") %>%
    dplyr::select(edad,
                  edad_años_meses,
                  fecha_diagnostico,
                  fecha_fallecimiento,
                  fallecido,
                  clasificacion_resumen,
                  residencia_provincia_id,
                  residencia_departamento_id,
                  residencia_provincia_nombre) %>%
    dplyr::mutate(gredad=case_when(edad_años_meses=="Meses" | edad_años_meses=="Años" & edad>=1 & edad <=19 ~ "00 a 19",
                                   edad_años_meses=="Años" & edad>=20 & edad <=64 ~ "20 a 64",
                                   edad_años_meses=="Años" & edad>=65 & edad <=110 ~ "65 y más",
                                   TRUE ~ "Sin esp."),
                  residencia_provincia_id=as.character(residencia_provincia_id))
 
  # df pais
  dataMsalARG <- dataMsal %>% dplyr::mutate(residencia_provincia_id="0",
                                            residencia_provincia_nombre="Argentina")
  
  # df depto 756 pba
  dataMsal_6_756 <- dataMsal %>% dplyr::filter(residencia_provincia_id==6 & residencia_departamento_id==756) %>%
    dplyr::mutate(residencia_provincia_id = "6_756",
                  residencia_provincia_nombre = "Buenos Aires - Partido de San Isidro")
  
  # df depto 826 pba
  dataMsal_6_826 <- dataMsal %>% dplyr::filter(residencia_provincia_id==6 & residencia_departamento_id==756) %>%
    dplyr::mutate(residencia_provincia_id = "6_826",
                  residencia_provincia_nombre = "Buenos Aires - Partido de Trenque Lauquen")
  
  # df amba
  dataMsalAmba<-dataMsal %>% dplyr::filter(residencia_provincia_id==2 | residencia_provincia_id==6 & residencia_departamento_id %in% (deptosAmba)) %>%
    dplyr::mutate(residencia_provincia_id = "3",
                  residencia_provincia_nombre = "AMBA")
  
  # df amba pba
  dataMsalAmbaPBA <- dataMsal %>% dplyr::filter(residencia_provincia_id==6 & residencia_departamento_id %in% (deptosAmba)) %>%
    dplyr::mutate(residencia_provincia_id = "7",
                  residencia_provincia_nombre = "Argentina - Buenos Aires (Partidos del AMBA)")
  
  # une todos 
  dataMsal <- bind_rows(dataMsal,
                        dataMsalAmba,
                        dataMsalAmbaPBA,
                        dataMsalARG,
                        dataMsal_6_756,
                        dataMsal_6_826)
  
  # da formato a df con grupos de edad
  casos <- dataMsal %>% dplyr::group_by(dateRep=as.Date(fecha_diagnostico),
                                        countriesAndTerritories=residencia_provincia_nombre,
                                        countryterritoryCode=paste0("ARG_",residencia_provincia_id)) %>%
    dplyr::summarise(new_cases=n(),
                     nc0019=sum(gredad=="00 a 19"),
                     nc2064=sum(gredad=="20 a 64"),
                     nc6599=sum(gredad=="65 y más"))
  
  muertes <- dataMsal %>% dplyr::filter(fallecido=="SI") %>%
    dplyr::group_by(dateRep=as.Date(fecha_fallecimiento),
                    countriesAndTerritories=residencia_provincia_nombre,
                    countryterritoryCode=paste0("ARG_",residencia_provincia_id)) %>%
    dplyr::summarise(new_deaths=n(),
                     nd0019=sum(gredad=="00 a 19"),
                     nd2064=sum(gredad=="20 a 64"),
                     nd6599=sum(gredad=="65 y más"))
  
  combinaciones=list(seq(min(casos$dateRep[is.na(casos$dateRep)==F & casos$dateRep>="2020-03-01"]),max(casos$dateRep[is.na(casos$dateRep)==F]),by=1),
                     unique(casos$countryterritoryCode))
  
  # df final
  dataEcdc <- 
    expand.grid(combinaciones) %>% dplyr::mutate(Var1=as.Date(Var1)) %>% 
    dplyr::rename(dateRep=Var1,
                  countryterritoryCode=Var2) %>%
    dplyr::left_join(casos, by=c("dateRep", "countryterritoryCode")) %>%
    dplyr::left_join(muertes, by=c("dateRep", "countryterritoryCode")) %>%
    dplyr::select(-countriesAndTerritories.x,-countriesAndTerritories.y) %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    dplyr::mutate(countryterritoryCode=case_when(countryterritoryCode=="ARG_0" ~ "ARG",
                                                 TRUE ~ countryterritoryCode)) %>%
    dplyr::group_by(countryterritoryCode) %>%
    mutate(total_cases=cumsum(new_cases),
           total_deaths=cumsum(new_deaths)) %>%
    dplyr::filter(countryterritoryCode == input$pais & dateRep<=diaActualizacion) %>%
    ungroup() %>%
    dplyr::select(-countryterritoryCode)
  
  # limpia entorno
  rm(casos)
  rm(muertes)
  rm(dataMsal)
  rm(dataMsalARG)
  rm(dataMsalAmbaPBA)
  rm(dataMsalAmba)
  rm(dataMsal_6_826)
  rm(dataMsal_6_756)
  rm(dataEcdcFull)
  
} else
  
##### SI ES OTRO PAIS #####
  
{
  # lee datos OWD
  dataEcdc <- dataEcdcFull

  # da formato
  dataEcdc <- dataEcdc %>% dplyr::rename(dateRep=date,
                                         countryterritoryCode=iso_code,
                                         cases=new_cases,
                                         deaths=new_deaths) %>%
                           dplyr::filter(dateRep<=Sys.Date(),
                                         countryterritoryCode==input$pais) %>%
                           dplyr::mutate(dateRep=as.Date(dateRep)) %>%
                           dplyr::select(dateRep,countryterritoryCode, cases, deaths) 
  
  # agrega dias vacios y frecuencias acumuladas                         
  dataEcdc <- dplyr::left_join(data.frame(dateRep=as.Date(seq(min(as.Date(dataEcdc$date)),max(as.Date(dataEcdc$date)), by=1))), dataEcdc) %>%
              dplyr::select(dateRep=dateRep,cases,deaths) %>%
              dplyr::mutate(deaths = ifelse(is.na(deaths), 0, deaths),
                            cases = ifelse(is.na(cases), 0, cases),
                            total_cases = cumsum(cases),
                            total_deaths = cumsum(deaths)) %>%
                            dplyr::rename(new_cases=cases,
                            new_deaths=deaths) %>%
                            dplyr::filter(dateRep <= diaActualizacion &
                            total_cases > 0)
  

}

##### DEFINE PARAMETROS EPIDEMIOLOGICOS #####

## Periodo preinfeccioso promedio (días)
periodoPreinfPromedio <- 5.84

## Duración media de la infecciosidad (días)
duracionMediaInf <- 4.8

## Porcentaje de casos severos
porcentajeCasosGraves <- 0.0328

## Porcentaje de casos críticos
porcentajeCasosCriticos <- 0.0108

## Días con síntomas antes de la hospitalización.
diasSintomasAntesHosp <- 7.12

## Días de hospitalización para casos severos.
diasHospCasosGraves <- 5.0

## Días de hospitalización para casos críticos.
diasHospCasosCriticos <- 23.0

## Días de la UCI para casos críticos
diasUCICasosCriticos <- 18.0

## Tasa letalidad
tasaLetalidadAjustada <- 0.0033

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

##### CARGA INFROMACION DE RECURSOS POR PAIS #####
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


##### ACTUALIZA MODELO #####

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

# FIX NUMEROS NEGATIVOS #
modeloSimulado$muertesDiariasProyeccion[is.nan(modeloSimulado$muertesDiariasProyeccion)==T] <- 0
modeloSimulado$muertesDiariasProyeccion[modeloSimulado$muertesDiariasProyeccion<1] <- 0
modeloSimulado$HHRR.criticCareBeds[is.nan(modeloSimulado$HHRR.criticCareBeds)==T] <- 0
modeloSimulado$HHRR.criticCareBeds[modeloSimulado$HHRR.criticCareBeds<1] <- 0
modeloSimulado$HHRR.SAT.criticCareBeds[is.nan(modeloSimulado$HHRR.SAT.criticCareBeds)==T] <- 0
modeloSimulado$HHRR.SAT.criticCareBeds[modeloSimulado$HHRR.SAT.criticCareBeds<1] <- 0
modeloSimulado$HHRR.ventilators[is.nan(modeloSimulado$HHRR.ventilators)==T] <- 0
modeloSimulado$HHRR.ventilators[modeloSimulado$HHRR.ventilators<1] <- 0
modeloSimulado$HHRR.SAT.ventilators[is.nan(modeloSimulado$HHRR.SAT.ventilators)==T] <- 0
modeloSimulado$HHRR.SAT.ventilators[modeloSimulado$HHRR.SAT.ventilators<1] <- 0
modeloSimulado$i_5d_ma[is.nan(modeloSimulado$i_5d_ma)==T] <- 0
modeloSimulado$i_5d_ma[modeloSimulado$i_5d_ma<1] <- 0


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

#### GUARDA ARCHIVO RDATA DE CADA PAIS ####

save(list=ls()[!ls() %in% c("dataEcdcFull")], file=paste0("DatosIniciales/DatosIniciales_",input$pais,".RData"))
#save.image(paste0("DatosIniciales/DatosIniciales_",input$pais,".RData"))
print(input$pais)
}

#### ACTUALIZA DATOS PARA RESUMEN Y MAPA ####
source("owd_data.R", encoding = "UTF-8")
source("map_set.R", encoding = "UTF-8")


