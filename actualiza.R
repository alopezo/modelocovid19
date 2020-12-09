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
setwd("C:/Users/Adrian/Desktop/branchPRY")
hoy <<- diaActualizacion <<- as.Date("2020-11-17")
paises_actualizar <- c("PRY","PRY_ASU","PRY_CEN")

##### carga población y oms data  ####
load("DatosIniciales/poblacion_data.RData")
source("owd_data.R", encoding = "UTF-8")
source("oms_data.R", encoding = "UTF-8")



#### casos/muertes y parámetros para cada país ####
input=list()
for(p in paises_actualizar){
input$pais = p

if (p=="PRY")
{
  ##### descarga ultimos datos  ####
  casos_PRY <- read.csv2("DatosPRY/Descargar_datos_data.csv", encoding = "UTF-8")
  muertes_PRY <- read.csv2("DatosPRY/FALLECIDOS_data.csv", encoding = "UTF-8")
  casos_PRY$Fecha.Confirmacion <-  as.Date(casos_PRY$Fecha.Confirmacion, format = "%d/%m/%y")
  casos_PRY <- casos_PRY %>% group_by(Fecha.Confirmacion) %>% tally()
  seq <- data.frame(Fecha.Confirmacion=seq(min(casos_PRY$Fecha.Confirmacion),max(casos_PRY$Fecha.Confirmacion),by=1))  
  casos_PRY <- merge(seq,casos_PRY,all.x=TRUE)
  casos_PRY$n[is.na(casos_PRY$n)==T] <- 0
  colnames(casos_PRY) <- c("date","new_cases")
  
  muertes_PRY$Fecha.de.Divulgación <-  as.Date(muertes_PRY$Fecha.de.Divulgación, format = "%d/%m/%y")
  muertes_PRY <- muertes_PRY %>% group_by(Fecha.de.Divulgación) %>% tally()
  colnames(muertes_PRY) <- c("date","new_deaths")
  
  dataEcdc <- merge(casos_PRY, muertes_PRY, all.x=T)
  dataEcdc$new_deaths[is.na(dataEcdc$new_deaths)==T] <- 0
  colnames(dataEcdc) <- c("dateRep","new_cases","new_deaths")
  dataEcdc <- dataEcdc %>% mutate(total_cases=cumsum(new_cases),
                                  total_deaths=cumsum(new_deaths))
  
} else
  
  if (p=="PRY_ASU")
  {
    ##### descarga ultimos datos  ####
    casos_PRY <- read.csv2("DatosPRY/Descargar_datos_data.csv", encoding = "UTF-8")
    muertes_PRY <- read.csv2("DatosPRY/FALLECIDOS_data.csv", encoding = "UTF-8")
    casos_PRY <- casos_PRY[casos_PRY$Departamento.Residencia=="ASUNCION",]
    muertes_PRY <- muertes_PRY[muertes_PRY$Departamento.Residencia=="ASUNCION",]
    
    casos_PRY$Fecha.Confirmacion <-  as.Date(casos_PRY$Fecha.Confirmacion, format = "%d/%m/%y")
    casos_PRY <- casos_PRY %>% group_by(Fecha.Confirmacion) %>% tally()
    seq <- data.frame(Fecha.Confirmacion=seq(min(casos_PRY$Fecha.Confirmacion),max(casos_PRY$Fecha.Confirmacion),by=1))  
    casos_PRY <- merge(seq,casos_PRY,all.x=TRUE)
    casos_PRY$n[is.na(casos_PRY$n)==T] <- 0
    colnames(casos_PRY) <- c("date","new_cases")
    
    muertes_PRY$Fecha.de.Divulgación <-  as.Date(muertes_PRY$Fecha.de.Divulgación, format = "%d/%m/%y")
    muertes_PRY <- muertes_PRY %>% group_by(Fecha.de.Divulgación) %>% tally()
    colnames(muertes_PRY) <- c("date","new_deaths")
    
    dataEcdc <- merge(casos_PRY, muertes_PRY, all.x=T)
    dataEcdc$new_deaths[is.na(dataEcdc$new_deaths)==T] <- 0
    colnames(dataEcdc) <- c("dateRep","new_cases","new_deaths")
    dataEcdc <- dataEcdc %>% mutate(total_cases=cumsum(new_cases),
                                    total_deaths=cumsum(new_deaths))
    
  } else
  
     if (p=="PRY_CEN")
      {
       ##### descarga ultimos datos  ####
       casos_PRY <- read.csv2("DatosPRY/Descargar_datos_data.csv", encoding = "UTF-8")
       muertes_PRY <- read.csv2("DatosPRY/FALLECIDOS_data.csv", encoding = "UTF-8")
        casos_PRY <- casos_PRY[casos_PRY$Departamento.Residencia=="CENTRAL",]
        muertes_PRY <- muertes_PRY[muertes_PRY$Departamento.Residencia=="CENTRAL",]
        
        casos_PRY$Fecha.Confirmacion <-  as.Date(casos_PRY$Fecha.Confirmacion, format = "%d/%m/%y")
        casos_PRY <- casos_PRY %>% group_by(Fecha.Confirmacion) %>% tally()
        seq <- data.frame(Fecha.Confirmacion=seq(min(casos_PRY$Fecha.Confirmacion),max(casos_PRY$Fecha.Confirmacion),by=1))  
        casos_PRY <- merge(seq,casos_PRY,all.x=TRUE)
        casos_PRY$n[is.na(casos_PRY$n)==T] <- 0
        colnames(casos_PRY) <- c("date","new_cases")
        
        muertes_PRY$Fecha.de.Divulgación <-  as.Date(muertes_PRY$Fecha.de.Divulgación, format = "%d/%m/%y")
        muertes_PRY <- muertes_PRY %>% group_by(Fecha.de.Divulgación) %>% tally()
        colnames(muertes_PRY) <- c("date","new_deaths")
        
        dataEcdc <- merge(casos_PRY, muertes_PRY, all.x=T)
        dataEcdc$new_deaths[is.na(dataEcdc$new_deaths)==T] <- 0
        colnames(dataEcdc) <- c("dateRep","new_cases","new_deaths")
        dataEcdc <- dataEcdc %>% mutate(total_cases=cumsum(new_cases),
                                        total_deaths=cumsum(new_deaths))
        
      }    


rm(casos_PRY)
rm(muertes_PRY)

rm(seq)


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
source("map_set.R", encoding = "UTF-8")


