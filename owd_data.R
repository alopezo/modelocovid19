library(tidyverse)
library(sqldf)
library(tidyr)
library(dplyr)


# vector paises
paises <-c("PRY","PRY_ASU","PRY_CEN")

#PARAGUAY
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

data_PAR <- data.frame(iso_code="PRY", dataEcdc)

# ASUNCION
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
  
data_ASU <- data.frame(iso_code="PRY_ASU", dataEcdc)


# CENTRAL
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

data_CEN <- data.frame(iso_code="PRY_CEN", dataEcdc)

data <- union_all(data_PAR,data_ASU)
data <- union_all(data,data_CEN)

owd_data <- data %>% dplyr::mutate(iso_code=iso_code, location=NA,
                date=dateRep,
                total_cases, new_cases,
                total_deaths, new_deaths,
                total_tests=NA, new_tests=NA,
                total_tests_per_thousand=NA, new_tests_per_thousand=NA,new_tests_smoothed_per_thousand=NA,
                stringency_index=NA, 
                population=NA,
                population_density=NA,
                aged_65_older=NA,
                life_expectancy=NA) %>% 
  filter(
         as.Date(date) <= as.Date(hoy))

rm(data_CEN)
rm(data_ASU)
rm(data_PAR)
# guarda input folder
# setwd("appTest")
save(owd_data, file =  "DatosIniciales/owd_data.RData")

# graphs
# gt <- owd_data %>% filter(as.Date(date) > as.Date("2020-03-01")) %>% 
#               dplyr::select(pais=location, date, total_tests_per_thousand) %>% 
#               ggplot()+ geom_step(aes(as.Date(date), 
#                                       as.integer(total_tests_per_thousand)))+
#               labs(x="Fecha", y="Test/Hab")+
#               theme_minimal() +
#               ggtitle("Test realizados (acum) cada 1000 hab.") +
#               facet_wrap(~pais)
# gsi <- owd_data %>% filter(as.Date(date) > as.Date("2020-03-01")) %>% 
#               dplyr::select(pais=iso_code, date, stringency_index) %>% 
#               ggplot()+ geom_step(aes(as.Date(date), 
#                                       as.numeric(stringency_index),
#                                       color=pais))+
#               labs(x="Fecha", y="Stringency Index")+
#               theme_minimal() +
#               ggtitle("Stringency Index")+
#               facet_wrap(~pais)
#               # ggplot2::annotate("text",x = as.Date("2020-05-01"),
#               #                   y = 30,label="Government Response Stringency Index: composite 
#               #                   measure based on 9 response indicators including 
#               #                   school closures, workplace closures, and travel bans, 
#               #                   rescaled to a value from 0 to 100 (100 = strictest response)")
