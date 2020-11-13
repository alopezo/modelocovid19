library(tidyverse)
library(sqldf)
library(tidyr)
library(dplyr)

# vector paises
paises <-c("ARG","BOL","BRA","CHL","COL","CRI","SLV","CEA",
           "ECU","GTM","HND","JAM","MEX","PAN","PRY","PER","DOM","URY","ARG_2","ARG_18","ARG_3","ARG_7", "ARG_50",
           "BHS", "BRB", "BLZ", "GUY", "HTI", "NIC", "SUR", "TTO", "VEN")

# get data
# Our World In Data. Deaths and Cases from ECDC
owd_data <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",header = T) %>% 
  dplyr::select(iso_code, location,
                date,
                total_cases, new_cases,
                total_deaths, new_deaths,
                total_tests, new_tests,
                total_tests_per_thousand, new_tests_per_thousand,new_tests_smoothed_per_thousand,
                stringency_index, 
                population,
                population_density,
                aged_65_older,
                life_expectancy) %>% 
  filter(iso_code %in% paises,
         as.Date(date) <= as.Date(hoy))

#agrega datos subnacionales de Argentina

dataMsal<-read.csv("appTest - Cod/Covid19Casos.csv", encoding = "UTF-8")
dataMsal<-dataMsal %>% filter(clasificacion_resumen=="Confirmado")
deptosAmba<-c(28,
              35,
              91,
              98,
              119,
              126,
              134,
              245,
              252,
              260,
              266,
              270,
              274,
              329,
              364,
              371,
              408,
              410,
              412,
              427,
              441,
              434,
              490,
              497,
              515,
              525,
              539,
              560,
              568,
              638,
              648,
              658,
              749,
              756,
              760,
              778,
              805,
              840,
              861,
              882)
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


dataMsal<-dataMsal %>% filter(countryterritoryCode %in% paises & dateRep<=hoy)


dataMsal<-spread(dataMsal, tipo, count) %>% filter(dateRep!="") %>% arrange(countryterritoryCode, dateRep)
dataMsal$dateRep<-as.Date(dataMsal$dateRep)
dataMsal[is.na(dataMsal)] <- 0
dataMsal<-dataMsal %>% group_by(countryterritoryCode) %>% dplyr::mutate(total_cases=cumsum(cases),total_deaths=cumsum(deaths))


dataMsal$location<-""
dataMsal$total_tests<-as.numeric(NA)
dataMsal$new_tests<-as.numeric(NA)
dataMsal$total_tests_per_thousand<-as.numeric(NA)
dataMsal$new_tests_per_thousand<-as.numeric(NA)
dataMsal$new_tests_smoothed_per_thousand<-as.numeric(NA)
dataMsal$stringency_index<-as.numeric(NA)
dataMsal$population<-as.numeric(NA)
dataMsal$population_density<-as.numeric(NA)
dataMsal$aged_65_older<-as.numeric(NA)
dataMsal$life_expectancy<-as.numeric(NA)

dataMsal<-
  dataMsal %>% dplyr::select(
    iso_code=countryterritoryCode,
    location=location,
    date=dateRep,
    total_cases,
    new_cases=cases,
    total_deaths,
    new_deaths=deaths,
    total_tests,
    new_tests,
    total_tests_per_thousand,
    new_tests_per_thousand,
    new_tests_smoothed_per_thousand,
    stringency_index,
    population,
    population_density,
    aged_65_older,
    life_expectancy
  )
dataMsal$date<-as.character(dataMsal$date)
dataMsal<-dataMsal %>% filter(date<='2020-08-30')
dataMsal$location[dataMsal$iso_code=="ARG_2"]<-"Argentina - Ciudad Autónoma de Buenos Aires"
dataMsal$population[dataMsal$iso_code=="ARG_2"]<-3075643
dataMsal$aged_65_older[dataMsal$iso_code=="ARG_2"]<-16.43
dataMsal$life_expectancy[dataMsal$iso_code=="ARG_2"]<-79
dataMsal$location[dataMsal$iso_code=="ARG_18"]<-"Argentina - Corrientes"
dataMsal$population[dataMsal$iso_code=="ARG_18"]<-1120801
dataMsal$aged_65_older[dataMsal$iso_code=="ARG_18"]<-9.84
dataMsal$life_expectancy[dataMsal$iso_code=="ARG_18"]<-78
dataMsal<-data.frame(dataMsal)
dataMsal$location[dataMsal$iso_code=="ARG_3"]<-"Argentina - AMBA"
dataMsal$population[dataMsal$iso_code=="ARG_3"]<-16706015
dataMsal$aged_65_older[dataMsal$iso_code=="ARG_3"]<-11.94
dataMsal$life_expectancy[dataMsal$iso_code=="ARG_3"]<-0
dataMsal$location[dataMsal$iso_code=="ARG_7"]<-"Argentina - Buenos Aires (Partidos del AMBA)"
dataMsal$population[dataMsal$iso_code=="ARG_7"]<-13630369
dataMsal$aged_65_older[dataMsal$iso_code=="ARG_7"]<-10.92
dataMsal$life_expectancy[dataMsal$iso_code=="ARG_7"]<-0
dataMsal$location[dataMsal$iso_code=="ARG_50"]<-"Argentina - Mendoza"
dataMsal$population[dataMsal$iso_code=="ARG_50"]<-1990338
dataMsal$aged_65_older[dataMsal$iso_code=="ARG_50"]<-12.89
dataMsal$life_expectancy[dataMsal$iso_code=="ARG_50"]<-79


##### CEARA #####

# download, extract and import  

download.file("http://download-integrasus.saude.ce.gov.br/casos_covid19", 
              destfile = "casos_covid19.zip", 
              mode="wb")

dataCeara <- read.csv(unzip("casos_covid19.zip"), sep=";")

# prepare data

dataCeara$dataInicioSintomas <- as.Date(dataCeara$dataInicioSintomas, format="%d-%m-%Y") #date format
dataCeara$dataObito <- as.Date(dataCeara$dataObito, format="%d-%m-%Y") #date format

firstDate <- min(dataCeara$dataInicioSintomas[is.na(dataCeara$dataInicioSintomas)==FALSE]) #first day of sequence
lastDate <- max(dataCeara$dataInicioSintomas[is.na(dataCeara$dataInicioSintomas)==FALSE]) #last day of sequence

cases <- dataCeara[is.na(dataCeara$dataInicioSintomas)==FALSE & 
                     dataCeara$resultadoFinalExame=="Positivo",]

cases <- cases %>% dplyr::group_by(dataInicioSintomas) %>% tally() 


seq <- data.frame(dateRep=seq(firstDate,lastDate, by=1))

cases <- merge(seq, cases, by.x="dateRep", by.y="dataInicioSintomas")

deaths <- dataCeara[is.na(dataCeara$dataInicioSintomas)==FALSE & 
                      dataCeara$obitoConfirmado=="True",]

deaths <- deaths %>% dplyr::group_by(dataObito) %>% tally() 
dataCeara <- merge(cases,deaths, by.x="dateRep", by.y="dataObito", all.x=TRUE)
colnames(dataCeara)[2:3] <- c("new_cases","new_deaths")
dataCeara$new_cases[is.na(dataCeara$new_cases)==TRUE] <- 0
dataCeara$new_deaths[is.na(dataCeara$new_deaths)==TRUE] <- 0

dataCeara <- dataCeara %>% mutate(total_cases=cumsum(new_cases), total_deaths=cumsum(new_deaths))
dataCeara$iso_code <- "CEA"
dataCeara$location <- "Ceara" 
dataCeara$date <- as.character(dataCeara$dateRep) 

# dataCeara <- dataCeara %>% dplyr::select(iso_code=iso_code, 
#                                          location=location,
#                                          date=dateRep,
#                                          total_cases=total_cases,
#                                          new_cases=new_cases,
#                                          total_deaths=total_deaths,
#                                          new_deaths=new_deaths,
#                                          total_tests=0,
#                                          new_tests=0,
#                                          total_tests_per_thousand=0,
#                                          new_tests_per_thousand=0,
#                                          new_tests_smoothed_per_thousand=0,
#                                          stringency_index=0,
#                                          population=0,
#                                          population_density=0,
#                                          aged_65_older=0,
#                                          life_expectancy=0)

rm(cases)
rm(deaths)
rm(seq)
rm(firstDate)
rm(lastDate)



owd_data<-union_all(owd_data,dataMsal)


owd_data<-union_all(owd_data,dataCeara)


rm(dataMsalAmba)
rm(dataMsal)
rm(dataMsalAmbaPBA)
rm(combinaciones)
rm(df_full)

# guarda input folder
# setwd("appTest")
save(owd_data, file =  "appTest - Cod/DatosIniciales/owd_data.RData")

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

