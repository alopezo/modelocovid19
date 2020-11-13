# ADOPCION CEARA

library(dplyr)

# download, extract and import from datasus  

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
                     dataCeara$resultadoFinalExame=="Positivo",] #filter confirmed

cases <- cases %>% dplyr::group_by(dataInicioSintomas) %>% tally() #group by and count 


seq <- data.frame(dateRep=seq(firstDate,lastDate, by=1)) #sequence

cases <- merge(seq, cases, by.x="dateRep", by.y="dataInicioSintomas") #format cases data.frame 

deaths <- dataCeara[is.na(dataCeara$dataInicioSintomas)==FALSE & 
                      dataCeara$obitoConfirmado=="True",] #filter deaths

deaths <- deaths %>% dplyr::group_by(dataObito) %>% tally() #group by and count

dataCeara <- merge(cases,deaths, by.x="dateRep", by.y="dataObito", all.x=TRUE) #merge cases and deaths by date

colnames(dataCeara)[2:3] <- c("new_cases","new_deaths") #column names

dataCeara$new_cases[is.na(dataCeara$new_cases)==TRUE] <- 0 #na to cero
dataCeara$new_deaths[is.na(dataCeara$new_deaths)==TRUE] <- 0 #na to cero

dataCeara <- dataCeara %>% mutate(total_cases=cumsum(new_cases), total_deaths=cumsum(new_deaths)) #final data frame

#enviromental cleaning
rm(cases)
rm(deaths)
rm(seq)
rm(firstDate)
rm(lastDate)

#updating OMS data
hoy='2020-11-01'
source("oms_data.R")


