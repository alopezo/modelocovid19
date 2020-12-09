library(dplyr)

oms_data <-
  owd_data %>% dplyr::select(
    iso_code = iso_code,
    location = location,
    date = date,
    total_cases = total_cases,
    new_cases = new_cases,
    total_deaths = total_deaths,
    new_deaths = new_deaths
  ) %>% filter(owd_data$date<=hoy)

save(oms_data,file="DatosIniciales/oms_data.RData")
