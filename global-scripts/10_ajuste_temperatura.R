# Daily temperature 2010-2019
# Author: Jean Carlo Pineda Lozano
# Date created: 20/04/2023
# Date modified: 20/04/2023
# Institution: World Bank


library(readr)
library(tidyverse)

# daily temperature
temp_colombia_dias <- read.csv("Bases/Ambientales/temp_colombia_dias.csv")
temp_colombia_dias$fecha <- temp_colombia_dias$dia + 40178 # this is the numeric date for 12/31/2009
temp_colombia_dias <- temp_colombia_dias %>% 
  mutate(fecha = as.Date(fecha, 
                         origin = "1899-12-30",
                         tz = "UTC")) %>% 
  select(-dia )%>% 
  mutate(temperatura = temperatura- 273.15) # convert from Kelvin to Celsius

names(temp_colombia_dias) <- c("cod_depto", "nom_depto", "temperatura", "sd_kelvin", "fecha")

#hourly temperature
temp_colombia_horas <- read.csv("Bases/Ambientales/temp_colombia_horas.csv")
temp_colombia_horas$fecha <- temp_colombia_horas$dia + 40178 # this is the numeric date for 12/31/2009
temp_colombia_horas <- temp_colombia_horas %>% 
  mutate(fecha = as.Date(fecha, 
                         origin = "1899-12-30",
                         tz = "UTC"))

horas <- data.frame("hora" = 1:87648,
                    "hora_exacta" = seq(1, 24, 1))

temp_colombia_horas <- temp_colombia_horas %>% 
  left_join(horas) %>% 
  select(-c(hora, dia)) %>% 
  mutate(temperatura = temperatura- 273.15) # convert from Kelvin to Celsius

names(temp_colombia_horas) <- c("cod_depto", "nom_depto", "temperatura", "sd_kelvin", "fecha", "hora")

#save data
write.csv(temp_colombia_dias, "Bases/Ambientales/temp_colombia_dias_ajustado.csv", row.names = F)
write.csv(temp_colombia_horas, "Bases/Ambientales/temp_colombia_horas_ajustado.csv", row.names = F)



# compare hourly aggregation with sent data

temp_colombia_dias_ajust <- temp_colombia_horas %>% 
  group_by(cod_depto, nom_depto, fecha) %>% 
  summarise(temperatura_ajust = mean(temperatura)) %>% 
  left_join(temp_colombia_dias)

temp_colombia_dias_ajust$diff <- temp_colombia_dias_ajust$temperatura_ajust - temp_colombia_dias_ajust$temperatura



