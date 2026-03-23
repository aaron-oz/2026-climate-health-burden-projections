# Colombia population projections
# Author: Jean Carlo Pineda Lozano
# Creation date: 17/01/2023
# Institution: World Bank

# packages load

library(readxl)
library(tidyr)
library(janitor)
library(dplyr)
library(tidyverse)
library(reshape2)



# memory clean
rm(list = ls()); invisible(gc())

# data upload

df_depto_2018_2050 <- read_excel("Bases/Proyecciones poblacionales/anexo-proyecciones-poblacion-departamental_2018-2050.xlsx", 
                                         skip = 11)
df_depto_2010_2017 <- read_excel("Bases/Proyecciones poblacionales/anexo-area-sexo-edad-proyecciones-poblacion-departamental_2005-2017.xlsx", 
                                 skip = 11)

df_nacional_2018_2070 <- read_excel("Bases/Proyecciones poblacionales/anexo-proyecciones-poblacion-Nacional2018_2070.xlsx", 
                                 skip = 11)


# data cleansing -----------------------------------------------------

# clean names
df_depto_2018_2050 <- clean_names(df_depto_2018_2050)
df_depto_2010_2017 <- clean_names(df_depto_2010_2017)
df_nacional_2018_2070 <- clean_names(df_nacional_2018_2070)

# rename variables
df_depto_2018_2050 <- df_depto_2018_2050 %>% 
  rename(cod_dpto = dp,
         nom_dpto = dpnom) %>% 
  select(-c(total, total_hombres, total_mujeres))

df_depto_2010_2017 <- df_depto_2010_2017 %>% 
  rename(cod_dpto = dp,
         nom_dpto = dpnom)


df_nacional_2018_2070 <- df_nacional_2018_2070 %>% 
  select(-c(dp, dpnom, total_general, total, total_hombres, total_mujeres))

# delete "total" variables
df_depto_2018_2050 <- df_depto_2018_2050[ , !str_detect(names(df_depto_2018_2050), "total")]
df_depto_2010_2017 <- df_depto_2010_2017 [ , !str_detect(names(df_depto_2010_2017), "total")]
df_nacional_2018_2070 <- df_nacional_2018_2070[ , !str_detect(names(df_nacional_2018_2070), "total")]

# filter year >= 2010
df_depto_2010_2017 <- df_depto_2010_2017 %>% 
  filter(ano >= 2010)

# melt databases
df_depto_2018_2050 <- melt(df_depto_2018_2050, id.vars = c("cod_dpto", "nom_dpto", "ano", "area_geografica"),
                           value.name = "poblacion")

df_depto_2010_2017 <- melt(df_depto_2010_2017, id.vars = c("cod_dpto", "nom_dpto", "ano", "area_geografica"),
                           value.name = "poblacion")

df_nacional_2018_2070 <- melt(df_nacional_2018_2070, id.vars = c("ano", "area_geografica"),
                           value.name = "poblacion")

df_depto_2018_2050$variable = str_replace(as.character(df_depto_2018_2050$variable), pattern = "hombres_",
                                          replacement = "M_")
df_depto_2018_2050$variable = str_replace(as.character(df_depto_2018_2050$variable), pattern = "mujeres_",
                                          replacement = "F_")
df_depto_2010_2017$variable = str_replace(as.character(df_depto_2010_2017$variable), pattern = "hombres_",
                                          replacement = "M_")
df_depto_2010_2017$variable = str_replace(as.character(df_depto_2010_2017$variable), pattern = "mujeres_",
                                          replacement = "F_")
df_nacional_2018_2070$variable = str_replace(as.character(df_nacional_2018_2070$variable), pattern = "hombres_",
                                          replacement = "M_")
df_nacional_2018_2070$variable = str_replace(as.character(df_nacional_2018_2070$variable), pattern = "mujeres_",
                                          replacement = "F_")
# new variable
df_depto_2018_2050 <- cbind(df_depto_2018_2050[, c(1,2,3,4,6)], colsplit(string = as.character(df_depto_2018_2050$variable), 
                                                                         pattern = "_", names = c("sexo", "edad")))
df_depto_2018_2050 <- df_depto_2018_2050 %>% 
  mutate_at(c("cod_dpto", "ano"), as.numeric)

df_depto_2010_2017 <- cbind(df_depto_2010_2017[, c(1,2,3,4,6)], colsplit(string = as.character(df_depto_2010_2017$variable), 
                                                                         pattern = "_", names = c("sexo", "edad")))
df_depto_2010_2017 <- df_depto_2010_2017 %>% 
  mutate_at(c("cod_dpto"), as.numeric)

df_nacional_2018_2070 <- cbind(df_nacional_2018_2070[, c(1,2,4)], colsplit(string = as.character(df_nacional_2018_2070$variable), 
                                                                         pattern = "_", names = c("sexo", "edad")))
df_nacional_2018_2070 <- df_nacional_2018_2070 %>% 
  mutate_at(c("ano"), as.numeric)

# adjust depto names
df_depto_2018_2050 <- df_depto_2018_2050 %>% 
  mutate(nom_dpto = case_when(
    nom_dpto == "Archipiélago de San Andrés, Providencia y Santa Catalina" ~ "Archipiélago de San Andrés",
    nom_dpto == "Quindío" ~ "Quindio",
    TRUE ~ nom_dpto))



# # merge df --------------------------------------------------------------
#departamental
df_depto_2010_2050 <- bind_rows(df_depto_2010_2017, df_depto_2018_2050)
df_depto_2010_2050$edad[df_depto_2010_2050$edad == "100_y_mas"] <- 100
df_depto_2010_2050$edad <- as.numeric(df_depto_2010_2050$edad)
df_depto_2010_2050 <- df_depto_2010_2050 %>% 
  filter(area_geografica == "Total") %>% 
  select(-area_geografica)


#nacional
df_nacional_2010_2070 <- df_depto_2010_2017 %>% 
  group_by(ano, area_geografica, sexo, edad) %>% 
  summarise(poblacion = sum(poblacion))

df_nacional_2010_2070 <- bind_rows(df_nacional_2010_2070, df_nacional_2018_2070)
df_nacional_2010_2070$edad[df_nacional_2010_2070$edad == "100_y_mas"] <- 100
df_nacional_2010_2070$edad <- as.numeric(df_nacional_2010_2070$edad)

df_nacional_2010_2070 <- df_nacional_2010_2070 %>% 
  filter(grepl(pattern =  "TOTAL|Total", x = area_geografica)) 

df_nacional_2010_2070$area_geografica <- NULL

# export ungroup data -----------------------------------------------------
# data export function
f_export_data = function(data, pathfile){
  
  #RDS
  saveRDS(data, file = paste0(pathfile,".rds"))
  # csv
  write.csv2(data, file = paste0(pathfile,".csv"), na = "", row.names = FALSE)
  # excel
  writexl::write_xlsx(data, path = paste0(pathfile,".xlsx")) 
}

# export
f_export_data(df_depto_2010_2050, 
              "Bases/Proyecciones poblacionales/Proyecciones_poblacion_depto_edad_simple_2010_2050")

f_export_data(df_nacional_2010_2070, 
              "Bases/Proyecciones poblacionales/Proyecciones_poblacion_nacional_edad_simple_2010_2070")


# grouping data -----------------------------------------------------------
# depto
df_depto_2010_2050$edad <- cut(df_depto_2010_2050$edad, breaks = c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,100),
                               labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                                          "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                                          "75-79", ">80"), 
                               include.lowest = T)
df_depto_2010_2050 <- df_depto_2010_2050 %>%
  group_by(cod_dpto, nom_dpto, ano, sexo, edad) %>% 
  summarise(poblacion  = sum(poblacion))

#country

df_nacional_2010_2070$edad <- cut(df_nacional_2010_2070$edad, breaks = c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,100),
                               labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                                          "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                                          "75-79", ">80"), 
                               include.lowest = T)

df_nacional_2010_2070 <- df_nacional_2010_2070 %>% 
  group_by(ano, sexo, edad) %>% 
  summarise(poblacion  = sum(poblacion))



# export databases --------------------------------------------------------


# export

f_export_data(df_depto_2010_2050, 
              "Bases/Proyecciones poblacionales/Proyecciones_poblacion_depto_2010_2050")

f_export_data(df_nacional_2010_2070, 
              "Bases/Proyecciones poblacionales/Proyecciones_poblacion_nacional_2010_2070")




