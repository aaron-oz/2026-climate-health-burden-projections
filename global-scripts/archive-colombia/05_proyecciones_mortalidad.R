# Colombia mortality projections
# Author: Jean Carlo Pineda Lozano
# Creation date: 16/02/2023
# Institution: World Bank


# configuration -----------------------------------------------------------


# memory clean
rm(list = ls()); invisible(gc())

# install packages
packages <- c("tidyverse", "readxl", "janitor", "dplyr", "reshape2")
to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install)) { 
  install.packages(to_install)
}

# packages load
library(readxl)
library(janitor)
library(dplyr)
library(tidyverse)
library(reshape2)


# data upload -------------------------------------------------------------
direccion_excel <- "Bases/Proyecciones mortalidad/anexo-cambio-demografico-Mortalidad-por-sexo-nal-2018-2070-dptal-2018-2050.xlsx"

# data 2010-2017
df_mort_homb_2010_17 <- read_excel("Bases/Proyecciones mortalidad/anexo-cambio-demografico-Mortalidad-nal-dptal-1985-2017.xlsx",
                              skip = 12, sheet = 1)
df_mort_muj_2010_17 <- read_excel("Bases/Proyecciones mortalidad/anexo-cambio-demografico-Mortalidad-nal-dptal-1985-2017.xlsx",
                                   skip = 12, sheet = 2)

# national data upload, select rows with information of "total"
df_mort_nal <- lapply(c(2,3), function(x) read_excel(direccion_excel, sheet = x, range = "B9:CY62"))
df_mort_nal[[1]]$sexo <- "M"
df_mort_nal[[2]]$sexo <- "F"
df_mort_nal <- bind_rows(df_mort_nal)
df_mort_nal$ubicacion <- "00 - Nacional"
df_mort_nal <- df_mort_nal %>% 
  select("AÑO","ubicacion", "sexo", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
         "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
         "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", 
         "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", 
         "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", 
         "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", 
         "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", 
         "95", "96", "97", "98", "99", "100")


# departamental data upload
vec_secuencia_inicio <- seq(186, 3930, 117)
vec_secuencia_final <- seq(219, 3963, 117)
vec_rango_depto <- paste0("B", vec_secuencia_inicio, ":", "CY", vec_secuencia_final)
# departament names
vec_secuencia_nombres <- paste0("B", vec_secuencia_inicio - 3)

list_datos_hombres <- lapply(vec_rango_depto, function(r) read_excel(direccion_excel, sheet = 2, range = r))
list_datos_mujeres <- lapply(vec_rango_depto, function(r) read_excel(direccion_excel, sheet = 3, range = r))
list_datos_hombres <- lapply(list_datos_hombres, function(x) x %>% mutate(across(everything(), as.numeric)))
list_datos_mujeres <- lapply(list_datos_mujeres, function(x) x %>% mutate(across(everything(), as.numeric)))
                             
list_nom_depto <- lapply(vec_secuencia_nombres, function(x) read_excel(direccion_excel, sheet = 2, range = x))
vec_nom_depto <- unlist(lapply(list_nom_depto, names))

# add department names to each departament data base 
for (i in 1:length(vec_nom_depto)) {
  list_datos_hombres[[i]]$ubicacion <- vec_nom_depto[i]
  list_datos_hombres[[i]]$sexo <- "M"
  list_datos_mujeres[[i]]$ubicacion <- vec_nom_depto[i]
  list_datos_mujeres[[i]]$sexo <- "F"
}

df_mort_depto <- bind_rows(list_datos_hombres, list_datos_mujeres)

# rows with any na
df_mort_depto %>% 
  filter(rowSums(across(everything(), is.na))>0) %>% 
  view()


# unify databases ---------------------------------------------------------
df_mort <- bind_rows(df_mort_nal, df_mort_depto)

#clean names
df_mort <- clean_names(df_mort)

# separate department name and code
df_mort <- df_mort %>% 
  separate(ubicacion, into = c("cod_dpto", "nom_dpto"), sep = " - ", remove = T)

# melt databases
df_mort <- pivot_longer(data = df_mort,
                        cols = starts_with("x"),
                        names_to = "edad",
                        values_to = "t_mort")
# adjust "edad"
df_mort$edad <- gsub("x", "", df_mort$edad) %>% as.numeric()
df_mort$cod_dpto <- as.numeric(df_mort$cod_dpto)

# mortality 2010-2070 -----------------------------------------------------

#clean names
df_mort_homb_2010_17 <- clean_names(df_mort_homb_2010_17)
df_mort_muj_2010_17 <- clean_names(df_mort_muj_2010_17)


# rename variables
df_mort_homb_2010_17 <- df_mort_homb_2010_17 %>% 
  rename(cod_dpto = dp,
         nom_dpto = dpnom)

df_mort_muj_2010_17 <- df_mort_muj_2010_17 %>% 
  rename(cod_dpto = dp,
         nom_dpto = dpnom)

# filter year >= 2010
df_mort_homb_2010_17 <- df_mort_homb_2010_17 %>% 
  filter(ano >= 2010)
df_mort_muj_2010_17 <- df_mort_muj_2010_17 %>% 
  filter(ano >= 2010)

# melt databases
df_mort_homb_2010_17 <- pivot_longer(data = df_mort_homb_2010_17,
                        cols = starts_with("x"),
                        names_to = "edad",
                        values_to = "t_mort")

df_mort_muj_2010_17 <- pivot_longer(data = df_mort_muj_2010_17,
                                     cols = starts_with("x"),
                                     names_to = "edad",
                                     values_to = "t_mort")

df_mort_homb_2010_17$edad = str_replace(as.character(df_mort_homb_2010_17$edad), pattern = "x",
                                          replacement = "M_")
df_mort_muj_2010_17$edad = str_replace(as.character(df_mort_muj_2010_17$edad), pattern = "x",
                                          replacement = "F_")


# new variable
df_mort_homb_2010_17 <- cbind(df_mort_homb_2010_17[, c(1,2,3,5)], colsplit(string = as.character(df_mort_homb_2010_17$edad),
                                                                         pattern = "_", names = c("sexo", "edad")))
df_mort_muj_2010_17 <- cbind(df_mort_muj_2010_17[, c(1,2,3,5)], colsplit(string = as.character(df_mort_muj_2010_17$edad),
                                                                         pattern = "_", names = c("sexo", "edad")))
df_mort_muj_2010_17$sexo <- "F"

df_mort_2010_17 <- bind_rows(df_mort_muj_2010_17, df_mort_homb_2010_17)
df_mort_2010_17 <- df_mort_2010_17 %>% 
  mutate_at(c("cod_dpto", "ano"), as.numeric)


# population data ---------------------------------------------------------
df_pob_depto_2010_2050 <- readRDS("Bases/Proyecciones poblacionales/Proyecciones_poblacion_depto_edad_simple_2010_2050.rds")
df_pob_nal_2010_2070 <- readRDS("Bases/Proyecciones poblacionales/Proyecciones_poblacion_nacional_edad_simple_2010_2070.rds")

df_pob_nal_2010_2070$nom_dpto <- "Nacional"
df_pob_nal_2010_2070$cod_dpto <- 0

df_pob <- bind_rows(df_pob_nal_2010_2070, df_pob_depto_2010_2050) %>% 
  select(c("cod_dpto", "nom_dpto", "ano", "sexo", "edad", "poblacion" ))



# merge mortality and population ------------------------------------------
# data export function
f_export_data = function(data, pathfile){
  
  #RDS
  saveRDS(data, file = paste0(pathfile,".rds"))
  # csv
  write.csv2(data, file = paste0(pathfile,".csv"), na = "", row.names = FALSE)
  # excel
  writexl::write_xlsx(data, path = paste0(pathfile,".xlsx")) 
}

# unify mortality rate 2010-2017 and 2018-2070
df_mort <- bind_rows(df_mort_2010_17, df_mort)

# save mortality rates simple ages
# f_export_data(df_mort, "Bases/Proyecciones mortalidad/Proyeccion_tasas_mortalidad_edades_simples_DANE")

keys <- df_mort[,c(1,2)] %>% 
  distinct()

df_mort <- df_mort %>% 
  select(-nom_dpto) %>% 
  left_join(df_pob %>% 
              select(-nom_dpto)) %>% 
  left_join(keys, multiple = "all")

# death estimate ----------------------------------------------------------
df_mort$muertes <- round(df_mort$t_mort * df_mort$poblacion)


# data exportation --------------------------------------------------------
df_mortality <- select(.data = df_mort, c("nom_dpto", "cod_dpto", "ano", "sexo", "edad", "poblacion",  
  "muertes"))

#simple ages
f_export_data(df_mortality, "Bases/Proyecciones mortalidad/Proyeccion_mortalidad_edades_simples")



# grouped ages
df_mortality$edad <- cut(df_mortality$edad, breaks = c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,100),
                                  labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                                             "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                                             "75-79", "80+"), 
                                  include.lowest = T)
df_mortality$edad <- as.character(df_mortality$edad)


# rows with any na
df_mortality %>% 
  filter(rowSums(across(everything(), is.na))>0) %>% 
  view()

df_mort_group <- df_mortality %>% 
  group_by(nom_dpto, cod_dpto, ano, sexo, edad) %>% 
  summarise(muertes = sum(muertes, na.rm = T))


# falt <- df_mortality %>% 
#   filter(rowSums(across(everything(), is.na))>0)
f_export_data(df_mort_group, "Bases/Proyecciones mortalidad/Proyeccion_mortalidad_edad_agrupada")

