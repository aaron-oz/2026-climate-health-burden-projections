# Mortalidad causas relacionadas con temperatura 2010-2020
# Estimaciones AVPP
# Autor: Jean Carlo Pineda Lozano
# fecha de creacion: 7/3/2023
# Institucion: Banco Mundial



# Configuracion -----------------------------------------------------------

# limpiar memoria
rm(list = ls()); invisible(gc())

# instalar paquetes necesarios
packages <- c("tidyverse", "janitor", "readxl")
to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install)) { 
  install.packages(to_install)
}


# cargar librerias
library(tidyverse)
library(janitor)
library(readxl)
library(stringr)
library(fs)


# Cargue de base de datos  ------------------------------------------------
df_mortalidad_estudio <- readRDS("Bases/Bases mortalidad estudio depto residencia/df_mortalidad_estudiodepto_resid.rds")
df_mortalidad_estudio <- filter(df_mortalidad_estudio, ano <= 2019)
df_pob <- readRDS("Bases/Proyecciones poblacionales/Proyecciones_poblacion_depto_2010_2050.rds")
df_pob <- df_pob %>% 
  rename(cod_depto = cod_dpto,
         gru_ed1 = edad) %>% 
  mutate(gru_ed1 = as.character(gru_ed1))

# cargue de tablas de vida

# 2005 a 2017
vec_2005 <- list.files("Bases/Tablas de vida/2005-2017/", full.names = T)
list_hojas_2005 <- sapply(vec_2005, excel_sheets)
list_hojas_2005 <- lapply(list_hojas_2005, function(a) a[grepl("A$", a)])
vec_sheet <- sort(unlist(list_hojas_2005[33], use.names = F))
# repetir mismo numero de veces las bases y las hhojas para el cargue iterativo
vec_bd <- rep(vec_2005, each = length(vec_sheet))
vc_sheet_rep <- rep(vec_sheet, times = 34)

# cargue iterativo de los datos
list_2005 <- mapply(function(e, r) read_excel(path = e, sheet = r, range = "A12:H30"),
                    e = vec_bd, r = vc_sheet_rep, SIMPLIFY = F)


vec_bd_names <- gsub("Bases/Tablas de vida/2005-2017/TV-|-Total.xlsx", "", vec_bd)
vec_sheet_sexo <- substr(x = vc_sheet_rep, 1, 1)
vec_sheet_ano <- substr(x = vc_sheet_rep, 2, 5)

df_tv <- data.frame()
for (i in seq_along(list_2005)) {
  tabla <- cbind(codDepto = vec_bd_names[i],
                 sexo = vec_sheet_sexo[i],
                 ano = vec_sheet_ano[i],
                 list_2005[[i]])
  df_tv <- rbind(df_tv, tabla)
}


# 2018 a 2050
vec_2018 <- list.files("Bases/Tablas de vida/2018-2050/", full.names = T)
list_hojas_2018 <- sapply(vec_2018, excel_sheets)
list_hojas_2018 <- lapply(list_hojas_2018, function(a) a[grepl("A$", a)])
vec_sheet <- sort(unlist(list_hojas_2018[33], use.names = F))
# repetir mismo numero de veces las bases y las hhojas para el cargue iterativo
vec_bd <- rep(vec_2018, each = length(vec_sheet))
vc_sheet_rep <- rep(vec_sheet, times = 34)

# cargue iterativo de los datos
list_2018 <- mapply(function(e, r) read_excel(path = e, sheet = r, range = "A12:H30"),
                    e = vec_bd, r = vc_sheet_rep, SIMPLIFY = F)


vec_bd_names <- gsub("Bases/Tablas de vida/2018-2050/TV|-Total.xlsx", "", vec_bd)
vec_sheet_sexo <- substr(x = vc_sheet_rep, 1, 1)
vec_sheet_ano <- substr(x = vc_sheet_rep, 2, 5)

df_tv_2018 <- data.frame()
for (i in seq_along(list_2018)) {
  tabla <- cbind(codDepto = vec_bd_names[i],
                 sexo = vec_sheet_sexo[i],
                 ano = vec_sheet_ano[i],
                 list_2018[[i]])
  df_tv_2018 <- rbind(df_tv_2018, tabla)
}
 
#unificar tablas de vida
df_tv_global <- rbind(df_tv, df_tv_2018) %>% 
  clean_names() %>% 
  select(c("cod_depto", "sexo", "ano", "age", "e_x")) %>% 
  rename(ev = e_x)

# guardar tablas de vida
# saveRDS(df_tv_global, "Bases/Tablas de vida/Tablas_vida_DANE_2005_2050.rds")
df_tv_global <- readRDS("Bases/Tablas de vida/Tablas_vida_DANE_2005_2050.rds")

# Estimacion AVPP ---------------------------------------------------------

#unificar bases
names(df_mortalidad_estudio) <- c("ano", "cod_depto", "sexo", "gru_ed1", "c_muerte", "muertes")
df_tv_global <- df_tv_global %>% 
  mutate(gru_ed1 = case_when(
    age == 0 ~ "0-4",
    age == 1 ~ "0-4",
    age == 5 ~ "5-9",
    age == 10 ~ "10-14",
    age == 15 ~ "15-19",
    age == 20 ~ "20-24",
    age == 25 ~ "25-29",
    age == 30 ~ "30-34",
    age == 35 ~ "35-39",
    age == 40 ~ "40-44",
    age == 45 ~ "45-49",   
    age == 50 ~ "50-54",
    age == 55 ~ "55-59",
    age == 60 ~ "60-64",
    age == 65 ~ "65-69",
    age == 70 ~ "70-74",
    age == 75 ~ "75-79",
    age == 80 ~ ">80",
    TRUE ~ NA_character_),
    cod_depto = as.numeric(cod_depto),
    cod_depto = case_when(
      cod_depto == 0 ~ 75, # extranjero se le adjudica esperanzas de vida nacionales
      TRUE ~ cod_depto), 
    ano = as.numeric(ano)) %>% 
  filter(age != 0) %>% # no se tiene en cuenta edad 0, esta se incluye a quinquenio 0-4
  select(-age) %>% 
  distinct()

df_avpp <- left_join(df_mortalidad_estudio, df_tv_global, multiple = "all")

# AVPP
df_avpp$avpp <- ifelse(df_avpp$gru_ed1 == ">80", 
                       round(df_avpp$muertes * 10, 2), # Supuesto de que mayores de 80 pierden 10 años, dado el rango más amplio que en los otros grupos
                       round(df_avpp$muertes * (df_avpp$ev-2.5), 2))

# tasas
df_avpp <- left_join(df_avpp, df_pob, multiple = "all")
df_avpp$tasa_avpp <- round((df_avpp$avpp/df_avpp$poblacion)*100000,2)

# Reordenar y quitar ev
df_avpp <- df_avpp %>% 
  select(c("ano", "cod_depto", "nom_dpto", "sexo", "gru_ed1", "c_muerte", "muertes", 
           "poblacion", "avpp", "tasa_avpp"))

# Exportar resultados -----------------------------------------------------


f_export_data = function(data, pathfile){
  
  #RDS
  saveRDS(data, file = paste0(pathfile,".rds"))
  # csv
  write.csv2(data, file = paste0(pathfile,".csv"), na = "", row.names = FALSE)
  # excel
  writexl::write_xlsx(data, path = paste0(pathfile,".xlsx")) 
}


f_export_data(df_avpp, "Bases/Estimaciones carga/avpp_totales")
