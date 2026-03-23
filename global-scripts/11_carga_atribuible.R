# Temperatura diaria 2010-2019
# Autor: Jean Carlo Pineda Lozano, Samuel Osorio
# fecha de creacion: 26/04/2023
# fecha de modificacion: 28/04/2023
# Institucion: Banco Mundial



# limpiar memoria
rm(list = ls()); invisible(gc())

# librerias
library(readr)
library(tidyverse)



# cargue de datos ---------------------------------------------------------
# temperatura diaria
# df_temperatura <- read.csv("Bases/Ambientales/temperatura_dias_con_na.csv")
df_temperatura <- readRDS("Bases/Ambientales/temperatura_diaria_pixel.rds")
# poblacion <- readRDS("Bases/Ambientales/temperatura_diaria_pixel.rds") %>% filter(fecha == as.Date("2010-01-01"))
poblacion <- readRDS("Bases/Ambientales/WorldPop/WorldPop_2010_2019_pixel.rds")
#poblacion dane (para san andres)
pob_depto_2010_2050 <- readRDS("Bases/Proyecciones poblacionales/Proyecciones_poblacion_depto_2010_2050_postcovid.rds")

#curvas
curves_er <- readRDS("Bases/Ambientales/Curvas ER/curves_all_causes.rds")
#rr max
max_rr <- readRDS("Bases/Ambientales/Curvas ER/max_rr_zone.rds")
#muertes
mortalidad_dia <- readRDS("Bases/Mortalidad depurada/mortalidad_diaria_DANE_2010_2019_imput.rds")
# TMREL
df_tmrel <- read_csv("Bases/Burkart/tmrel_125_summaries.csv")

#avpp totales
avpp_totales <- readRDS("Bases/Estimaciones carga/avpp_totales.rds")

#tablas de vida
df_tv_global <- readRDS("Bases/Tablas de vida/Tablas_vida_DANE_2005_2050.rds")

# ajuste datos ------------------------------------------------------------
#poblacion
index_unique <- df_temperatura %>% 
  select(index_right, DPTO_CCDGO) %>% 
  distinct()

pob_pixel <- poblacion %>% 
  rename(index_right = indx_rg) %>% 
  left_join(index_unique) 

pob_depto <- pob_pixel %>% 
  group_by(ano, DPTO_CCDGO) %>% 
  summarise(pob_depto = sum(sum_z))

prop_pob_depto <- left_join(pob_pixel, pob_depto) %>% 
  mutate(pr = sum_z/pob_depto) %>% 
  select(c(ano, index_right, DPTO_CCDGO, pr)) %>% 
  rename(cod_depto = DPTO_CCDGO)
  
prop_san_a <- data.frame(ano = 2010:2019,
                         index_right = 1499,
                         cod_depto = 88,
                         pr = 1)
prop_pob_depto <- rbind(prop_pob_depto, prop_san_a)

# poblacion <- sum(poblacion$pob)

#eliminar objetos temporales
rm(index_unique, pob_pixel, pob_depto, prop_san_a)

#poblacion dane (para san andres)
pob_san_a <- pob_depto_2010_2050 %>% 
  filter(cod_dpto == 88,
         ano <= 2019) %>% 
  group_by(ano, cod_dpto) %>% 
  summarise(poblacion = sum(poblacion)) %>% 
  rename(cod_depto = cod_dpto)



# avpp
avpp_totales$c_muerte[avpp_totales$c_muerte == "cardiopatía_hiperten"] <- "cardiopatia_hipertensiva"
avpp_totales$c_muerte[avpp_totales$c_muerte == "miocardiopatia_miocar"] <- "miocardiopatia_miocarditis"

# tablas de vida
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
    cod_depto = as.numeric(cod_depto)) %>% 
  filter(cod_depto != 0, # se eliminan tablas nacionales para este cálculo
         ano <= 2019) %>%
  filter(age != 0) %>% # no se tiene en cuenta edad 0, esta se incluye a quinquenio 0-4
  select(-age) %>% 
  distinct()

#rr max
names(max_rr) <- c("zona", "c_muerte", "temperatura", "rr_max_temp", "rr_min_temp")
max_rr$zona <- as.character(max_rr$zona)
max_rr$c_muerte[max_rr$c_muerte == "cardiopatía_hipertensiva"] <- "cardiopatia_hipertensiva"

# temperatura

names(df_temperatura) <- c("temperatura", "fecha", "index_right", "pob", "cod_depto")

df_temperatura$temperatura <- round(df_temperatura$temperatura, 1)

df_divipola <- readRDS("Bases/divipola.rds")
df_divipola <- df_divipola %>%
  dplyr::select(codigo_departamento, nombre_departamento) %>%
  distinct()

names(df_divipola) <- c("cod_depto", "nom_depto")

df_temperatura <- left_join(df_temperatura, df_divipola) %>% 
  relocate(any_of(c("nom_depto")), .after = cod_depto)

df_temperatura <- df_temperatura %>% 
  mutate(ano = as.numeric(substr(fecha, 1, 4 )))

df_temperatura <- left_join(df_temperatura, prop_pob_depto)
df_temperatura$pob <- NULL
# df_temperatura$ano <- NULL

#curvas
names(curves_er) <- c("zona", "temperatura", "rr_mean", "rr_lower", "rr_upper", "rr_max", "c_muerte")
curves_er$zona <- trimws(curves_er$zona, which = c("both"))
curves_er$c_muerte <- trimws(curves_er$c_muerte, which = c("both"))
curves_er$c_muerte[curves_er$c_muerte == "cardiopatía_hipertensiva"] <- "cardiopatia_hipertensiva"


curves_er <- curves_er %>%
  mutate(c_muerte = stringi::stri_trans_general(str = c_muerte, id = "Latin-ASCII")) %>%
  mutate(temperatura = round(as.numeric(temperatura),1)) %>%
  mutate(zona = as.character(zona))

# curves_er$rr_max <- NULL

# mortalidad
mortalidad_dia$c_muerte[mortalidad_dia$c_muerte == "cardiopatía_hiperten"] <- "cardiopatia_hipertensiva"
mortalidad_dia$c_muerte[mortalidad_dia$c_muerte == "miocardiopatia_miocar"] <- "miocardiopatia_miocarditis"

# quitar quienes residen en el extranjero
mortalidad_dia <- mortalidad_dia %>% 
  filter(codptore != 75)

mortalidad_dia_unique <- mortalidad_dia %>% 
  group_by(codptore, fecha_def, c_muerte) %>% 
  summarise(muertes = sum(muertes)) %>% 
  as.data.frame()

names(mortalidad_dia) <- c("fecha", "cod_depto", "sexo", "gru_ed1", "c_muerte", "muertes")
names(mortalidad_dia_unique) <- c("cod_depto", "fecha", "c_muerte", "muertes")

# agrupar temperaturas por año y zonas de acuerdo al promedio 2010-2019

df_temp_periodo <- df_temperatura %>% 
  group_by(ano, index_right) %>% 
  summarise(zona = round(mean(temperatura, na.rm = T)))

# riesgo diario -----------------------------------------------------------
df_rr <- df_temperatura %>% 
  left_join(df_temp_periodo) %>% 
  mutate(temperatura = round(temperatura, 1),
         fecha = as.Date(fecha))

# copiar y pegar 17 veces la base (una por cada causa de muerte) 
df_rr <- map(seq_len(17), ~df_rr) %>% 
  bind_rows()

# poner cada causa de muerte el numero de filas necesario
c_muerte <- sort(unique(curves_er$c_muerte))
df_rr$c_muerte = rep(c_muerte, each = 5465046)

#unir mortalidad y base de temperatura y zonas para acortar base solo a dias con muertes por cada causa
df_rr <- left_join(df_rr, mortalidad_dia_unique) %>% 
  filter(!is.na(muertes)) %>% 
  select(-muertes)

df_rr$c_muerte <- trimws(df_rr$c_muerte, which = c("both"))
df_rr$zona <- trimws(df_rr$zona, which = c("both"))

df_rr <- df_rr %>%
  mutate(c_muerte = stringi::stri_trans_general(str = c_muerte, id = "Latin-ASCII")) %>%
  mutate(temperatura = round(as.numeric(temperatura),1)) %>%
  mutate(zona = as.character(zona),
         zona = case_when(
           zona == "29" ~ "28", # hay pixeles con zonas 29 y 30, que no existen en metodologia de GBD
           zona == "30" ~ "28",
           zona == "31" ~ "28",
           TRUE ~ zona
         )) %>% 
  select(c("ano", "fecha", "index_right", "cod_depto", "nom_depto", "pr", "c_muerte",
           "zona", "temperatura"))

curves_er_2 <- filter(curves_er, zona %in% df_rr$zona)

# joint por causa de muerte y zona, si la temperatura está por encima de la temperatura donde hay RR 
# en esa zona, asignar el RR más alto de la zona, pero si esta por debajo, asignar el RR más bajo
# de la zona

curves_max <- curves_er %>%
  group_by(zona) %>%
  summarise(max_temp = max(temperatura),
            min_temp = min(temperatura))

df_rr <- df_rr %>% 
  left_join(curves_max)

df_rr <- df_rr %>%
  mutate(temperatura = case_when(
    temperatura > max_temp ~ max_temp,
    temperatura < min_temp ~ min_temp,
    TRUE ~ temperatura)) %>% 
  select(-c(max_temp, min_temp))

df_rr <- left_join(df_rr, curves_er_2)



# tmrel -------------------------------------------------------------------

# TMREL
# correr esta sentencia si se quieren usar los valores promedio entre 2010 y 2020
df_tmrel <- df_tmrel %>% 
  group_by(meanTempCat) %>% 
  summarise(tmrelMean = mean(tmrelMean),
            tmrelLower = mean(tmrelLower),
            tmrelUpper = mean(tmrelUpper)) %>% 
  mutate(zona = as.character(meanTempCat)) %>% 
  select(-meanTempCat) 

# copiar y pegar 17 veces la base (una por cada causa de muerte) 
df_tmrel <- map(seq_len(17), ~df_tmrel) %>% 
  bind_rows()

# poner cada causa de muerte el numero de filas necesario
df_tmrel$c_muerte = rep(c_muerte, each = 23)

# redondear y hacer numéricos
df_tmrel <- df_tmrel %>% 
  mutate(
    across(where(is.numeric), ~round(.x, 1)))%>% 
  left_join(curves_er %>% 
              rename(tmrelMean = temperatura,
                     rr_tmrel_mean = rr_mean,
                     rr_tmrel_lower = rr_lower,
                     rr_tmrel_upper = rr_upper))

df_rr <- left_join(df_rr, df_tmrel)

zona_rr <- curves_er_2 %>% 
  group_by(zona, c_muerte) %>% 
  summarise(rr_max = max(rr_max))

df_rr <- df_rr %>%
  select(-rr_max) %>% 
  left_join(zona_rr)
# saveRDS(df_rr, "Bases/Estimaciones carga/RR diario.rds")

# base para los sevs ------------------------------------------------------
poblacion <- poblacion %>% 
  rename(index_right = indx_rg,
         pob = sum_z)

df_base_sevs <- df_temperatura %>%
  group_by(ano, cod_depto, index_right) %>% 
  summarise(zona = round(mean(temperatura, na.rm = T)),
            temperatura = mean(temperatura)) %>% 
  mutate(zona = ifelse(zona >28, 28, zona)) %>% 
  select(-temperatura)

df_base_sevs <- df_temperatura %>%
  left_join(df_base_sevs)

df_base_sevs <- df_base_sevs %>% 
  mutate(zona=as.character(zona)) %>% 
  left_join(curves_max)

df_base_sevs <- df_base_sevs %>%
  mutate(temperatura = case_when(
    temperatura > max_temp ~ max_temp,
    temperatura < min_temp ~ min_temp,
    TRUE ~ temperatura)) %>% 
  select(-c(max_temp, min_temp))

df_base_sevs <- left_join(df_base_sevs, poblacion) 
df_base_sevs <- left_join(df_base_sevs, pob_san_a) 

df_base_sevs <- df_base_sevs %>% 
  mutate(pob = case_when(
    is.na(pob) ~ poblacion,
    TRUE ~ pob
  )) %>% 
  select(-poblacion)

# df_base_sevs <- df_base_sevs %>%
#   group_by(ano, cod_depto, zona) %>%
#   summarise(pob = sum(pob),
#             temperatura = mean(temperatura))

s <- as.Date("2010-01-01")
eneros <- seq(from = s, by="year", length.out=10)

df_base_sevs_d <- df_base_sevs %>% 
  filter(fecha %in% eneros)
  
  
pob_depto <- df_base_sevs_d %>% 
  group_by(ano, cod_depto) %>% 
  summarise(pob_depto = sum(pob))

pob_zona <- df_base_sevs_d %>% 
  group_by(ano, cod_depto, zona) %>% 
  summarise(pob_zona = sum(pob))

df_base_sevs <- left_join(df_base_sevs, pob_depto)
df_base_sevs <- left_join(df_base_sevs, pob_zona)

df_base_sevs <- df_base_sevs %>% mutate(pr = pob/pob_depto,
                                        pr_zona = pob/pob_zona)

#solo un año
# df_base_sevs <- df_base_sevs %>% filter(ano == 2019)

# copiar y pegar 17 veces la base (una por cada causa de muerte) 
df_base_sevs <- map(seq_len(17), ~df_base_sevs) %>% 
  bind_rows()

# poner cada causa de muerte el numero de filas necesario
c_muerte <- sort(unique(curves_er$c_muerte))
df_base_sevs$c_muerte = rep(c_muerte, each = 5465046)

# cruce con curvas er
df_base_sevs <- df_base_sevs %>% 
  mutate(temperatura = round(temperatura,1),
         zona = as.character(zona)) %>% 
  left_join(curves_er)

# rr max
df_base_sevs <- df_base_sevs %>% 
  select(-rr_max) %>% 
  left_join(zona_rr)

# saveRDS(df_base_sevs, "Bases/Estimaciones carga/Base_sevs_zona_depto_ano.rds")


# paf ---------------------------------------------------------------------
#paf ponderados por poblacion 
# df_rr <- left_join(df_rr, prop_pob_depto)

df_rr <- df_rr %>% 
  mutate(paf = case_when(
    rr_mean >= 1 ~ pr*(rr_mean -1)/rr_mean,
    TRUE ~ pr* -1*((1/rr_mean)-1)/(1/rr_mean) #-((1/RR)-1)/(1/RR)
      ),
    paf2 = case_when(
      rr_mean >= 1 ~ pr*(rr_mean -1)/rr_mean,
      TRUE ~ pr* -1*((1/rr_mean)-1)/(1/rr_mean) #-((1/RR)-1)/(1/RR)
    ),
    paf = ifelse(paf < 0, 0, paf)) # formula codigos gbd ifelse(x>=1, pr*(x-1)/x, pr*-1*((1/x)-1)/(1/x)))


#efecto
df_rr <- df_rr %>% 
  mutate(effect = case_when(
         temperatura < tmrelMean ~ "low_temperature",
         temperatura > tmrelMean ~ "high_temperature",
         TRUE ~ NA_character_))


# saveRDS(df_rr, "Bases/Estimaciones carga/PAF_diario_pixel.rds")

# los PAF se dividen en Heat y cold
coldPafs <- filter(df_rr, effect == "low_temperature")
heatPafs <- filter(df_rr, effect == "high_temperature")


#paf anual por deartamento
# paf_year_depto_cold <- coldPafs %>%
#   mutate(fecha = substr(fecha, 1, 4 )) %>%
#   group_by(cod_depto, fecha, c_muerte) %>%
#   summarise(paf = sum(paf))
# 
# 
# paf_year_depto_heat <- heatPafs %>%
#   mutate(fecha = substr(fecha, 1, 4 )) %>%
#   group_by(cod_depto, fecha, c_muerte) %>%
#   summarise(paf = sum(paf))

# paf diario departamento

paf_day_depto_cold <- coldPafs %>%
  group_by(cod_depto, fecha, c_muerte) %>%
  summarise(paf = sum(paf),
            paf2= sum(paf2))

paf_day_depto_heat <- heatPafs %>%
  group_by(cod_depto, fecha, c_muerte) %>%
  summarise(paf = sum(paf),
            paf2= sum(paf2))

# sev ---------------------------------------------------------------------
# calculo con base con rrmaximos por zona depto ano y causa de muerte

#codigo samuel
# obtener poblacion expuesta a temperaturas diarias
# sevs <- df_base_sevs %>%
#   mutate(sev = ifelse(rr_max <=1 | rr_mean<=1, 0, pr_zona * (rr_mean - 1)/(rr_max-1)))%>%
#   group_by(cod_depto, ano, c_muerte, zona) %>%
#   summarise(sev = sum(sev)) %>%
#   mutate(sev = ifelse(sev < 0, 0, sev),
#          sev = ifelse(sev > 1, 1, sev))
# 
# sevs <- left_join(sevs, pob_depto)
# sevs <- left_join(sevs, pob_zona)
#   
# # promedios ponderados entre zonas
# sevs <- sevs %>%
#   mutate(ppzonadep = pob_zona / pob_depto,
#          sev2 = sev * ppzonadep) %>%
#   group_by(cod_depto, c_muerte) %>%
#   summarise(sev2 = sum(sev2))

#anuales
base_pobtemp <- df_base_sevs %>%
  group_by(fecha, cod_depto, c_muerte, zona, temperatura) %>%
  summarise(pob_temp = sum(pob),
            pob_zona = mean(pob_zona),
            pob_depto = mean(pob_depto),
            rr_mean = mean(rr_mean),
            rr_max = mean(rr_max))

# creo la variable de pesos
base_pobtemp <- base_pobtemp %>%
  mutate(pr_zona = pob_temp / pob_zona)

# se guarda salida intermedia porque es muy pesado el procesamiento
saveRDS(base_pobtemp,"Bases/Estimaciones carga/base_pobtemp.rds")

# calculo los SEVs
sev <- base_pobtemp %>%
  mutate(sev = ifelse(rr_max <=1 | rr_mean<=1, 0, pr_zona * (rr_mean - 1)/(rr_max-1))) %>%
  mutate(fecha = as.numeric(substr(fecha, 1, 4 ))) %>% 
  rename(ano = fecha) %>%
  group_by(ano, cod_depto, c_muerte, zona) %>%
  summarise(sev = sum(sev),
            pob_zona = mean(pob_zona),
            pob_depto = mean(pob_depto)) %>%
  mutate(sev = ifelse(sev < 0, 0, sev),
         sev = ifelse(sev > 1, 1, sev))

# promedios ponderados entre zonas
sev2 <- sev %>%
  mutate(ppzonadep = pob_zona / pob_depto,
         sev2 = sev * ppzonadep) %>%
  group_by(ano,cod_depto, c_muerte) %>%
  summarise(sev = sum(sev2))


# saveRDS(sev2, "Bases/Estimaciones carga/sevs_year_departamento.rds")
# write.csv(sev2, "Bases/Estimaciones carga/sevs_year_departamento.csv")


# riesgo por año y efecto atribuible --------------------------------------
# 
# paf_year_depto <- paf_day_depto_cold %>%
#   rename(paf_cold = paf2) %>%
#   full_join(paf_day_depto_heat %>%
#               rename(paf_heat = paf2)) %>%
#   mutate(fecha = substr(fecha, 1, 4 )) %>%
#   rename(ano = fecha) %>% 
#   group_by(ano, cod_depto, c_muerte) %>% 
#   summarise(paf_cold = sum(paf_cold),
# #             paf_heat = sum(paf_heat)) %>% 
#   mutate(paf_heat = ifelse(is.na(paf_heat) | paf_heat <0, 0, paf_heat),
#          paf_heat = ifelse(paf_heat >1, 1, paf_heat),
#          paf_cold = ifelse(is.na(paf_cold)| paf_cold <0, 0, paf_cold),
#          paf_cold = ifelse(paf_cold >1, 1, paf_cold),
#     paf_non_optimal_temp = paf_cold + paf_heat) %>%
#   left_join(sevs)


paf_day_depto <- df_rr %>% 
  select(fecha, cod_depto, c_muerte) %>% 
  distinct() %>% 
  left_join(paf_day_depto_cold %>% 
              rename(paf_cold = paf,
                     paf_cold2 = paf2)) %>% 
  left_join(paf_day_depto_heat %>% 
              rename(paf_heat = paf,
                     paf_heat2 = paf2)) %>% 
  mutate(paf_heat = ifelse(is.na(paf_heat), 0, paf_heat),
         paf_cold = ifelse(is.na(paf_cold), 0, paf_cold),
         paf_non_optimal_temp = paf_cold + paf_heat)



# enfoque 2

# paf_day_depto2 <- paf_day_depto_cold %>% 
#   rename(paf_cold = paf) %>% 
#   full_join(paf_day_depto_heat %>% 
#               rename(paf_heat = paf)) %>% 
#   mutate(paf_heat = ifelse(is.na(paf_heat) | paf_heat <0, 0, paf_heat),
#          paf_cold = ifelse(is.na(paf_cold)| paf_cold <0, 0, paf_cold),
#          paf_non_optimal_temp = paf_cold + paf_heat,
#          paf_non_optimal_temp_nj = 1-((1-paf_cold) * (1-paf_heat)))
# 
# 1-(1-0.4)*(1-0.03)


# saveRDS(paf_day_depto, "Bases/Estimaciones carga/PAF_dia_departamento.rds")
# write.csv(paf_day_depto, "Bases/Estimaciones carga/PAF_dia_departamento.csv")
# carga atribuible a temperaturas no optimas ------------------------------

# carga_atriuible <- avpp_totales %>%
#   left_join(paf_year_depto %>%
#               rename(ano = fecha) %>%
#               mutate(ano = as.numeric(ano))) %>%
#   mutate(muertes_cold = round(muertes*paf_cold*sev,2),
#          muertes_heat = round(muertes*paf_heat*sev,2),
#          muertes_non_optimal_temp = round(muertes*paf_non_optimal_temp*sev,2),
#          avpp_cold = round(avpp*paf_cold*sev,2),
#          avpp_heat = round(avpp*paf_heat*sev,2),
#          avpp_non_optimal_temp = round(avpp*paf_non_optimal_temp*sev,2))

#con ajustes sev samuel
carga_atriuible <- mortalidad_dia %>% 
  left_join(paf_day_depto) %>% 
  mutate(fecha = as.numeric(substr(fecha, 1, 4 ))) %>% 
  rename(ano = fecha) %>%
  left_join(sev2) %>% 
  mutate(muertes_cold = muertes*paf_cold*sev,
         muertes_heat = muertes*paf_heat*sev,
         muertes_non_optimal_temp = muertes*paf_non_optimal_temp*sev) %>% 
  group_by(ano, cod_depto, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes),
            muertes_cold = sum(muertes_cold),
            muertes_heat = sum(muertes_heat),
            muertes_non_optimal_temp = sum(muertes_non_optimal_temp)) %>% 
  mutate(muertes_cold = ifelse(is.na(muertes_cold), 0, muertes_cold),
         muertes_heat = ifelse(is.na(muertes_heat), 0, muertes_heat),
         muertes_non_optimal_temp = ifelse(is.na(muertes_non_optimal_temp), 0, muertes_non_optimal_temp))

# saveRDS(carga_atriuible, "Bases/Estimaciones carga/muertes_atribuibles.rds")


#avpp atribuibles
carga_atriuible <- left_join(carga_atriuible, df_tv_global %>% mutate(ano = as.numeric(ano)))

carga_atriuible <- carga_atriuible %>% 
  mutate(avpp_cold = ifelse(gru_ed1 == ">80", round(muertes_cold * 10, 4),
                            round(muertes_cold * (ev-2.5), 4)),
         avpp_heat = ifelse(gru_ed1 == ">80", round(muertes_heat * 10, 4),
                            round(muertes_heat * (ev-2.5), 4)),
         avpp_non_optimal_temp = ifelse(gru_ed1 == ">80", round(muertes_non_optimal_temp * 10, 4),
                                        round(muertes_non_optimal_temp * (ev-2.5), 4))) %>% 
  select(-ev)


saveRDS(carga_atriuible, "Bases/Estimaciones carga/avpp_atribuibles.rds")
write.csv(carga_atriuible, "Bases/Estimaciones carga/avpp_atribuibles.csv")


carga_atriuible_nosevs <- mortalidad_dia %>% 
  left_join(paf_day_depto) %>% 
  mutate(fecha = as.numeric(substr(fecha, 1, 4 ))) %>% 
  rename(ano = fecha) %>%
  mutate(muertes_cold = muertes*paf_cold,
         muertes_heat = muertes*paf_heat,
         muertes_non_optimal_temp = muertes*paf_non_optimal_temp) %>% 
  group_by(ano, cod_depto, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes),
            muertes_cold = sum(muertes_cold),
            muertes_heat = sum(muertes_heat),
            muertes_non_optimal_temp = sum(muertes_non_optimal_temp)) %>% 
  mutate(muertes_cold = ifelse(is.na(muertes_cold), 0, muertes_cold),
         muertes_heat = ifelse(is.na(muertes_heat), 0, muertes_heat),
         muertes_non_optimal_temp = ifelse(is.na(muertes_non_optimal_temp), 0, muertes_non_optimal_temp))
