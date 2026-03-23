# Mortalidad causas relacionadas con temperatura 2010-2020
# Autor: Jean Carlo Pineda Lozano
# fecha de creacion: 24/11/2022
# fecha de modificacion: 3/3/2023
# Institucion: Banco Mundial



# configuracion -----------------------------------------------------------


# Cargue de paquetes

library(readxl)
library(tidyr)
library(janitor)
library(dplyr)
library(tidyverse)
library(dlookr)
library(missRanger)

# limpiar memoria
rm(list = ls()); invisible(gc())



# cargue de datos ---------------------------------------------------------

# mortalidad obtenida EEVV DANE: https://microdatos.dane.gov.co/index.php/catalog/MICRODATOS/about_collection/22/?per_page=10
df_mort_2010 <- read_excel("Bases/Mortalidad cruda/nofetal2010.xlsx")
df_mort_2011 <- read_excel("Bases/Mortalidad cruda/defu2011.xlsx")
df_mort_2012 <- read_excel("Bases/Mortalidad cruda/defu2012.xlsx")
df_mort_2013 <- read_excel("Bases/Mortalidad cruda/nofetal2013.xlsx")
df_mort_2014 <- read_excel("Bases/Mortalidad cruda/Def_2014.xlsx")
df_mort_2015 <- read.delim("Bases/Mortalidad cruda/Defun_2015.txt")
df_mort_2016 <- read.delim("Bases/Mortalidad cruda/Nofetal_2016.txt", quote = NULL)
df_mort_2017 <- read.delim("Bases/Mortalidad cruda/nofetal2017.txt", quote = NULL)
df_mort_2018 <- read.delim("Bases/Mortalidad cruda/nofetal2018.txt", quote = NULL)
df_mort_2019 <- read.csv("Bases/Mortalidad cruda/nofetal_2019.csv", sep = ";")
df_mort_2020 <- read_csv("Bases/Mortalidad cruda/nofetal_2020.csv")
df_mort_2021 <- read_csv("Bases/Mortalidad cruda/nofetal2021.csv")

list_mortalidad <- list(df_mort_2010, df_mort_2011, df_mort_2012, df_mort_2013,
                        df_mort_2014, df_mort_2015, df_mort_2016, df_mort_2017,
                        df_mort_2018, df_mort_2019, df_mort_2020, df_mort_2021)

list_mortalidad <- lapply(list_mortalidad, clean_names)


# depuracion de datos -----------------------------------------------------


names(list_mortalidad) <- paste0("df_mort_", 2010:2021)

#seleccion de variables
list_var <- c("ano", "cod_dpto", "cod_munic", "codptore", "codmunre", "area_res", "sexo",
              "gru_ed1", "c_bas1")
list_mort_select =list(
  df_mort_2010 = c("fecha_def", "cod_dpto", "cod_munic", "codptore", "codmunre", 
                   "area_res", "sexo", "gru_ed1", "c_bas1"),
  df_mort_2011 = c("fecha_def", "cod_dpto", "cod_munic", "codptore", "codmunre", 
                   "area_res", "sexo", "gru_ed1", "c_bas1"),
  df_mort_2012 = c("fecha_def", "cod_dpto", "cod_munic", "codptore", "codmunre", 
                   "area_res", "sexo", "gru_ed1", "c_bas1"),
  df_mort_2013 = c("fecha_def", "cod_dpto", "cod_munic", "codptore", "codmunre", 
                   "area_res", "sexo", "gru_ed1", "c_bas1"),
  df_mort_2014 = list_var,
  df_mort_2015 = list_var,
  df_mort_2016 = list_var,
  df_mort_2017 = list_var,
  df_mort_2018 = list_var,
  df_mort_2019 = list_var,
  df_mort_2020 = list_var,
  df_mort_2021 = list_var)

mort_select <- list()
for(x in names(list_mort_select)){
  mort_select[[x]] <- select (list_mortalidad[[x]], any_of(list_mort_select[[x]]))
}

#unificar nombres
mort_select <- lapply(mort_select, function(x){
  names(x) <- list_var
  x
})


# primera observacion no tiene datos
mort_select$df_mort_2014 <- mort_select$df_mort_2014[-1,]
mort_select$df_mort_2013 <- mort_select$df_mort_2013[-1,]

# recodificar fecha
mort_select$df_mort_2010$ano <- 2010
mort_select$df_mort_2011$ano <- 2011
mort_select$df_mort_2012$ano <- 2012
mort_select$df_mort_2013$ano <- 2013

# variables numericas
mort_select <- lapply(mort_select, function(x)
  x %>% 
    mutate(
      across(starts_with(c("ano", "cod_dpto", "codptore", "gru_ed1")), ~as.numeric(.x))))


# recodificar grupo de edad
mort_select = lapply(mort_select, function(x)
  x %>%
    mutate(
      gru_ed1 = case_when(
        gru_ed1 == "00" ~ "0-4", gru_ed1 == 0 ~ "0-4", gru_ed1 == 01 ~ "0-4", gru_ed1 == 1 ~ "0-4",
        gru_ed1 == 02 ~ "0-4", gru_ed1 == 2 ~ "0-4", gru_ed1 == 03 ~ "0-4", gru_ed1 == 3 ~ "0-4",
        gru_ed1 == 04 ~ "0-4", gru_ed1 == 4 ~ "0-4", gru_ed1 == 05 ~ "0-4", gru_ed1 == 5 ~ "0-4",
        gru_ed1 == 06 ~ "0-4", gru_ed1 == 6 ~ "0-4", gru_ed1 == 07 ~ "0-4", gru_ed1 == 7 ~ "0-4",
        gru_ed1 == 08 ~ "0-4", gru_ed1 == 8 ~ "0-4", gru_ed1 == 09 ~ "5-9", gru_ed1 == 9 ~ "5-9",
        gru_ed1 == 10 ~ "10-14", gru_ed1 == 11 ~ "15-19", gru_ed1 == 12 ~ "20-24",
        gru_ed1 == 13 ~ "25-29", gru_ed1 == 14 ~ "30-34", gru_ed1 == 15 ~ "35-39",
        gru_ed1 == 16 ~ "40-44", gru_ed1 == 17 ~ "45-49", gru_ed1 == 18 ~ "50-54",
        gru_ed1 == 19 ~ "55-59", gru_ed1 == 20 ~ "60-64", gru_ed1 == 21 ~ "65-69",
        gru_ed1 == 22 ~ "70-74", gru_ed1 == 23 ~ "75-79", gru_ed1 == 24 ~ ">80",
        gru_ed1 == 25 ~ ">80", gru_ed1 == 26 ~ ">80", gru_ed1 == 27 ~ ">80",
        gru_ed1 == 28 ~ ">80", gru_ed1 == 29 ~ NA_character_, TRUE ~ NA_character_
      )
    ))


# recodificar sexo
mort_select = lapply(mort_select, function(x)
  x %>%
    mutate(
      sexo = case_when(
        sexo == "1" ~ "M",
        sexo == "2" ~ "F",
        TRUE ~ NA_character_)
    ))

# ajuste area de residencia
mort_select = lapply(mort_select, function(x)
  x %>%
    mutate(
      area_res = case_when(
        area_res == "1" ~ "Cabecera municipal",
        area_res == "2" ~ "Centro poblado",
        area_res == "3" ~ "Rural disperso",
        area_res == "9" ~ "Sin infornmacion",
        TRUE ~ "NA"),
    ))

# variables categoricas
mort_select <- lapply(mort_select, function(x)
  x %>% 
    mutate(
      across(starts_with(c("cod_mun", "codmunre")), ~as.character(.x)),
      across(where(is.character), ~trimws(toupper(.x)))))



#unificar serie en una sola base

mortalidad <- bind_rows(mort_select$df_mort_2010, mort_select$df_mort_2011, mort_select$df_mort_2012,
                        mort_select$df_mort_2013, mort_select$df_mort_2014, mort_select$df_mort_2015,
                        mort_select$df_mort_2016, mort_select$df_mort_2017, mort_select$df_mort_2018,
                        mort_select$df_mort_2019, mort_select$df_mort_2020, mort_select$df_mort_2021)

mortalidad$cod_munic[str_length(mortalidad$cod_munic) == 2] = paste("0", mortalidad$cod_munic[str_length(mortalidad$cod_munic) == 2], sep = "")
mortalidad$cod_munic[str_length(mortalidad$cod_munic) == 1] = paste("00", mortalidad$cod_munic[str_length(mortalidad$cod_munic) == 1], sep = "")
mortalidad <-mortalidad %>% 
  mutate(cod_munic = paste0(cod_dpto, cod_munic),
         cod_munic = as.numeric(cod_munic))

#NAs en codptore y codmunre, se transforman en caracteres para luego volverlos NAs, dado el error:
# "NAs are not allowed in subscripted assignments"

mortalidad$codptore[is.na(mortalidad$codptore)] <- "na"
mortalidad$codmunre[is.na(mortalidad$codmunre)] <- "na"

mortalidad$codmunre[str_length(mortalidad$codmunre) == 2] = paste("0", mortalidad$codmunre[str_length(mortalidad$codmunre) == 2], sep = "")
mortalidad$codmunre[str_length(mortalidad$codmunre) == 1] = paste("00", mortalidad$codmunre[str_length(mortalidad$codmunre) == 1], sep = "")
mortalidad <-mortalidad %>% 
  mutate(codmunre = paste0(codptore, codmunre),
         codmunre = as.numeric(codmunre))

mortalidad$codptore[mortalidad$codptore == "na"] <- NA
mortalidad$codptore <- as.numeric(mortalidad$codptore)

# na en dpto residencia se toma depto ocurrencia

mortalidad <- mortalidad %>% 
  mutate(
    codptore = case_when(
      is.na(codptore) ~ cod_dpto,
      codptore == 1 ~ cod_dpto,
      TRUE ~ codptore
    ))

# agregar nombre departamento 
mortalidad <- mortalidad %>% 
  select(-c(cod_dpto, cod_munic, codmunre))

df_divipola <- readRDS("Bases/divipola.rds")
df_divipola <- df_divipola %>%
  dplyr::select(codigo_departamento, nombre_departamento) %>%
  distinct()

names(df_divipola) <- c("codptore", "nom_dptore")

mortalidad <- left_join(mortalidad, df_divipola) %>% 
  relocate(any_of(c("nom_dptore")), .after = codptore)

mortalidad <- mortalidad %>% 
  mutate(nom_dptore = case_when(
    codptore == 75 ~ "EXTRANJERO",
    TRUE ~ nom_dptore))

rm(df_mort_2010, df_mort_2011, df_mort_2012, df_mort_2013,
   df_mort_2014, df_mort_2015, df_mort_2016, df_mort_2017,
   df_mort_2018, df_mort_2019, df_mort_2020, df_mort_2021,
   list_mort_select, list_mortalidad, mort_select)

# saveRDS(mortalidad, "Bases/mortalidad_2010_2021.rds")
# imputacion de datos faltantes ---------------------------------------

# cargue de base con depuracion previa
mortalidad <- readRDS("Bases/mortalidad_2010_2021.rds")

#grafico de nas
plot_na_pareto(mortalidad)

# imputacion de datos, correrlo tarda aproximadamente 3 horas
# mortalidad_imput <- missRanger(
#   mortalidad,
#   formula = .~.,
#   num.trees = 1000,
#   seed = 3
# )
# saveRDS(mortalidad_imput, "Bases/mortalidad_imput.rds")


# ajustes base unificada --------------------------------------------------
# base con datos imputados
mortalidad_imput <-  readRDS("Bases/mortalidad_imput.rds")
# contar de nas
colSums(is.na(mortalidad_imput))

# agrupar muertes
df_muertes_group <- mortalidad_imput %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_bas1) %>% 
  summarise(muertes = n()) %>% 
  as.data.frame()


# separacion por causa de muerte ------------------------------------------

# codigos CIE-10 para cada causa de muerte

cardiopatia_isquemica <- c("I20", "I21", "I22", "I23", "I24", "I25") # I20-I25,9
acv <- c("G45", "G460", "G461", "G462", "G463", "G464", "G465", "G466", "G467", "G468", #G45-G46.8
         "I60", "I61", "I62", "I63", "I65", "I66", "I670", "I671", "I672", "I673", # I60-I63.9, I65-I66.9, I67.0-I67.3
          "I675", "I676", "I681", "I682", "I690", "I691", "I692", "I693") #I67.5-I67.6, I68.1-I68.2, I69.0-I69.3 
miocardiopatia_miocarditis <- c("B33.2", "I40", "I41", "I421", "I422", "I423", "I424", "I425", "I426",
                                "I427", "I428", #B33.2, I40-I41.9, I42.1-I42.8
                                "I43", "I514") # I43-I43.9, I51.4
cardiopatía_hipertensiva <- c("I11") #I11-I11.9
ivri <- c("A481", "A70", "B974", "B975", "B976", "J09", "J10", "J11", "J12", "J13", "J14",
          "J151", "J152", "J153", "J154", "J155", "J156", "J157", "J158", # A48.1, A70, B97.4-B97.6, J09-J15.8
          "J16", "J20", "J21", "J910", "P230", "P231", "P232", "P233", "P234", # J16-J16.9, J20-J21.9, J91.0, P23.0-P23.4
          "U04")# U04-U04.9
epoc <- c("J41", "J42", "J43", "J44") # J41-J44.9
dm <- c("E100", "E101","E103","E104","E105","E106","E107","E108","E109","E110", "E111", #E10-E10.1, E10.3-E11.1
        "E113", "E114", "E115", "E116", "E117", "E118", "E119", "P702") # E11.3-E11.9, P70.2

erc <- c("D631", "E102", "E112", "I12", "I13", "N02", "N03", "N04", "N05", "N06", "N07", "N080", "N081",
         "N082", "N083", "N084", "N085", "N086", "N087", "N088", "N150", "N18", # D63.1, E10.2, E11.2, I12-I13.9, N02-N08.8, N15.0, N18-N18.9
         "Q61", "Q620", "Q621", "Q622", "Q623", "Q624", "Q625", "Q626", "Q627", "Q628") # Q61-Q62.8

homicidio <- c("X85", "X86", "X87", "X88", "X89", "X90", "X91", "X92", "X93", "X94", "X95", "X96", "X97", "X98", "X99",
               "Y00", "Y01", "Y02", "Y03", "Y04", "Y05", "Y06","Y07", "Y08", "Y871") #X85-Y08.9, Y87.1

suicidio <- c("X60", "X61", "X62", "X63", "X64", "X66", "X67", "X68", "X69", "X70", "X71", "X72", 
              "X73", "X74", "X75", "X76", "X77", "X78", "X79", "X80", "X81", "X82", "X83", "Y870") # X60-X64.9, X66-X83.9, Y87.0

ahogamiento <- c("W65", "W66", "W67", "W68", "W69", "W70", "W73", "W74") # W65-W70.9, W73-W74.9

desastres <- c("X33", "X34", "X35", "X36", "X37", "X38")

lesiones_mecanicas <- c("W2", "W30", "W31", "W32", "W33", "W34", "W35", "W36", "W37", "W38", "W40",
                        "W41", "W42", "W43", "W450", "W451", "W452", "W460", "W461", "W462", #W20-W38.9, W40-W43.9, W45.0-W45.2, W46-W46.2,
                        "W49", "W450", "W451", "W452")	#W49-W52

accidentes_trafico <- c("V01", "V02", "V03", "V04", "V06", "V07", "V09", "V08", paste0("V", 10:80), #V01-V04.9, V06-V80.9
                        "V82", "V872", "V873") #V82-V82.9, V87.2-V87.3

relacion_transporte <- c("V000", "V001", "V002", "V003", "V004", "V005", "V006", "V007", "V008", #V00-V00.8
                         "V05", "V81", "V83", "V84", "V85", "V86", "V882",  "V883",  paste0("V", 90:98)) # V05-V05.9, V81-V81.9, V83-V86.9, V88.2-V88.3, V90-V98.8

no_intencionales <- c("W39", "W77", "W81", paste0("W", 85:87), paste0("X", 50:54), "X57", "X58") #W39-W39.9, W77-W77.9, W81-W81.9, W85-W87.9, X50-X54.9, X57-X58.9


relacionadas_animales <- c(paste0("W", 52:62), "W64", paste0("X", 20:29)) # W52.0-W62.9, W64-W64.9, X20-X29.9
  


# cardiopatia isquemica
df_cardiopatia_isquemica <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(cardiopatia_isquemica, collapse = "|"),
                x = c_bas1, ignore.case = T)) %>% 
  mutate(c_muerte = "cardiopatia_isquemica") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))
# ACV
df_acv <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(acv, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "acv") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))

# miocardiopatia y miocarditis
df_miocardiopatia_miocarditis <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(miocardiopatia_miocarditis, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "miocardiopatia_miocarditis") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))

# Cardiopatía hipertensiva
df_cardiopatía_hipertensiva <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(cardiopatía_hipertensiva, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "cardiopatía_hipertensiva") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))

# Infección de las vías respiratorias inferiores
df_ivri <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(ivri, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "ivri") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))

# Enfermedad pulmonar obstructiva crónica
df_epoc <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(epoc, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "epoc") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))

# Diabetes
df_dm <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(dm, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "dm") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))

# Enfermedad renal crónica
df_erc <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(erc, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "erc") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))

# Homicidio
df_homicidio <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(homicidio, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "homicidio") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))

# suicidio
df_suicidio <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(suicidio, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "suicidio") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))

# ahogamiento
df_ahogamiento <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(ahogamiento, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "ahogamiento") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))

# desastres
df_desastres <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(desastres, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "desastres") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))

# lesiones_mecanicas
df_lesiones_mecanicas <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(lesiones_mecanicas, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "lesiones_mecanicas") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))

# accidentes trafico
df_accidentes_trafico <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(accidentes_trafico, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "accidentes_trafico") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))

# Otras lesiones relacionadas con el transporte
df_relacion_transporte <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(relacion_transporte, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "relacion_transporte") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))

# Otras lesiones no intencionales
df_no_intencionales <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(no_intencionales, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "no_intencionales") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))

# Relacionadas con los animales
df_relacionadas_animales <- df_muertes_group %>% 
  filter(grepl(pattern = paste0(relacionadas_animales, collapse = "|"),
               x = c_bas1, ignore.case = T))%>% 
  mutate(c_muerte = "relacionadas_animales") %>% 
  group_by(ano, codptore, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes))


df_mortalidad_estudio <- bind_rows(df_cardiopatia_isquemica, df_acv, df_miocardiopatia_miocarditis, 
                                   df_cardiopatía_hipertensiva, df_ivri, df_epoc, df_dm, df_erc,
                                   df_homicidio, df_suicidio, df_ahogamiento, df_desastres, 
                                   df_lesiones_mecanicas, df_accidentes_trafico, df_relacion_transporte,
                                   df_no_intencionales, df_relacionadas_animales)

list_mortalidad_estudio <- list(df_mortalidad_estudio, df_cardiopatia_isquemica, df_acv, df_miocardiopatia_miocarditis, 
                              df_cardiopatía_hipertensiva, df_ivri, df_epoc, df_dm, df_erc,
                              df_homicidio, df_suicidio, df_ahogamiento, df_desastres, 
                              df_lesiones_mecanicas, df_accidentes_trafico, df_relacion_transporte,
                              df_no_intencionales, df_relacionadas_animales)

names_vec <- c("df_mortalidad_estudio", "df_cardiopatia_isquemica", "df_acv", "df_miocardiopatía_miocarditis", 
                             "df_cardiopatía_hipertensiva", "df_ivri", "df_epoc", "df_dm", "df_erc",
                             "df_homicidio", "df_suicidio", "df_ahogamiento", "df_desastres", 
                             "df_lesiones_mecanicas", "df_accidentes_trafico", "df_relacion_transporte",
                             "df_no_intencionales", "df_relacionadas_animales")

names(list_mortalidad_estudio) <- names_vec



# mortalidad no agrupada
# mortalidad_imput <- mortalidad_imput %>% 
#   filter(c_bas1 %in% df_mortalidad_estudio$c_bas1,
#          ano <= 2019) %>% 
#   mutate(c_muerte = case_when(
#     grepl(pattern = paste0(cardiopatia_isquemica, collapse = "|"), x = c_bas1, ignore.case = T)~ "cardiopatia_isquemica",
#     grepl(pattern = paste0(acv, collapse = "|"), x = c_bas1, ignore.case = T)~ "acv",
#     grepl(pattern = paste0(miocardiopatia_miocarditis, collapse = "|"), x = c_bas1, ignore.case = T)~ "miocardiopatia_miocarditis",
#     grepl(pattern = paste0(cardiopatía_hipertensiva, collapse = "|"), x = c_bas1, ignore.case = T)~ "cardiopatía_hipertensiva",
#     grepl(pattern = paste0(ivri, collapse = "|"), x = c_bas1, ignore.case = T)~ "ivri",
#     grepl(pattern = paste0(epoc, collapse = "|"), x = c_bas1, ignore.case = T)~ "epoc",
#     grepl(pattern = paste0(homicidio, collapse = "|"), x = c_bas1, ignore.case = T)~ "homicidio",
#     grepl(pattern = paste0(suicidio, collapse = "|"), x = c_bas1, ignore.case = T)~ "suicidio",
#     grepl(pattern = paste0(ahogamiento, collapse = "|"), x = c_bas1, ignore.case = T)~ "ahogamiento",
#     grepl(pattern = paste0(desastres, collapse = "|"), x = c_bas1, ignore.case = T)~ "desastres",
#     grepl(pattern = paste0(lesiones_mecanicas, collapse = "|"), x = c_bas1, ignore.case = T)~ "lesiones_mecanicas",
#     grepl(pattern = paste0(accidentes_trafico, collapse = "|"), x = c_bas1, ignore.case = T)~ "accidentes_trafico",
#     grepl(pattern = paste0(relacion_transporte, collapse = "|"), x = c_bas1, ignore.case = T)~ "relacion_transporte",
#     grepl(pattern = paste0(no_intencionales, collapse = "|"), x = c_bas1, ignore.case = T)~ "no_intencionales",
#     grepl(pattern = paste0(relacionadas_animales, collapse = "|"), x = c_bas1, ignore.case = T)~ "relacionadas_animales",
#     grepl(pattern = paste0(dm, collapse = "|"), x = c_bas1, ignore.case = T)~ "dm",
#     grepl(pattern = paste0(erc, collapse = "|"), x = c_bas1, ignore.case = T)~ "erc",
#     TRUE ~ NA_character_))
#  

# Exportar bases depuradas ----------------------------------------------------------


# funcion para exportar datos
f_export_data = function(data, pathfile){
  
  #RDS
  saveRDS(data, file = paste0(pathfile,".rds"))
  # csv
  write.csv2(data, file = paste0(pathfile,".csv"), na = "", row.names = FALSE)
  # excel
  writexl::write_xlsx(data, path = paste0(pathfile,".xlsx")) 
}

# exportacion
for (i in names_vec) {
  f_export_data(data = list_mortalidad_estudio[[i]], 
                pathfile = paste0("Bases/Bases mortalidad estudio depto residencia/", 
                                  i, "depto_resid"))
}


# f_export_data(mortalidad_imput, "Bases/Bases mortalidad estudio depto residencia/df_mortalidad_estudio_desagregada")
