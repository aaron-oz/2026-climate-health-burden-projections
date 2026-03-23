# Mortalidad causas relacionadas con temperatura 2010-2020
# Autor: Jean Carlo Pineda Lozano
# fecha de creacion: 1/12/2022
# Institucion: Banco Mundial



# limpiar memoria
rm(list = ls()); invisible(gc())

# instalar paquetes necesarios
packages <- c("tidyverse", "gtsummary", "flextable", "officer")
to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install)) { 
  install.packages(to_install)
}


# cargar librerias
library(tidyverse)
library(flextable)
library(gtsummary)
library(officer)
library(ggplot2)
library("RColorBrewer")

# ajuste de datos mortalidad ---------------------------------------------------------

# cargue de base de datos global
df_mortalidad_estudio <- readRDS("Bases/Bases mortalidad estudio depto residencia/df_mortalidad_estudio_desagregada.rds")

# Ajuste grupo de edad
df_mortalidad_estudio$gru_ed1 <- factor(df_mortalidad_estudio$gru_ed1,
                                levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                           "30-34", "35-39", "40-44", "45-49", "50-54",
                                           "55-59", "60-64", "65-69", "70-74", "75-79",
                                           ">80"))
df_mortalidad_estudio$ano <- as.character(df_mortalidad_estudio$ano)

#mayusculas
df_mortalidad_estudio$nom_dptore <- str_to_title(df_mortalidad_estudio$nom_dptore, 
                                               locale = "latin1")
df_mortalidad_estudio$nom_dptore[df_mortalidad_estudio$nom_dptore == "Archipiélago De San Andrés, Providencia Y"] <- "San Andrés"

df_mortalidad_estudio <- df_mortalidad_estudio %>% 
  mutate(c_muerte = case_when(
    c_muerte == "cardiopatia_isquemica" ~ "Cardiopatía isquémica",
    c_muerte == "acv" ~ "ACV",
    c_muerte == "epoc" ~ "EPOC",
    c_muerte == "erc" ~ "ERC",
    c_muerte == "dm" ~ "DM",
    c_muerte == "accidentes_trafico" ~ "Accidentes tráfico",
    c_muerte == "homicidio" ~ "Homicidio",
    c_muerte == "suicidio" ~ "Suicidio",
    c_muerte == "ahogamiento" ~ "Ahogamiento",
    c_muerte == "cardiopatía_hipertensiva" ~ "Cardiopatía hipertensiva",
    c_muerte == "relacion_transporte" ~ "Relación transporte",
    c_muerte == "miocardiopatia_miocarditis" ~ "Miocardiopatía miocarditis",
    c_muerte == "lesiones_mecanicas" ~ "Lesiones mecánicas",
    c_muerte == "desastres" ~ "Desastres",
    c_muerte == "no_intencionales" ~ "No intencionales",
    c_muerte == "ivri" ~ "IVRI",
    c_muerte == "relacionadas_animales" ~ "Relacionadas animales")) %>% 
  filter(ano != c("2020", "2021"))



# tablas ------------------------------------------------------------------


# configuracion tablas
set_gtsummary_theme(list(
  "tbl_summary-arg:missing_text" = "Sin datos"
))
set_gtsummary_theme(theme_gtsummary_compact())
set_gtsummary_theme(theme_gtsummary_language(language = "es", decimal.mark = ",", big.mark = "."))
set_gtsummary_theme(theme_gtsummary_mean_sd())



# Tabla descriptiva global ------------------------------------------------
tab_desc <- df_mortalidad_estudio %>% 
  tbl_summary(
    include = c(ano, sexo, gru_ed1, nom_dptore, c_muerte),
    label = c(ano ~ "Año",
              sexo ~ "Sexo",
              gru_ed1 ~ "Grupo de edad",
              nom_dptore ~ "Departamento",
              c_muerte ~ "Causa de muerte"),
    digits = list(all_categorical() ~ c(0, 1), all_continuous() ~ c(1, 1))
  ) %>% 
  bold_labels()
tab_desc

tab_desc <- tab_desc %>% 
  as_flex_table()%>% 
  autofit()


# tabla descriptiva Cardiorrespiratorias ----------------------------
cardiorrespiratorias <- c("Cardiopatía isquémica", "ACV", "Miocardiopatía miocarditis",
  "Cardiopatía hipertensiva","IVRI", "EPOC")

tab_desc_cardiorrespiratorias <- df_mortalidad_estudio %>% 
  filter(grepl(pattern = paste0(cardiorrespiratorias, collapse = "|"),
               x = c_muerte, ignore.case = T)) %>% 
  tbl_summary(by = c_muerte,
    include = c(ano,sexo, gru_ed1, nom_dptore),
    label = c(ano ~ "Año", 
              sexo ~ "Sexo",
              gru_ed1 ~ "Grupo de edad",
              nom_dptore ~ "Departamento"),
    digits = list(all_categorical() ~ c(0, 1), all_continuous() ~ c(1, 1))
  ) %>% 
  bold_labels()

tab_desc_cardiorrespiratorias

tab_desc_cardiorrespiratorias <- tab_desc_cardiorrespiratorias %>% 
  as_flex_table()%>% 
  autofit()

# tabla descriptiva metabolicas ----------------------------

tab_desc_metabolicas <- df_mortalidad_estudio %>% 
  filter(grepl(pattern = "dm|erc",x = c_muerte, ignore.case = T)) %>% 
  tbl_summary(by = c_muerte,
              include = c(ano,sexo, gru_ed1, nom_dptore),
              label = c(ano ~ "Año", 
                        sexo ~ "Sexo",
                        gru_ed1 ~ "Grupo de edad",
                        nom_dptore ~ "Departamento"),
              digits = list(all_categorical() ~ c(0, 1), all_continuous() ~ c(1, 1))
  ) %>% 
  bold_labels()

tab_desc_metabolicas

tab_desc_metabolicas <- tab_desc_metabolicas %>% 
  as_flex_table()%>% 
  autofit()
# tabla descriptiva Causas externas ----------------------------
causas_externas1 <- c("Homicidio", "Suicidio", "Ahogamiento", "Desastres", "Lesiones mecánicas")
causas_externas2 <- c("Accidentes tráfico", "Relación transporte", "No intencionales", 
                      "Relacionadas animales")

#causas externas 1
tab_desc_causas_externas1 <- df_mortalidad_estudio %>% 
  filter(grepl(pattern = paste0(causas_externas1, collapse = "|"),
               x = c_muerte, ignore.case = T)) %>% 
  tbl_summary(by = c_muerte,
              include = c(ano,sexo, gru_ed1, nom_dptore),
              label = c(ano ~ "Año", 
                        sexo ~ "Sexo",
                        gru_ed1 ~ "Grupo de edad",
                        nom_dptore ~ "Departamento"),
              digits = list(all_categorical() ~ c(0, 1), all_continuous() ~ c(1, 1))
  ) %>% 
  bold_labels()

tab_desc_causas_externas1

#causas externas 2
tab_desc_causas_externas2 <- df_mortalidad_estudio %>% 
  filter(grepl(pattern = paste0(causas_externas2, collapse = "|"),
               x = c_muerte, ignore.case = T)) %>% 
  tbl_summary(by = c_muerte,
              include = c(ano,sexo, gru_ed1, nom_dptore),
              label = c(ano ~ "Año", 
                        sexo ~ "Sexo",
                        gru_ed1 ~ "Grupo de edad",
                        nom_dptore ~ "Departamento"),
              digits = list(all_categorical() ~ c(0, 1), all_continuous() ~ c(1, 1))
  ) %>% 
  bold_labels()

tab_desc_causas_externas2

tab_desc_causas_externas1 <- tab_desc_causas_externas1 %>% 
  as_flex_table()%>% 
  autofit()

tab_desc_causas_externas2 <- tab_desc_causas_externas2 %>% 
  as_flex_table()%>% 
  autofit()

# guardar tablas ----------------------------------------------------------
save_as_docx(
  "Tabla descriptiva mortalidad todas las causas" = tab_desc,
  "Tabla descriptiva causas cardiorespiratorias" = tab_desc_cardiorrespiratorias,
  "Tabla descriptiva causas metabólicas" = tab_desc_metabolicas,
  "Tabla descriptiva causas externas primera parte" = tab_desc_causas_externas1,
  "Tabla descriptiva causas externas segunda parte" = tab_desc_causas_externas2,
  path = "Salidas/tab_descriptivas.docx"
)





# graficas ----------------------------------------------------------------

df_c_muerte <- df_mortalidad_estudio %>% 
  group_by(ano, c_muerte) %>% 
  summarise(muertes = sum(n()))

cardiorresp <- c("Cardiopatía isquémica", "ACV", "EPOC", "Cardiopatía hipertensiva",
                 "Miocardiopatía miocarditis", "IVRI")
metabol <- c("ERC", "DM")
causa_ext <- c("Accidentes tráfico", "Homicidio", "Suicidio", "Ahogamiento",
               "Relación transporte", "Lesiones mecánicas", "Desastres",
               "No intencionales", "Relacionadas animales")


df_c_muerte %>%
  filter(c_muerte %in% cardiorresp) %>% 
  ggplot(aes(x = c_muerte, y = muertes, fill = ano)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=brewer.pal(n = 10, name = "Spectral"))+
  guides(fill = guide_legend(title = "Año")) +
  labs(x = "Causa de muerte", y = "N muertes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_rect(fill = "#FFFFFF"))

ggsave(filename = "Grafico c_muerte.jpg", plot = last_plot(), path = "Salidas")

df_c_muerte %>%
  filter(c_muerte %in% metabol) %>% 
  ggplot(aes(x = c_muerte, y = muertes, fill = ano)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=brewer.pal(n = 10, name = "Spectral"))+
  guides(fill = guide_legend(title = "Año")) +
  labs(x = "Causa de muerte", y = "N muertes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_rect(fill = "#FFFFFF"))

ggsave(filename = "Grafico metabolicas.jpg", plot = last_plot(), path = "Salidas")


df_c_muerte %>%
  filter(c_muerte %in% causa_ext) %>% 
  ggplot(aes(x = c_muerte, y = muertes, fill = ano)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=brewer.pal(n = 10, name = "Spectral"))+
  guides(fill = guide_legend(title = "Año")) +
  labs(x = "Causa de muerte", y = "N muertes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        panel.background = element_rect(fill = "#FFFFFF"))

ggsave(filename = "Grafico causa externa.jpg", plot = last_plot(), path = "Salidas")



# Graficas proyeccion mortalidad ------------------------------------------

df_proyect_mort <- read_rds("Bases/Proyecciones mortalidad/Proyeccion_mortalidad_edad_agrupada.rds")

df_proyect_mort %>% 
  filter(nom_dpto != "Nacional") %>% 
  group_by(ano) %>% 
  summarise(muertes = sum(muertes)) %>% 
  ggplot(aes(x = ano, y = muertes)) +
  geom_line(linetype = 3,lwd = 1.1)
  












