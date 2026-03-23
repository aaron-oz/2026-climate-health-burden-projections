# Mortality from temperature-related causes 2010-2020
# Author: Jean Carlo Pineda Lozano
# Date created: 3/3/2022
# Institution: World Bank



# clear memory
rm(list = ls()); invisible(gc())

# install required packages
packages <- c("tidyverse", "gtsummary", "flextable", "officer")
to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install)) { 
  install.packages(to_install)
}


# load libraries
library(tidyverse)
library(ggplot2)
library("RColorBrewer")
library(janitor)

# mortality data adjustment ---------------------------------------------------------

# data loading
df_mortalidad_hist <- readRDS("Bases/mortalidad_2010_2021.rds")
df_mortalidad_proy <- readRDS("Bases/Proyecciones mortalidad/Proyeccion_mortalidad_edad_agrupada.rds")
df_mortalidad_estudio <- readRDS("Bases/Bases mortalidad estudio depto residencia/df_mortalidad_estudiodepto_resid.rds")
df_pob <- readRDS("Bases/Proyecciones poblacionales/Proyecciones_poblacion_nacional_2010_2070.rds")
#national adjustment and 2018-2020

df_mortalidad_hist_sex <- df_mortalidad_hist %>% 
  group_by(ano) %>% 
  summarise(muertes_hist = n()) %>% 
  as.data.frame()

# %>% 
#   filter(ano >= 2018)


df_mortalidad_proy_sex <- df_mortalidad_proy %>% 
  filter(nom_dpto == "Nacional") %>% 
  group_by(ano) %>% 
  summarise(muertes_proy = sum(muertes))%>% 
  as.data.frame() %>% 
  filter(ano <= 2021) %>% 
  left_join(df_mortalidad_hist_sex, by = "ano") %>% 
  mutate(ano = as.character(ano))

df_mort <- pivot_longer(data = df_mortalidad_proy_sex, 
                        cols = starts_with("muertes"),
                        names_to = "tipo")

ggplot(df_mort, aes(x = ano, y = value, group = tipo, color = tipo)) +
  geom_line(linetype = 7,lwd = 1.1) + 
  geom_point()+
  scale_color_discrete(labels = c("Historico", "Proyectado"))+
  geom_text(label = df_mort$value, 
            nudge_x=0.45, nudge_y=0.1,
            color = "black") +
  theme_minimal()


# specific cause --------------------------------------------------------

# national and age group aggregation
df_mortalidad_gru_edad <- df_mortalidad_estudio %>% 
  group_by(ano, sexo, gru_ed1, c_muerte) %>% 
  summarise(muertes = sum(muertes)) %>% 
  rename(edad = gru_ed1)

df_pob$edad <- as.character(df_pob$edad)

#rate calculation
df_mort_tasa <- left_join(df_mortalidad_gru_edad, df_pob, by = c("ano", "sexo", "edad"))
df_mort_tasa$tasa_esp <- round((df_mort_tasa$muertes/df_mort_tasa$poblacion)*100000, 2)



################################################
# national aggregation
df_mortalidad_nal <- df_mortalidad_estudio %>% 
  group_by(ano, c_muerte) %>% 
  summarise(muertes = sum(muertes))

#rate calculation
df_mort_tasa_nal <- df_mortalidad_nal %>% 
  left_join(df_pob %>% 
              group_by(ano) %>% 
              summarise(poblacion = sum(poblacion)))

df_mort_tasa_nal$tasa_esp <- round((df_mort_tasa_nal$muertes/df_mort_tasa_nal$poblacion) *100000,
                                   digits = 2)

# plot all causes of death
df_mort_tasa_nal %>% 
  # filter(c_muerte == "accidentes_trafico") %>% 
  ggplot(aes(x = ano, y = tasa_esp, group = c_muerte, color = c_muerte)) +
  geom_line(linetype = 7,lwd = 1.1) + 
  geom_point()+
  # scale_color_discrete(labels = c("Historico", "Proyectado"))+
  # geom_text(label = df_mort$value, 
  #           nudge_x=0.45, nudge_y=0.1,
  #           color = "black") +
  theme_minimal()


# plot first cause
df_mort_tasa_nal %>% 
  filter(c_muerte == "accidentes_trafico") %>% 
  ggplot(aes(x = ano, y = tasa_esp)) +
  geom_line(linetype = 7,lwd = 1.1) + 
  geom_point()+
  # scale_color_discrete(labels = c("Historico", "Proyectado"))+
  # geom_text(label = df_mort$value, 
  #           nudge_x=0.45, nudge_y=0.1,
  #           color = "black") +
  labs(title =df_mort_tasa_nal$c_muerte) +
  theme_minimal()


# iterate plots for all causes of death

lista_c_muerte <- unique(df_mort_tasa_nal$c_muerte)

for (i in seq_along(lista_c_muerte)) {
  Grafico_c_muerte =
    ggplot(subset(df_mort_tasa_nal, df_mort_tasa_nal$c_muerte==lista_c_muerte[i]),
           aes(x = ano, y = tasa_esp)) +
    geom_line(linetype = 7,lwd = 0.5, color = "red") + 
    # geom_point()+
    geom_text(aes(label = tasa_esp), vjust = -0.3, color = "black", size = 6) +
    theme_minimal()+
    labs(x="Año",
         y="Tasa especifica") +
    theme(text = element_text(size=10),
          plot.title = element_text(size=rel(2), vjust=2, face="bold", color="black", lineheight=1.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position="none",
          axis.title.x = element_text(face="bold", vjust=1.5, colour="black", size=rel(1.5)),
          axis.title.y = element_text(face="bold", vjust=1.5, colour="black", size=rel(1.5)),
          axis.text = element_text(colour = "black")) +
    ggtitle(paste("Causa de muerte: ", lista_c_muerte[i])) #Set a title per group
  
  print (Grafico_c_muerte)
  
  # save plot as .jpeg
  ggsave(Grafico_c_muerte,
         file=paste("Salidas/Grafico tendencia", lista_c_muerte[i], ".jpeg", sep=''),
         width = 5, height = 2.5)
}

ggsave(Grafico_c_muerte,
       file=paste("Salidas/Grafico tendencia", lista_c_muerte[i], ".jpeg", sep=''))
       

