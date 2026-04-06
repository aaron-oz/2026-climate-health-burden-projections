# DANE population projections 2010-2050(70)
# Author: Jean Carlo Pineda-Lozano
# Date created: 24/1/2023
# Institution: World Bank



# Clear memory
rm(list = ls()); invisible(gc())

# Install required packages
packages <- c("tidyverse", "gtsummary", "flextable", "officer", "gganimate")
to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install)) { 
  install.packages(to_install)
}


# Load libraries
library(tidyverse)
library(flextable)
library(gtsummary)
library(officer)
library(ggplot2)
library(RColorBrewer)
library(gganimate)
install.packages("areaplot")
library(areaplot)



# Data loading ---------------------------------------------------------

pob_depto_2010_2050 <- readRDS("E:/wb_carga_ temperatura/Bases/Proyecciones poblacionales/Proyecciones_poblacion_depto_2010_2050.rds")
pob_nacional_2010_2070 <- readRDS("E:/wb_carga_ temperatura/Bases/Proyecciones poblacionales/Proyecciones_poblacion_nacional_2010_2070.rds")



# Plots ----------------------------------------------------------------

# General population trend 2010-2070 (bars)

pob_nacional_2010_2070 %>% 
  group_by(ano) %>% 
  summarise(poblacion = sum (poblacion)) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = ano, y = poblacion)) + 
  geom_col(fill = 4) +
  labs(x = "Año", y = "Población") +
  ylim(c(0,70000000))+
  theme(panel.background = element_rect(fill = "#FFFFFF"))

ggsave(filename = "Grafico poblacion nal 210-2070.jpg", plot = last_plot(), path = "Salidas")

# Population pyramids by decade

# 2010

pob_2010 = pob_nacional_2010_2070 %>% 
  filter(ano == 2010)

pob_2070 = pob_nacional_2010_2070 %>% 
  filter(ano == 2010)

  ggplot(pob_2070, aes(x = edad,
                y = poblacion,
                fill = sexo)) +
  geom_bar(data = subset(pob_nacional_2010_2070, sexo == "M") %>% mutate(poblacion = -poblacion),
           stat = "identity", width = 0.5, fill = 4) +
  geom_bar(data = subset(pob_nacional_2010_2070, sexo == "F"),
           stat = "identity", width = 0.5, fill = 2) +
  coord_flip() +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) +
  labs(title = "Pirámide Poblacional Colombia, 2010",
       x = "",
       y = "Hombres                        Mujeres",
       caption = "Fuente: DANE") +
  scale_y_continuous(labels = abs)


ggsave(filename = "Piramide poblacional 2010.jpg", plot = last_plot(), path = "Salidas")


# Population by departments

pob_depto_2010_2050 %>% 
  group_by(nom_dpto, ano) %>% 
  summarise(poblacion = sum(poblacion)) %>% 
  ggplot(aes(x = ano, y = poblacion, color = nom_dpto))+
  geom_line(linetype = 7,linewidth = 0.1) +
  theme(legend.position = "none")+
  # geom_text_repel(aes(label = nom_dpto)) +
  labs(x = "Año",y = "Población por deparatamento")


