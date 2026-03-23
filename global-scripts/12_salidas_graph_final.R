# Mortalidad causas relacionadas con temperatura 2010-2019
# salidas finales, mapas y figuras
# Autor: Jean Carlo Pineda Lozano
# fecha de creacion: 11/05/2023
# Institucion: Banco Mundial



# limpiar memoria
rm(list = ls()); invisible(gc())

# instalar paquetes necesarios
packages <- c("tidyverse", "gtsummary", "flextable", "officer", "sf", "rnaturalearthdata", "classInt")
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
library(sf)
library(rnaturalearthdata)
library(classInt)
library(ggspatial)
library(cowplot)
library(raster)
library(sp)
library(rgdal)
library(sf)
library(rgeos)
library(maptools)
library(mapview)
library(RColorBrewer)
library(classInt)
library(tidyverse)
library(tidyr)
library(dplyr)
library(reshape2)
library(readxl)


# cargue de datos ---------------------------------------------------------

# cargue de base de datos apmp
carga_atribuible <- readRDS("Bases/Estimaciones carga/avpp_atribuibles.rds")
shape_colombia <- st_read("Bases/Ambientales/Shapes/MGN_DPTO_POLITICO.shp")
df_pob <- readRDS("Bases/Proyecciones poblacionales/Proyecciones_poblacion_depto_2010_2050_postcovid.rds") %>% 
  rename(cod_depto = cod_dpto)


# ajuste de datos ---------------------------------------------------------
pob_year_depto <- df_pob %>% filter(ano <= 2019) %>% 
  group_by(ano, cod_depto) %>% 
  summarise(poblacion = sum(poblacion))

shape_colombia <- shape_colombia %>% 
  rename(cod_depto = DPTO_CCDGO) %>% 
  mutate(cod_depto = as.numeric(cod_depto)) %>% 
  dplyr::select(c(cod_depto, DPTO_CNMBR, geometry))

# st_write(shape_colombia, "Bases/Ambientales/Shapes/shape_departamentos.shp")

# construccion de capas ---------------------------------------------------

# muertes por departamento atribuibles al calor, frio y total
#muertes frio
death_cold_all_causes <- carga_atribuible %>% 
  group_by(cod_depto) %>% 
  summarise(muertes_cold = round(sum(muertes_cold)))
  
write.csv(death_cold_all_causes, "Salidas/Capas mapas/Murtes_frio.csv")

# muertes calor
death_heat_all_causes <- carga_atribuible %>% 
  group_by(cod_depto) %>% 
  summarise(muertes_heat = round(sum(muertes_heat)))
write.csv(death_heat_all_causes, "Salidas/Capas mapas/Murtes_calor.csv")

# muertes temp no optima
death_non_optim_all_causes <- carga_atribuible %>% 
  group_by(cod_depto) %>% 
  summarise(muertes_non_optimal_temp = sum(muertes_non_optimal_temp))
write.csv(death_heat_all_causes, "Salidas/Capas mapas/Murtes_non_optimal.csv")



#AVPP frio
avpp_cold_all_causes <- carga_atribuible %>% 
  group_by(cod_depto) %>% 
  summarise(avpp_cold = round(sum(avpp_cold)))

write.csv(avpp_cold_all_causes, "Salidas/Capas mapas/avpp_frio.csv")

# AVPP calor
avpp_heat_all_causes <- carga_atribuible %>% 
  group_by(cod_depto) %>% 
  summarise(avpp_heat = round(sum(avpp_heat)))
write.csv(avpp_heat_all_causes, "Salidas/Capas mapas/avpp_calor.csv")

# AVPP temp no optima
avpp_non_optim_all_causes <- carga_atribuible %>% 
  group_by(cod_depto) %>% 
  summarise(avpp_non_optimal_temp = round(sum(avpp_non_optimal_temp)))
write.csv(avpp_non_optim_all_causes, "Salidas/Capas mapas/avpp_non_optimal.csv")



# tasas mortalidad
#muertes frio
tasas_cold_all_causes <- carga_atribuible %>% 
  group_by(ano, cod_depto) %>% 
  summarise(muertes_cold = round(sum(muertes_cold))) %>% 
  left_join(pob_year_depto) %>% 
  mutate(tasa = muertes_cold/poblacion) %>% 
  group_by(cod_depto) %>% 
  summarise(tasa = mean(tasa)) %>% 
  mutate(tasa = round(tasa*100000,2))

write.csv(tasas_cold_all_causes, "Salidas/Capas mapas/Tasas_mortalidad_frio.csv")

# muertes calor
tasas_heat_all_causes <- carga_atribuible %>% 
  group_by(ano, cod_depto) %>% 
  summarise(muertes_heat = round(sum(muertes_heat))) %>% 
  left_join(pob_year_depto) %>% 
  mutate(tasa = muertes_heat/poblacion) %>% 
  group_by(cod_depto) %>% 
  summarise(tasa = mean(tasa)) %>% 
  mutate(tasa = round(tasa*100000,2))

write.csv(tasas_heat_all_causes, "Salidas/Capas mapas/Tasas_mortalidad_calor.csv")


# muertes temp no optima
tasas_non_optim_all_causes <- carga_atribuible %>% 
  group_by(ano, cod_depto) %>% 
  summarise(muertes_non_optimal_temp = round(sum(muertes_non_optimal_temp))) %>% 
  left_join(pob_year_depto) %>% 
  mutate(tasa = muertes_non_optimal_temp/poblacion) %>% 
  group_by(cod_depto) %>% 
  summarise(tasa = mean(tasa)) %>% 
  mutate(tasa = round(tasa*100000,2))

write.csv(tasas_non_optim_all_causes, "Salidas/Capas mapas/Tasas_mortalidad_non_optimal.csv")



#  mapa muertes frio ----------------------------------------------------
  
  union <- left_join(shape_colombia, death_cold_all_causes)
  
  #Paleta de colores
  numclass <- 4 #numero de categorias
  colores <- brewer.pal(numclass,"Blues")#generación de según el objeto anterior colores
  
  #determinación de los umbrales entre categorias (método cuantil)
  var <- union$muertes_cold
  brks <- classIntervals(var, n=numclass, style = "quantile") #otros Style:"fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih" or "headtails"
  brks <- brks$brks
  print(brks)
  
  #determinar que color corresponde a cada valor de densidad
  codigos_num <- findInterval(var,brks, all.inside = T)
  codigos_color <- colores[codigos_num]
  
  #Se agrega la nueva columna con los intervalos a representar
  
  union <- mutate(union,
                  COD_ORDINAL=codigos_num)
  
  #ubicamos la fila de san Andres # para el caso departamental
  union_sai <- union %>% filter(cod_depto == 88)
  union_col <- union %>% filter(cod_depto != 88)
  
  
  g2 <- ggplot() +
    geom_sf(data = union_col, aes(fill = union_col$COD_ORDINAL)) +
    #  geom_sf(data = dp1, aes(fill = crp)) +
    ggtitle(label = 'Cold atributable deaths Colombia 2010-2019') +
    scale_fill_gradientn(name = 'Number of deaths', 
                         colours = RColorBrewer::brewer.pal(n = 4, name = 'Blues'),
                         na.value = 'white', labels = c("0-1","2-195","196-1667","1668-99523"),
                         guide = "legend") +
    #scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
    annotation_scale(location = "br", width_hint = 0.5) +#libreria ggspatial
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), # 0.2 # 0.3
                           style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = extent(union_col)[1:2], ylim = extent(union_col)[3:4]) +
    #geom_text(data= dengue_col, aes(x = X, y = Y, label = COD_FINAL),
    #         color = "darkblue", fontface = "bold", check_overlap = FALSE, size = 3.3) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
          panel.grid.major = element_blank(),
          # legend.key.width = unit(5, 'line'),
          panel.grid.minor = element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0.005, 0.005),
          legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(0.5, "cm"),
          legend.background = element_rect(fill = alpha('white', 1), colour = alpha('white', 0.4))) +
    labs(x = 'Longitud', y = 'Latitud', caption = "World Bank 2023") 
  g2
  
  num = union_sai$COD_ORDINAL #escriba aquí en valor de intervalo al que pertenece san Andres
  a <- brewer.pal(n = 4, name = 'Blues')
  a[num] #el resultado en la consola se pega en dos lineas abajo en el campo fill
  
  g3 <- ggplot() +
    geom_sf(data = union_sai, fill = aes(fill = "#EFF3FF")) +
    ggtitle(label = 'San Andres y Providencia') +
    theme_bw() + 
    coord_sf(xlim = c(-81.76,-81.67), ylim = c(12.48,12.6))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = 'none',
          plot.title = element_text(hjust = 0.5, size = 7, face = "bold"))
  
  ###ggdraw, cowplot libreria
  gg_inset <- ggdraw() +
    draw_plot(g2) +
    # draw_plot(g1, x = 0.72, y = 0.76, width = 0.28, height = 0.19) +
    draw_plot(g3, x = 0.21, y = 0.77, width = 0.22, height = 0.16)
  
  ggsave(plot = gg_inset,
         filename = 'Salidas/Cold atributable deaths Colombia 2010-2019.png', units = 'in', width = 10, height = 10, dpi = 300)
  
 
  
  
#  mapa muertes calor ----------------------------------------------------

union <- left_join(shape_colombia, death_heat_all_causes)
  

  #Paleta de colores
  numclass <- 5 #numero de categorias
  colores <- brewer.pal(numclass,"YlOrRd")#generación de según el objeto anterior colores
  
  #determinación de los umbrales entre categorias (método cuantil)
  var <- union$muertes_heat
  brks <- classIntervals(var, n=numclass, style = "quantile") #otros Style:"fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih" or "headtails"
  brks <- brks$brks
  print(brks)
  
  #determinar que color corresponde a cada valor de densidad
  codigos_num <- findInterval(var,brks, all.inside = T)
  codigos_color <- colores[codigos_num]
  
  #Se agrega la nueva columna con los intervalos a representar
  
  union <- mutate(union,
                  COD_ORDINAL=codigos_num)

  #ubicamos la fila de san Andres # para el caso departamental
  union_sai <- union %>% filter(cod_depto == 88)
  union_col <- union %>% filter(cod_depto != 88)
  
  g2 <- ggplot() +
    geom_sf(data = union_col, aes(fill = union_col$COD_ORDINAL)) +
    #  geom_sf(data = dp1, aes(fill = crp)) +
    ggtitle(label = 'Heat atributable deaths Colombia 2010-2019') +
    scale_fill_gradientn(name = 'Number of deaths', 
                         colours = RColorBrewer::brewer.pal(n = 4, name = 'YlOrRd'),
                         na.value = 'white', labels = c("0-5", "6-112", "113-689", "690-6944"),
                         guide = "legend") +
    #scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
    annotation_scale(location = "br", width_hint = 0.5) +#libreria ggspatial
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), # 0.2 # 0.3
                           style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = extent(union_col)[1:2], ylim = extent(union_col)[3:4]) +
    #geom_text(data= dengue_col, aes(x = X, y = Y, label = COD_FINAL),
    #         color = "darkblue", fontface = "bold", check_overlap = FALSE, size = 3.3) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
          panel.grid.major = element_blank(),
          # legend.key.width = unit(5, 'line'),
          panel.grid.minor = element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0.005, 0.005),
          legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(0.5, "cm"),
          legend.background = element_rect(fill = alpha('white', 1), colour = alpha('white', 0.4))) +
    labs(x = 'Longitud', y = 'Latitud', caption = "World Bank 2023") 
    
  g2
  num = union_sai$COD_ORDINAL #escriba aquí en valor de intervalo al que pertenece san Andres
  a <- brewer.pal(n = 5, name = 'YlOrRd')
  a[num] #el resultado en la consola se pega en dos lineas abajo en el campo fill
  
  g3 <- ggplot() +
    geom_sf(data = union_sai, fill = aes(fill = "#FFFFB2")) +
    ggtitle(label = 'San Andres y Providencia') +
    theme_bw() + 
    coord_sf(xlim = c(-81.76,-81.67), ylim = c(12.48,12.6))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = 'none',
          plot.title = element_text(hjust = 0.5, size = 7, face = "bold"))
  
  ###ggdraw, cowplot libreria
  gg_inset <- ggdraw() +
    draw_plot(g2) +
    # draw_plot(g1, x = 0.72, y = 0.76, width = 0.28, height = 0.19) +
    draw_plot(g3, x = 0.21, y = 0.77, width = 0.22, height = 0.16)
  
  ggsave(plot = gg_inset,
         filename = 'Salidas/Heat atributable deaths Colombia 2010-2019.png', units = 'in', width = 10, height = 10, dpi = 300)
  
  # ggsave(plot = gg_inset,
  #        filename = 'Salidas/Cold atributable deaths Colombia 2010-2019.pdf', units = 'in', width = 10, height = 10, dpi = 300)
  # 
  

#  mapa muertes non optimaltemp ----------------------------------------------------
  
  union <- left_join(shape_colombia, death_non_optim_all_causes)
  
  #Paleta de colores
  numclass <- 4 #numero de categorias
  colores <- brewer.pal(numclass,"BuPu")#generación de según el objeto anterior colores
  
  #determinación de los umbrales entre categorias (método cuantil)
  var <- union$muertes_non_optimal_temp
  brks <- classIntervals(var, n=numclass, style = "quantile") #otros Style:"fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih" or "headtails"
  brks <- brks$brks
  print(brks)
  
  #determinar que color corresponde a cada valor de densidad
  codigos_num <- findInterval(var,brks, all.inside = T)
  codigos_color <- colores[codigos_num]
  
  #Se agrega la nueva columna con los intervalos a representar
  
  union <- mutate(union,
                  COD_ORDINAL=codigos_num)
  
  #ubicamos la fila de san Andres # para el caso departamental
  union_sai <- union %>% filter(cod_depto == 88)
  union_col <- union %>% filter(cod_depto != 88)
  
  
  g2 <- ggplot() +
    geom_sf(data = union_col, aes(fill = union_col$COD_ORDINAL)) +
    #  geom_sf(data = dp1, aes(fill = crp)) +
    ggtitle(label = 'Non optimal temperatures atributable deaths Colombia 2010-2019') +
    scale_fill_gradientn(name = 'Number of deaths', 
                         colours = RColorBrewer::brewer.pal(n = 4, name = 'BuPu'),
                         na.value = 'white', labels = c("0-4","5-675","676-2466","2467-99523"),
                         guide = "legend") +
    #scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
    annotation_scale(location = "br", width_hint = 0.5) +#libreria ggspatial
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), # 0.2 # 0.3
                           style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = extent(union_col)[1:2], ylim = extent(union_col)[3:4]) +
    #geom_text(data= dengue_col, aes(x = X, y = Y, label = COD_FINAL),
    #         color = "darkblue", fontface = "bold", check_overlap = FALSE, size = 3.3) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.6, size = 15, face = "bold"),
          panel.grid.major = element_blank(),
          # legend.key.width = unit(5, 'line'),
          panel.grid.minor = element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0.005, 0.005),
          legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(0.5, "cm"),
          legend.background = element_rect(fill = alpha('white', 1), colour = alpha('white', 0.4))) +
    labs(x = 'Longitud', y = 'Latitud', caption = "World Bank 2023") 
  g2
  
  num = union_sai$COD_ORDINAL #escriba aquí en valor de intervalo al que pertenece san Andres
  a <- brewer.pal(n = 4, name = 'BuPu')
  a[num] #el resultado en la consola se pega en dos lineas abajo en el campo fill
  
  g3 <- ggplot() +
    geom_sf(data = union_sai, fill = aes(fill = "#EDF8FB")) +
    ggtitle(label = 'San Andres y Providencia') +
    theme_bw() + 
    coord_sf(xlim = c(-81.76,-81.67), ylim = c(12.48,12.6))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = 'none',
          plot.title = element_text(hjust = 0.5, size = 7, face = "bold"))
  
  ###ggdraw, cowplot libreria
  gg_inset <- ggdraw() +
    draw_plot(g2) +
    # draw_plot(g1, x = 0.72, y = 0.76, width = 0.28, height = 0.19) +
    draw_plot(g3, x = 0.21, y = 0.77, width = 0.22, height = 0.16)
  
  ggsave(plot = gg_inset,
         filename = 'Salidas/Non optimal temperatures atributable deaths Colombia 2010-2019.png', units = 'in', width = 10, height = 10, dpi = 300)
  
  
  
  
  
  
  
#  mapa tasas mortalidad frio ----------------------------------------------------
  
  union <- left_join(shape_colombia, tasas_cold_all_causes)
  
  #Paleta de colores
  numclass <- 5 #numero de categorias
  colores <- brewer.pal(numclass,"Blues")#generación de según el objeto anterior colores
  
  #determinación de los umbrales entre categorias (método cuantil)
  var <- union$tasa
  brks <- classIntervals(var, n=numclass, style = "quantile") #otros Style:"fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih" or "headtails"
  brks <- brks$brks
  print(brks)
  
  #determinar que color corresponde a cada valor de densidad
  codigos_num <- findInterval(var,brks, all.inside = T)
  codigos_color <- colores[codigos_num]
  
  #Se agrega la nueva columna con los intervalos a representar
  
  union <- mutate(union,
                  COD_ORDINAL=codigos_num)
  
  #ubicamos la fila de san Andres # para el caso departamental
  union_sai <- union %>% filter(cod_depto == 88)
  union_col <- union %>% filter(cod_depto != 88)
  
  
  g2 <- ggplot() +
    geom_sf(data = union_col, aes(fill = union_col$COD_ORDINAL)) +
    #  geom_sf(data = dp1, aes(fill = crp)) +
    ggtitle(label = 'Mortality rate attributable to cold Colombia 2010-2019') +
    scale_fill_gradientn(name = 'Rate x 100.000', 
                         colours = RColorBrewer::brewer.pal(n = 4, name = 'Blues'),
                         na.value = 'white', labels = c("0.0-0.4","0.5-5.4","5.5-17.9","18.0-136.4"),
                         guide = "legend") +
    #scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
    annotation_scale(location = "br", width_hint = 0.5) +#libreria ggspatial
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), # 0.2 # 0.3
                           style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = extent(union_col)[1:2], ylim = extent(union_col)[3:4]) +
    #geom_text(data= dengue_col, aes(x = X, y = Y, label = COD_FINAL),
    #         color = "darkblue", fontface = "bold", check_overlap = FALSE, size = 3.3) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
          panel.grid.major = element_blank(),
          # legend.key.width = unit(5, 'line'),
          panel.grid.minor = element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0.005, 0.005),
          legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(0.5, "cm"),
          legend.background = element_rect(fill = alpha('white', 1), colour = alpha('white', 0.4))) +
    labs(x = 'Longitud', y = 'Latitud', caption = "World Bank 2023") 
  g2
  
  num = union_sai$COD_ORDINAL #escriba aquí en valor de intervalo al que pertenece san Andres
  a <- brewer.pal(n = 4, name = 'Blues')
  a[num] #el resultado en la consola se pega en dos lineas abajo en el campo fill
  
  g3 <- ggplot() +
    geom_sf(data = union_sai, fill = aes(fill = "#EFF3FF")) +
    ggtitle(label = 'San Andres y Providencia') +
    theme_bw() + 
    coord_sf(xlim = c(-81.76,-81.67), ylim = c(12.48,12.6))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = 'none',
          plot.title = element_text(hjust = 0.5, size = 7, face = "bold"))
  
  ###ggdraw, cowplot libreria
  gg_inset <- ggdraw() +
    draw_plot(g2) +
    # draw_plot(g1, x = 0.72, y = 0.76, width = 0.28, height = 0.19) +
    draw_plot(g3, x = 0.21, y = 0.77, width = 0.22, height = 0.16)
  
  ggsave(plot = gg_inset,
         filename = 'Salidas/Mortality rate attributable to cold Colombia 2010-2019.png', units = 'in', width = 10, height = 10, dpi = 300)
  
  
  
  
  
#  mapa tasas mortalidad calor ----------------------------------------------------
  
  union <- left_join(shape_colombia, tasas_heat_all_causes)
  
  
  #Paleta de colores
  numclass <- 5 #numero de categorias
  colores <- brewer.pal(numclass,"YlOrRd")#generación de según el objeto anterior colores
  
  #determinación de los umbrales entre categorias (método cuantil)
  var <- union$tasa
  brks <- classIntervals(var, n=numclass, style = "quantile") #otros Style:"fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih" or "headtails"
  brks <- brks$brks
  print(brks)
  
  #determinar que color corresponde a cada valor de densidad
  codigos_num <- findInterval(var,brks, all.inside = T)
  codigos_color <- colores[codigos_num]
  
  #Se agrega la nueva columna con los intervalos a representar
  
  union <- mutate(union,
                  COD_ORDINAL=codigos_num)
  
  #ubicamos la fila de san Andres # para el caso departamental
  union_sai <- union %>% filter(cod_depto == 88)
  union_col <- union %>% filter(cod_depto != 88)
  
  g2 <- ggplot() +
    geom_sf(data = union_col, aes(fill = union_col$COD_ORDINAL)) +
    #  geom_sf(data = dp1, aes(fill = crp)) +
    ggtitle(label = 'Mortality rate attributable to heat Colombia 2010-2019') +
    scale_fill_gradientn(name = 'Rate x 100.000', 
                         colours = RColorBrewer::brewer.pal(n = 4, name = 'YlOrRd'),
                         na.value = 'white', labels = c("0.0-0.04", "0.05-1.1", "1.2-3.4", 
                                                        "3.5-17.7"),
                         guide = "legend") +
    #scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
    annotation_scale(location = "br", width_hint = 0.5) +#libreria ggspatial
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), # 0.2 # 0.3
                           style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = extent(union_col)[1:2], ylim = extent(union_col)[3:4]) +
    #geom_text(data= dengue_col, aes(x = X, y = Y, label = COD_FINAL),
    #         color = "darkblue", fontface = "bold", check_overlap = FALSE, size = 3.3) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
          panel.grid.major = element_blank(),
          # legend.key.width = unit(5, 'line'),
          panel.grid.minor = element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0.005, 0.005),
          legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(0.5, "cm"),
          legend.background = element_rect(fill = alpha('white', 1), colour = alpha('white', 0.4))) +
    labs(x = 'Longitud', y = 'Latitud', caption = "World Bank 2023") 
  
  g2
  num = union_sai$COD_ORDINAL #escriba aquí en valor de intervalo al que pertenece san Andres
  a <- brewer.pal(n = 5, name = 'YlOrRd')
  a[num] #el resultado en la consola se pega en dos lineas abajo en el campo fill
  
  g3 <- ggplot() +
    geom_sf(data = union_sai, fill = aes(fill = "#FFFFB2")) +
    ggtitle(label = 'San Andres y Providencia') +
    theme_bw() + 
    coord_sf(xlim = c(-81.76,-81.67), ylim = c(12.48,12.6))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = 'none',
          plot.title = element_text(hjust = 0.5, size = 7, face = "bold"))
  
  ###ggdraw, cowplot libreria
  gg_inset <- ggdraw() +
    draw_plot(g2) +
    # draw_plot(g1, x = 0.72, y = 0.76, width = 0.28, height = 0.19) +
    draw_plot(g3, x = 0.21, y = 0.77, width = 0.22, height = 0.16)
  
  ggsave(plot = gg_inset,
         filename = 'Salidas/Mortality rate attributable to heat Colombia 2010-2019.png', units = 'in', width = 10, height = 10, dpi = 300)
  
  # ggsave(plot = gg_inset,
  #        filename = 'Salidas/Cold atributable deaths Colombia 2010-2019.pdf', units = 'in', width = 10, height = 10, dpi = 300)
  # 
  
  
  
  
  
#  mapa tasas non optimal temp ----------------------------------------------------
  
  union <- left_join(shape_colombia, tasas_non_optim_all_causes)
  
  #Paleta de colores
  numclass <- 4 #numero de categorias
  colores <- brewer.pal(numclass,"BuPu")#generación de según el objeto anterior colores
  
  #determinación de los umbrales entre categorias (método cuantil)
  var <- union$tasa
  brks <- classIntervals(var, n=numclass, style = "quantile") #otros Style:"fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih" or "headtails"
  brks <- brks$brks
  print(brks)
  
  #determinar que color corresponde a cada valor de densidad
  codigos_num <- findInterval(var,brks, all.inside = T)
  codigos_color <- colores[codigos_num]
  
  #Se agrega la nueva columna con los intervalos a representar
  
  union <- mutate(union,
                  COD_ORDINAL=codigos_num)
  
  #ubicamos la fila de san Andres # para el caso departamental
  union_sai <- union %>% filter(cod_depto == 88)
  union_col <- union %>% filter(cod_depto != 88)
  
  
  g2 <- ggplot() +
    geom_sf(data = union_col, aes(fill = union_col$COD_ORDINAL)) +
    #  geom_sf(data = dp1, aes(fill = crp)) +
    ggtitle(label = 'Mortality rate attributable to Non optimal temperatures Colombia 2010-2019') +
    scale_fill_gradientn(name = 'Rate x 100.000', 
                         colours = RColorBrewer::brewer.pal(n = 4, name = 'BuPu'),
                         na.value = 'white', labels = c("0.0-0.08","0.09-5.66", 
                                                        "5.67-18.3","18.4-136.4"),
                         guide = "legend") +
    #scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
    annotation_scale(location = "br", width_hint = 0.5) +#libreria ggspatial
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), # 0.2 # 0.3
                           style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = extent(union_col)[1:2], ylim = extent(union_col)[3:4]) +
    #geom_text(data= dengue_col, aes(x = X, y = Y, label = COD_FINAL),
    #         color = "darkblue", fontface = "bold", check_overlap = FALSE, size = 3.3) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.6, size = 13, face = "bold"),
          panel.grid.major = element_blank(),
          # legend.key.width = unit(5, 'line'),
          panel.grid.minor = element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0.005, 0.005),
          legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(0.5, "cm"),
          legend.background = element_rect(fill = alpha('white', 1), colour = alpha('white', 0.4))) +
    labs(x = 'Longitud', y = 'Latitud', caption = "World Bank 2023") 
  g2
  
  num = union_sai$COD_ORDINAL #escriba aquí en valor de intervalo al que pertenece san Andres
  a <- brewer.pal(n = 4, name = 'BuPu')
  a[num] #el resultado en la consola se pega en dos lineas abajo en el campo fill
  
  g3 <- ggplot() +
    geom_sf(data = union_sai, fill = aes(fill = "#EDF8FB")) +
    ggtitle(label = 'San Andres y Providencia') +
    theme_bw() + 
    coord_sf(xlim = c(-81.76,-81.67), ylim = c(12.48,12.6))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = 'none',
          plot.title = element_text(hjust = 0.5, size = 7, face = "bold"))
  
  ###ggdraw, cowplot libreria
  gg_inset <- ggdraw() +
    draw_plot(g2) +
    # draw_plot(g1, x = 0.72, y = 0.76, width = 0.28, height = 0.19) +
    draw_plot(g3, x = 0.21, y = 0.77, width = 0.22, height = 0.16)
  
  ggsave(plot = gg_inset,
         filename = 'Salidas/Mortality rate attributable to Non optimal temperatures Colombia 2010-2019.png', units = 'in', width = 10, height = 10, dpi = 300)
  
  
  
  
# tablas y graficas grupos de edad ----------------------------------------

carga_edad <- carga_atribuible %>% 
    mutate(gru_ed1 = case_when(
      gru_ed1 == "0-4" ~ "<5",
      gru_ed1 == "0-4" ~ "<5",
      gru_ed1 == "5-9" ~ "5-14",
      gru_ed1 == "10-14" ~ "5-14",
      gru_ed1 == "15-19" ~ "15-49",
      gru_ed1 == "20-24" ~ "15-49",
      gru_ed1 == "25-29" ~ "15-49",
      gru_ed1 == "30-34" ~ "15-49",
      gru_ed1 == "35-39" ~ "15-49",
      gru_ed1 == "40-44" ~ "15-49",
      gru_ed1 == "45-49" ~ "15-49",   
      gru_ed1 == "50-54" ~ "50-69",
      gru_ed1 == "55-59" ~ "50-69",
      gru_ed1 == "60-64" ~ "50-69",
      gru_ed1 == "65-69" ~ "50-69",
      gru_ed1 == "70-74" ~ "70+",
      gru_ed1 == "75-79" ~ "70+",
      gru_ed1 == ">80" ~ "70+",
      TRUE~ NA_character_
    ))

carga_edad$gru_ed1 <- factor(carga_edad$gru_ed1, 
                             levels = c("<5", "5-14","15-49", "50-69", "70+"))
  # tabla muertes frio, calor por edad

# Función para eliminar la notación científica y agregar separadores
marks_no_sci <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)

# Frio
ggplot(carga_edad, aes(x = sexo, y = muertes_cold, fill = gru_ed1)) + 
    geom_bar(stat = "identity")+
  # scale_fill_manual(values = c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c"))+
  scale_fill_brewer(palette = "Blues") +
  guides(fill = guide_legend(title = "Age"))+
  scale_y_continuous(labels = marks_no_sci)+
  labs(title = "Cold atributable deaths 2010-2019",
       x = "Sex",
       y = "# deaths")+
  theme_minimal()

# ggsave(filename = "Muertes frio edad 2010-2019.jpeg", plot = last_plot(), path = "Salidas")

# calor
ggplot(carga_edad, aes(x = sexo, y = muertes_heat, fill = gru_ed1)) + 
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = guide_legend(title = "Age"))+
  scale_y_continuous(labels = marks_no_sci)+
  labs(title = "Heat atributable deaths 2010-2019",
       x = "Sex",
       y = "# deaths")+
  theme_minimal()
  
# ggsave(filename = "Muertes calor edad 2010-20199.jpeg", plot = last_plot(), path = "Salidas")


# resumen -----------------------------------------------------------------

carga_atribuible %>% 
  group_by(c_muerte) %>% 
  summarise(avpp_heat = sum(avpp_heat),
            avpp_cold = sum(avpp_cold)) %>% 
  arrange(avpp_heat)

#poblacion por año
pob_year <- df_pob %>% filter(ano <= 2019) %>% group_by(ano) %>% summarise(poblacion = sum(poblacion))
#tasas nacionales
carga_ano <- carga_atribuible %>% 
  group_by(ano) %>% 
  summarise(muertes_non_optimal_temp = sum(muertes_non_optimal_temp),
            muertes_cold = sum(muertes_cold),
            muertes_heat = sum(muertes_heat)) %>% 
  left_join(pob_year) %>% 
  mutate(tasa_muertes_not = (muertes_non_optimal_temp/poblacion)*100000,
         tasa_muertes_cold = (muertes_cold/poblacion)*100000,
         tasa_muertes_heat = (muertes_heat/poblacion)*100000)






