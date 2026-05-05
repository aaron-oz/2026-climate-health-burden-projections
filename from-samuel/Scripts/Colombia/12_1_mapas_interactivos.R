
library("leaflet")
library("Rcpp")
library(tidyverse)
library(sf)
library(ggplot2)
library(mapdeck)
library(mapview)
library(viridis)
library(RColorBrewer)


# limpiar memoria
rm(list = ls()); invisible(gc())


cent <- st_read("Bases/Ambientales/Shapes/pixeles_unicos_reindex.shp")

dep <- st_read("Bases/Ambientales/Shapes/shape_departamentos.shp")
dep <- read_sf("Bases/Ambientales/Shapes/shape_departamentos.shp")
plot(dep)
st_crs(dep)

identical(st_crs(cent), st_crs(dep))
dep <- st_transform(dep, crs = st_crs(cent))
dep2 <- dep[,c(-2)]

wpop <- read.csv("Bases/Ambientales/WorldPop/ppp_COL_2020_1km_Aggregated.csv")
wpop <- wpop %>% 
  st_as_sf(coords = c("X", "Y"), crs = "4326")

# poblacion
gri5 <- st_read("Bases/Ambientales/Shapes/grilla_5.shp")
gri5 <- read_sf("Bases/Ambientales/Shapes/grilla_5.shp")
plot(gri5)
gri5$sum_z <- round(gri5$sum_z,0)
Poblacion <- gri5[,c(2,3)]

# temp diaria y pob
temp_d <- readRDS("Bases/Ambientales/temperatura_diaria_pixel.rds")
tempd <- temp_d[,c(1,3)]
temp <- tempd %>%
  rename(indx_rg = index_right) %>% 
  group_by(indx_rg) %>% 
  summarise(temp = mean(temperatura))

tempp <- left_join(gri5, temp)
tempp$temp <- round(tempp$temp,1)
Temperatura<- tempp[,c(4,3)]

# carga
mcc <- read.csv("Salidas/Capas mapas/Murtes_calor.csv")
mff <- read.csv("Salidas/Capas mapas/Murtes_frio.csv")
mno <- read.csv("Salidas/Capas mapas/Murtes_non_optimal.csv")
avpp <- read.csv("Salidas/Capas mapas/avpp_non_optimal.csv")
# base <- read.csv("Salidas/Capas mapas/costosapvppatribuiblesclima.csv")
base <- read.csv("Salidas/Capas mapas/costosapvpp_atribuibles_clima_final.csv")

mc <- mcc[,c(-1)]
mf <- mff[,c(-1)]
mno <- mno[,c(-1)]
avpp <- avpp[,c(-1)]
apvpp <- base[,c(3,34)]
smlva <- base[,c(3,55)]
pib <- base[,c(3,56)]

Muertes_calor <- dep2 %>% 
  left_join(mc) %>% 
  select(muertes_heat, geometry)

Muertes_frio <- dep2 %>% 
  left_join(mf) %>% 
  select(muertes_cold, geometry)

Muertes_temperatura <- Muertes_frio %>% 
  st_join(Muertes_calor, join = st_equals, left = TRUE) %>%
  mutate(muertes = muertes_cold + muertes_heat) %>% 
  select(muertes, geometry)

AVPP_temperatura <- dep2 %>% 
  left_join(avpp) %>% 
  select(avpp_non_optimal_temp, geometry)

apvpp2 <- apvpp %>%
  group_by(cod_depto) %>% 
  summarise(apvpp = sum(apvpp_non_optimal_temp))

APVPP <- dep2 %>% 
  left_join(apvpp2) %>% 
  select(apvpp, geometry)

APVPP$apvpp <- round(APVPP$apvpp, 1)

smlva2 <- smlva %>%
  group_by(cod_depto) %>% 
  # summarise(smlva = sum(ce_ch_smlva_non_optimal_temp)) %>%
  summarise(smlva = sum(ce_smlva_non_optemp_descont)) %>% 
  mutate(smlva = round(smlva / 1e9, 1))

SMLVA_TM <- dep2 %>% 
  left_join(smlva2) %>% 
  select(smlva, geometry)

pib2 <- pib %>%
  group_by(cod_depto) %>% 
  summarise(pib = sum(ce_pib_non_optemp_descont)) %>% 
  mutate(pib = round(pib / 1e9, 1))

PIB_TM <- dep2 %>% 
  left_join(pib2) %>% 
  select(pib, geometry)

# plot del departamento

# Create a continuous palette function
pal <- colorNumeric(
  palette = "Blues",
  domain = countries$gdp_md_est)

leaflet(dep) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.1,
              fillColor = ~colorQuantile("YlOrRd", cod_depto)(cod_depto),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))

pal = mapviewPalette("mapviewTopoColors")

mapview(Temperatura) + 
  mapView(dep, alpha.regions = (fillOpacity = 0.1))


mapview(Temperatura) + 
  mapView(Muertes_calor, col.regions = plasma) +
  mapView(Muertes_frio, col.regions = plasma) +
  AVPP_temperatura +
  APVPP +
  SMLVA_TM + 
  PIB_TM
  

mapview(PIB)
mapview(AVPP_temperatura, zcol = "avpp_non_optimal_temp", at = seq(0, 1200000, 200000),
        col.regions=brewer.pal(6, "YlOrRd"))
        
# color = mapviewGetOption("YlOrRd"))
#         color = brewer.pal(6,"YlOrRd"))


sevs <- readRDS("Bases/Estimaciones carga/PAF_SEVS_proyecciones.rds")

sevs2 <- sevs %>%
  group_by(ano, cod_depto) %>% 
  summarise(sevpr = mean(sev),
            sevsu = sum(sev))

sevs3 <- sevs %>%
  group_by(ano) %>% 
  summarise(sevpr = mean(sev),
            sevsu = sum(sev))

sevs4 <- sevs2 %>%
  group_by(ano) %>% 
  summarise(sevsu = sum(sevpr), sevpr = mean(sevpr))


plot(sevs3$ano, sevs3$sevpr)
plot(sevs3$ano, sevs3$sevsu)
plot(sevs4$ano, sevs4$sevpr)

plot(ano_pais$ano, ano_pais$temp)
ggplot(ano_pais, aes(ano)) + 
  geom_line(aes(y = temp), colour = 'black')

by(sevs2$sev, sevs2$cod_depto, plot)


mue <- base %>% 
  group_by(ano) %>% 
  summarise(mue_cal = sum(muertes_heat), 
            mue_frio = sum(muertes_cold))

ggplot(mue, aes(ano)) + 
  geom_line(aes(y = mue_frio), colour = 'blue') +
  geom_line(aes(y = mue_cal), colour = 'red')  +
  geom_line(aes(y = mue_cal), colour = 'red') +
  ylab("Deaths") + xlab("year") +
  ggtitle("Muertes por frio (azul) y calor (rojo), 2010-2019") +
  theme_bw()

ggplot(ano_pais, aes(ano)) + 
  geom_line(aes(y = paf_cold_s), colour = 'blue') +
  geom_line(aes(y = paf_heat_s), colour = 'red')  +
  ylab("Deaths") + xlab("year") +
  ggtitle("Muertes por frio (azul) y calor (rojo), 2010-2019") +
  theme_bw()

ggplot(ano_pais, aes(ano)) + 
  geom_line(aes(y = paf_cold_m), colour = 'blue') +
  geom_line(aes(y = paf_heat_m), colour = 'red')  +
  ylab("Deaths") + xlab("year") +
  ggtitle("Muertes por frio (azul) y calor (rojo), 2010-2019") +
  theme_bw()


