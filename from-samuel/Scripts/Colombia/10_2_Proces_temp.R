# limpiar memoria
rm(list = ls()); invisible(gc())

library(tidyverse)
library(sf)
library(ggplot2)
library(janitor)

# cargue de datos ---------------------------------------------------------



# lote_1 <- readRDS("Bases/Ambientales/pixel_per_grid/pixel_per_grid-pop_lote1.rds")

dep <- read_sf("Bases/Ambientales/Shapes/MGN_DPTO_POLITICO.shp")
# plot(dep)
st_crs(dep)

# leer shape de pixeles unicos de temperatura
cent <- read_sf("Bases/Ambientales/Shapes/pixeles_unicos_reindex.shp")
cent <- st_read("Bases/Ambientales/Shapes/pixeles_unicos_reindex.shp")
st_crs(cent) # WGS 84
# plot(st_geometry(cent))

# transferir departamento
dep2 <- dep[, c(1,8:10)]
identical(st_crs(cent), st_crs(dep2))
dep2 <- st_transform(dep2, crs = st_crs(cent))

cent2 <- st_join(cent, dep2, join = st_within)

# transformar de puntos a grilla
cellSize <- 0.25
grid <- (st_bbox(cent2) + cellSize/2*c(-1,-1,1,1)) %>%
  st_make_grid(cellsize=c(cellSize, cellSize)) %>% st_sf()

# plot de puntos y grilla
ggplot() +
  geom_sf(data = dep2, fill = "transparent") +
  geom_sf(data = grid, fill = "transparent", color = "blue") +
  geom_sf(data = cent2, color = "red", size=0.1) +
  theme_bw()

st_write(grid, "Bases/Ambientales/Shapes/grilla_30km.shp")

# pasar atributos de uno a otro
identical(st_crs(grid), st_crs(cent2)) # true
grid2 <- st_join(grid, cent2, join = st_intersects)

# intersección con departamentos
grid3 <- grid2 %>% st_filter(dep2, .predicate = st_intersects)

  # plot de puntos y grilla
  ggplot() +
    geom_sf(data = dep2, fill = "transparent") +
    geom_sf(data = grid3, fill = "transparent", color = "blue") +
    theme_bw()

# leer base de poblacion 1km2 DANE
#pob <- st_read("/Users/samueldavid/Library/CloudStorage/OneDrive-Personal/BM/Flagship report/Carga temperatura/Datos/Poblacion/ShapefileGrid1km2_VihopeCNPV2018/Grid1km2_VihopeCNPV2018.shp")
#pob <- read_sf("/Users/samueldavid/Library/CloudStorage/OneDrive-Personal/BM/Flagship report/Carga temperatura/Datos/Poblacion/ShapefileGrid1km2_VihopeCNPV2018/Grid1km2_VihopeCNPV2018.shp")

#pob2 <- pob[,c(3,9:11)] # corto las variables que necesito
#st_crs(pob2) # WGS 84
#plot(st_geometry(pob2)) #revisar que quede bien

# worldpop ----------------------------------------------------------------


# 2010 --------------------------------------------------------------------

# leer base csv de world pop 2010
wpop_2010 <- read.csv("Bases/Ambientales/WorldPop/ppp_COL_2010_1km_Aggregated.csv")

wpop_2010 <- wpop_2010 %>% 
  st_as_sf(coords = c("X", "Y"), crs = "4326")

st_crs(wpop_2010) <- st_crs(grid3)
identical(st_crs(grid3), st_crs(wpop_2010)) # verdad

# join
grid4 <- st_join(grid3, wpop_2010, join = st_contains, left = TRUE) # demora
Sys.time()
grid5 <- grid4 %>% 
  group_by(indx_rg, geometry) %>% 
  summarise(sum_z = sum(Z))

grid5$ano <- 2010
st_write(grid5, "Bases/Ambientales/WorldPop/WorldPop_2010.shp")
grid5 <- st_drop_geometry(grid5)
write.csv(grid5,  "Bases/Ambientales/WorldPop/WorldPop_2010.csv", row.names = F)


# 2011 --------------------------------------------------------------------

# leer base csv de world pop 2011
wpop_2011 <- read.csv("Bases/Ambientales/WorldPop/ppp_COL_2011_1km_Aggregated.csv")

wpop_2011 <- wpop_2011 %>% 
  st_as_sf(coords = c("X", "Y"), crs = "4326")

st_crs(wpop_2011) <- st_crs(grid3)
identical(st_crs(grid3), st_crs(wpop_2011)) # verdad

# join
grid4 <- st_join(grid3, wpop_2011, join = st_contains, left = TRUE) # demora

grid5 <- grid4 %>% 
  group_by(indx_rg, geometry) %>% 
  summarise(sum_z = sum(Z))

grid5$ano <- 2011
st_write(grid5, "Bases/Ambientales/WorldPop/WorldPop_2011.shp")
grid5 <- st_drop_geometry(grid5)
write.csv(grid5,  "Bases/Ambientales/WorldPop/WorldPop_2011.csv", row.names = F)


# 2012 --------------------------------------------------------------------


# leer base csv de world pop 2012
wpop_2012 <- read.csv("Bases/Ambientales/WorldPop/ppp_COL_2012_1km_Aggregated.csv")

wpop_2012 <- wpop_2012 %>% 
  st_as_sf(coords = c("X", "Y"), crs = "4326")

st_crs(wpop_2012) <- st_crs(grid3)
identical(st_crs(grid3), st_crs(wpop_2012)) # verdad

# join
grid4 <- st_join(grid3, wpop_2012, join = st_contains, left = TRUE) # demora

grid5 <- grid4 %>% 
  group_by(indx_rg, geometry) %>% 
  summarise(sum_z = sum(Z))

grid5$ano <- 2012
st_write(grid5, "Bases/Ambientales/WorldPop/WorldPop_2012.shp")
grid5 <- st_drop_geometry(grid5)
write.csv(grid5,  "Bases/Ambientales/WorldPop/WorldPop_2012.csv", row.names = F)

# 2013 --------------------------------------------------------------------
# leer base csv de world pop 2013
wpop_2013 <- read.csv("Bases/Ambientales/WorldPop/ppp_COL_2013_1km_Aggregated.csv")

wpop_2013 <- wpop_2013 %>% 
  st_as_sf(coords = c("X", "Y"), crs = "4326")

st_crs(wpop_2013) <- st_crs(grid3)
identical(st_crs(grid3), st_crs(wpop_2013)) # verdad

# join
grid4 <- st_join(grid3, wpop_2013, join = st_contains, left = TRUE) # demora

grid5 <- grid4 %>% 
  group_by(indx_rg, geometry) %>% 
  summarise(sum_z = sum(Z))

grid5$ano <- 2013
st_write(grid5, "Bases/Ambientales/WorldPop/WorldPop_2013.shp")
grid5 <- st_drop_geometry(grid5)
write.csv(grid5,  "Bases/Ambientales/WorldPop/WorldPop_2013.csv", row.names = F)



# 2014 --------------------------------------------------------------------

# leer base csv de world pop 2014
wpop_2014 <- read.csv("Bases/Ambientales/WorldPop/ppp_COL_2014_1km_Aggregated.csv")

wpop_2014 <- wpop_2014 %>% 
  st_as_sf(coords = c("X", "Y"), crs = "4326")

st_crs(wpop_2014) <- st_crs(grid3)
identical(st_crs(grid3), st_crs(wpop_2014)) # verdad

# join
grid4 <- st_join(grid3, wpop_2014, join = st_contains, left = TRUE) # demora

grid5 <- grid4 %>% 
  group_by(indx_rg, geometry) %>% 
  summarise(sum_z = sum(Z))

grid5$ano <- 2014
st_write(grid5, "Bases/Ambientales/WorldPop/WorldPop_2014.shp")
grid5 <- st_drop_geometry(grid5)
write.csv(grid5,  "Bases/Ambientales/WorldPop/WorldPop_2014.csv", row.names = F)



# 2015 --------------------------------------------------------------------

# leer base csv de world pop 2015
wpop_2015 <- read.csv("Bases/Ambientales/WorldPop/ppp_COL_2015_1km_Aggregated.csv")

wpop_2015 <- wpop_2015 %>% 
  st_as_sf(coords = c("X", "Y"), crs = "4326")

st_crs(wpop_2015) <- st_crs(grid3)
identical(st_crs(grid3), st_crs(wpop_2015)) # verdad

# join
grid4 <- st_join(grid3, wpop_2015, join = st_contains, left = TRUE) # demora

grid5 <- grid4 %>% 
  group_by(indx_rg, geometry) %>% 
  summarise(sum_z = sum(Z))

grid5$ano <- 2015
st_write(grid5, "Bases/Ambientales/WorldPop/WorldPop_2015.shp")
grid5 <- st_drop_geometry(grid5)
write.csv(grid5,  "Bases/Ambientales/WorldPop/WorldPop_2015.csv", row.names = F)


# 2016 --------------------------------------------------------------------

# leer base csv de world pop 2016
wpop_2016 <- read.csv("Bases/Ambientales/WorldPop/ppp_COL_2016_1km_Aggregated.csv")

wpop_2016 <- wpop_2016 %>% 
  st_as_sf(coords = c("X", "Y"), crs = "4326")

st_crs(wpop_2016) <- st_crs(grid3)
identical(st_crs(grid3), st_crs(wpop_2016)) # verdad

# join
grid4 <- st_join(grid3, wpop_2016, join = st_contains, left = TRUE) # demora

grid5 <- grid4 %>% 
  group_by(indx_rg, geometry) %>% 
  summarise(sum_z = sum(Z))

grid5$ano <- 2016
st_write(grid5, "Bases/Ambientales/WorldPop/WorldPop_2016.shp")
grid5 <- st_drop_geometry(grid5)
write.csv(grid5,  "Bases/Ambientales/WorldPop/WorldPop_2016.csv", row.names = F)

# 2017 --------------------------------------------------------------------

# leer base csv de world pop 2017
wpop_2017 <- read.csv("Bases/Ambientales/WorldPop/ppp_COL_2017_1km_Aggregated.csv")

wpop_2017 <- wpop_2017 %>% 
  st_as_sf(coords = c("X", "Y"), crs = "4326")

st_crs(wpop_2017) <- st_crs(grid3)
identical(st_crs(grid3), st_crs(wpop_2017)) # verdad

# join
grid4 <- st_join(grid3, wpop_2017, join = st_contains, left = TRUE) # demora

grid5 <- grid4 %>% 
  group_by(indx_rg, geometry) %>% 
  summarise(sum_z = sum(Z))

grid5$ano <- 2017
st_write(grid5, "Bases/Ambientales/WorldPop/WorldPop_2017.shp")
grid5 <- st_drop_geometry(grid5)
write.csv(grid5,  "Bases/Ambientales/WorldPop/WorldPop_2017.csv", row.names = F)



# 2018 --------------------------------------------------------------------

# leer base csv de world pop 2018
wpop_2018 <- read.csv("Bases/Ambientales/WorldPop/ppp_COL_2018_1km_Aggregated.csv")

wpop_2018 <- wpop_2018 %>% 
  st_as_sf(coords = c("X", "Y"), crs = "4326")

st_crs(wpop_2018) <- st_crs(grid3)
identical(st_crs(grid3), st_crs(wpop_2018)) # verdad

# join
grid4 <- st_join(grid3, wpop_2018, join = st_contains, left = TRUE) # demora

grid5 <- grid4 %>% 
  group_by(indx_rg, geometry) %>% 
  summarise(sum_z = sum(Z))

grid5$ano <- 2018
st_write(grid5, "Bases/Ambientales/WorldPop/WorldPop_2018.shp")
grid5 <- st_drop_geometry(grid5)
write.csv(grid5,  "Bases/Ambientales/WorldPop/WorldPop_2018.csv", row.names = F)


# 2019 --------------------------------------------------------------------

# leer base csv de world pop 2018
wpop_2019 <- read.csv("Bases/Ambientales/WorldPop/ppp_COL_2019_1km_Aggregated.csv")

wpop_2019 <- wpop_2019 %>% 
  st_as_sf(coords = c("X", "Y"), crs = "4326")

st_crs(wpop_2019) <- st_crs(grid3)
identical(st_crs(grid3), st_crs(wpop_2019)) # verdad

# join
grid4 <- st_join(grid3, wpop_2019, join = st_contains, left = TRUE) # demora

grid5 <- grid4 %>% 
  group_by(indx_rg, geometry) %>% 
  summarise(sum_z = sum(Z))

grid5$ano <- 2019
st_write(grid5, "Bases/Ambientales/WorldPop/WorldPop_2019.shp")
grid5 <- st_drop_geometry(grid5)
write.csv(grid5,  "Bases/Ambientales/WorldPop/WorldPop_2019.csv", row.names = F)


# plot de puntos y grilla
ggplot() +
  geom_sf(data = grid5, aes(fill = sum_z)) +
  #  geom_sf(data = dp1, aes(fill = crp)) +
  ggtitle(label = 'relleno') +
  scale_fill_gradientn(name = 'Tasa x 100.000', 
                       colours = RColorBrewer::brewer.pal(n = 9, name = 'YlOrRd'),
                       na.value = 'white', guide = "legend")


# agrupacion poblacion 2010-2019 ------------------------------------------
names_wp <- list.files("Bases/Ambientales/WorldPop/",
                       pattern = "^WorldPop_.+\\.csv$", full.names = T)
list_wp <- lapply(names_wp, function(x) read.csv(x))

list_wp <- lapply(list_wp, clean_names) 

list_wp <- lapply(list_wp, function(x)
                    x %>% 
                    filter(!is.na(indx_rg))) 


wp_2010_19 <- bind_rows(list_wp)

write.csv(wp_2010_19, "Bases/Ambientales/WorldPop/WorldPop_2010_2019_pixel.csv")
saveRDS(wp_2010_19, "Bases/Ambientales/WorldPop/WorldPop_2010_2019_pixel.rds")

# Temperatura diaria por pixel 2010-2019
# Autor: Jean Carlo Pineda Lozano
# fecha de creacion: 2/05/2023
# fecha de modificacion: 2/05/2023
# Institucion: Banco Mundial



# limpiar memoria
rm(list = ls()); invisible(gc())

library(readr)
library(tidyverse)
library(openair)


# depuracion de bases -----------------------------------------------------
pix1 <- pix %>% 
  group_by(hour, index_right, dia, x, y) %>% 
  summarise(temperatura = mean(temp),
            DN = mean(DN)) 13_54


lote_1$fecha <- lote_1$dia + 40178 # esta es la fecha numerica para 31/12/2009
lote_1$coord <- paste0(lote_1$x, "-", lote_1$y)

lote_1 <- lote_1 %>% 
  mutate(temp = temp- 273.15, # se pasan de kelvin a Celsius
         fecha = as.Date(fecha, 
                         origin = "1899-12-30",
                         tz = "UTC")) %>% 
  select(-dia )


horas <- data.frame("hora" = 1:32830000,
                    "hora_exacta" = seq(1, 24, 1))


########### NC files
library(ncdf4)

our_nc_data <- nc_open("Proyecciones/timeseries-annual-mean_cmip6-ssp119-2015-2100.nc")
print(our_nc_data)

attributes(our_nc_data$var)
attributes(our_nc_data$dim)
# Get latitude and longitude with the ncvar_get function and store each into their own object:
lat <- ncvar_get(our_nc_data, "lat")
nlat <- dim(lat) #to check it matches the metadata: 181
lon <- ncvar_get(our_nc_data, "lon")
nlon <- dim(lon) #to check, should be 361
# Check your lat lon dimensions match the information in the metadata we explored before:
print(c(nlon, nlat))
# Get the time variable. Remember: our metadata said our time units are in seconds since 1981-01-01 00:00:00, so you will not see a recognizable date and time format, but a big number like "457185600". We will take care of this later
time <- ncvar_get(our_nc_data, "year")
head(time) # just to have a look at the numbers
tunits <- ncatt_get(our_nc_data, "year", "units") #check units
nt <- dim(time) #should be 86

#get the variable in "timeseries-tas-annual-mean"
lswt_array <- ncvar_get(our_nc_data, "timeseries-tas-annual-mean") 

fillvalue <- ncatt_get(our_nc_data, "timeseries-tas-annual-mean", "_FillValue")
dim(lswt_array) #to check; 361 181  86
#right away let's replace the nc FillValues with NAs
lswt_array[lswt_array==fillvalue$value] <- NA
lswt_array

# try it out:
lswt_slice <- lswt_array[ , , 3] 
lswt_slice <- lswt_array[ , , 25]

# and why not, draw it out:
image(lon, lat, lswt_slice)

#Create 2D matrix of long, lat and time
lonlattime <- as.matrix(expand.grid(lon,lat,time)) # this might take several seconds
#reshape whole lswt_array
lswt_vec_long <- as.vector(lswt_array)
length(lswt_vec_long) # 5619326
#Create data.frame
temp_w <- data.frame(cbind(lonlattime, lswt_vec_long))
colnames(temp_w) <- c('long','lat','year','temp')
head(temp_w)

# filtrar para coordinadas Colombia
temp_col <- temp_w %>%
  filter(lat > -4.29 & lat < 12.43 & long > -78.99 & long < -66.87)
head(temp_col)

# pasarlo a espacial
temp_col2 <- temp_col %>% 
  st_as_sf(coords = c("long", "lat"),
           crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

identical(st_crs(temp_col2), st_crs(dep2)) # no 
temp_col2 <- st_transform(temp_col2, crs = st_crs(dep2))

st_write(temp_col2, "puntosproyecc.shp")


ggplot() +
  geom_sf(data = dep2, fill = "transparent") +
  geom_sf(data = temp_col2, fill = "transparent", color = "blue")
# se ve que hay un punto cerca del norte de Bogota

st_write(grid, "grilla_30km.shp")

library(mapview)
mapview(temp_col2) # el punto mas cercano de Bog es el de Zipaquira

temp_col <- temp_col %>%
  filter(lat > -4.29 & lat < 12.43 & long > -78.99 & long < -66.87)
head(temp_col)

# filtrar unico punto
temp_bog <- temp_col %>%
  filter(lat > 4.9951 & lat < 5.00578 & long > -74.00683 & long < -73.99334)
View(temp_bog)

plot(temp_bog$year, temp_bog$temp) # tienen valores que tienen sentido

saveRDS(temp_bog, 'ptemp_bog_ssp119.rds')


###### NC files para los otros

our_nc_data <- nc_open("/Users/samueldosoriog/Library/CloudStorage/OneDrive-Personal/BM/Colombia/Flagship report/Carga temperatura/Datos/Temperatura/Proyecciones/NCDs/timeseries-annual-mean_cmip6-ssp370-2015-2100.nc")

attributes(our_nc_data$var)
attributes(our_nc_data$dim)
# Get latitude and longitude with the ncvar_get function and store each into their own object:
lat <- ncvar_get(our_nc_data, "lat")
nlat <- dim(lat) #to check it matches the metadata: 181
lon <- ncvar_get(our_nc_data, "lon")
nlon <- dim(lon) #to check, should be 361
# Check your lat lon dimensions match the information in the metadata we explored before:
print(c(nlon, nlat))
# Get the time variable. Remember: our metadata said our time units are in seconds since 1981-01-01 00:00:00, so you will not see a recognizable date and time format, but a big number like "457185600". We will take care of this later
time <- ncvar_get(our_nc_data, "year")
head(time) # just to have a look at the numbers
tunits <- ncatt_get(our_nc_data, "year", "units") #check units
dim(time) #should be 86

#get the variable in "timeseries-tas-annual-mean"
lswt_array <- ncvar_get(our_nc_data, "timeseries-tas-annual-mean") 

fillvalue <- ncatt_get(our_nc_data, "timeseries-tas-annual-mean", "_FillValue")
dim(lswt_array) #to check; 361 181  86
#right away let's replace the nc FillValues with NAs
lswt_array[lswt_array==fillvalue$value] <- NA

#Create 2D matrix of long, lat and time
lonlattime <- as.matrix(expand.grid(lon,lat,time)) # this might take several seconds
#reshape whole lswt_array
lswt_vec_long <- as.vector(lswt_array)
length(lswt_vec_long) # 5619326
#Create data.frame
temp_w <- data.frame(cbind(lonlattime, lswt_vec_long))
colnames(temp_w) <- c('long','lat','year','temp')
head(temp_w)

# filtrar para coordinadas Colombia
temp_bog <- temp_w %>%
  filter(lat > 4.9951 & lat < 5.00578 & long > -74.00683 & long < -73.99334)

plot(temp_bog$year, temp_bog$temp) # tienen valores que tienen sentido

saveRDS(temp_bog, 'ptemp_bog_ssp370.rds')

























