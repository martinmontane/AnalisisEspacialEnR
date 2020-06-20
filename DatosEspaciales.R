## ----message=FALSE , tidy=TRUE, tidy.opts=list(width.cutoff=60)-----------------------------------------------
library(tidyverse) # Paquete multiuso
library(sf) # Paquete clave para manipular datos espaciales
library(leaflet) # Uno de los paquetes para 


## ---- tidy=TRUE, tidy.opts=list(width.cutoff=60)--------------------------------------------------------------
# Creamos un Data Frame con los datos necesarios
datos <- data.frame(lat = c(-34.714656, 51.532068),
                    long = c(-58.785999, -0.177305),
                    ubicacion = c("UTDT", "Abbey Road"))
# Lo convertimos a un objeto sf
puntosEspaciales <- st_as_sf(datos, 
                             coords = c("long", "lat"),
                             crs = 4326)
# st_distance() nos permite encontrar la diferencia en la unidad que diga el CRS (sistema de coordenadas de referencia)
st_distance(puntosEspaciales) # En metros
st_distance(puntosEspaciales)/1000 # En kilómetros


## -------------------------------------------------------------------------------------------------------------
leaflet(puntosEspaciales) %>% 
  addTiles() %>% 
  addMarkers()


## ----out.width="800px",echo=FALSE, fig.cap="La proyección MERCATOR distorsiona nuestra percepción de los tamaños", fig.align="center", fig.pos="htb!"----
knitr::include_graphics(path = "data/Tutorial 3/projections.png")


## ----out.width="800px",echo=FALSE, fig.cap="La proyección MOLLWEIDE mantiene la representación de los tamaños", fig.align="center", fig.pos="htb!"----
knitr::include_graphics(path = "data/Tutorial 3/mollyout.png")


## ----message=FALSE, warning=FALSE, echo=FALSE-----------------------------------------------------------------
library(sf)
calles <- read_sf("data/callejero-rar/callejero.shp")


## ----message=FALSE, warning=FALSE, eval=FALSE-----------------------------------------------------------------
## # Cargamos la librería de SF
## library(sf)
## calles <- read_sf("callejero-rar/callejero.shp")


## -------------------------------------------------------------------------------------------------------------
st_crs(calles)


## ----message=FALSE, warning=FALSE, echo=FALSE-----------------------------------------------------------------
radiosCensales <- read_sf("data/caba_radios_censales.geojson")


## ----message=FALSE, warning=FALSE, eval=FALSE-----------------------------------------------------------------
## radiosCensales <- read_sf("caba_radios_censales.geojson")


## -------------------------------------------------------------------------------------------------------------
glimpse(radiosCensales)


## ----message=FALSE, warning=FALSE, echo=FALSE-----------------------------------------------------------------
hospitales <- read_delim("data/Hospitales.csv",delim = ";")


## ----message=FALSE, warning=FALSE, eval=FALSE-----------------------------------------------------------------
## hospitales <- read_delim("Hospitales.csv",delim = ";")


## -------------------------------------------------------------------------------------------------------------
glimpse(hospitales)


## -------------------------------------------------------------------------------------------------------------
hospitales <- st_as_sf(hospitales,coords=c("long","lat"), crs=4326)


## ----message=FALSE, warning=FALSE, echo=FALSE-----------------------------------------------------------------
manzanas <- read_sf("data/manzanas.geojson")


## ----message=FALSE, warning=FALSE, eval=FALSE-----------------------------------------------------------------
## manzanas <- read_sf("manzanas.geojson")


## -------------------------------------------------------------------------------------------------------------
hospitales <- st_transform(hospitales, crs="+proj=tmerc +lat_0=-34.6297166 +lon_0=-58.4627 +k=1 +x_0=100000 +y_0=100000 +ellps=intl +units=m +no_defs")


## -------------------------------------------------------------------------------------------------------------
coberturaHospitales <- st_buffer(hospitales,dist = 1000)


## -------------------------------------------------------------------------------------------------------------
coberturaHospitales <- coberturaHospitales %>% summarise(cobertura=TRUE) 


## -------------------------------------------------------------------------------------------------------------
avenidas <- calles %>% filter(tipo_c == "AVENIDA")


## -------------------------------------------------------------------------------------------------------------
manzanas <- st_transform(manzanas,crs = st_crs(hospitales))


## ----message=FALSE, warning=FALSE-----------------------------------------------------------------------------
manzanasCentroides <- st_centroid(manzanas)
ggplot() +
  geom_sf(data = manzanas) +
  geom_sf(data = manzanasCentroides,color="red", size= 0.001)


## -------------------------------------------------------------------------------------------------------------
avenidas <- st_transform(avenidas,crs = st_crs(hospitales))


## -------------------------------------------------------------------------------------------------------------
distanciaAvenidas <- st_distance(manzanasCentroides,avenidas)


## -------------------------------------------------------------------------------------------------------------
# 12.520 filas (centroides de manzanas) y 6,758 columnas (tramos de avenidas)
dim(distanciaAvenidas)


## -------------------------------------------------------------------------------------------------------------
# 1 significa filas, 2 columnas. functon(x) min(x) significa que para cada fila devuelva el valor mínimo
avenidaMasCercana <- apply(distanciaAvenidas,1,function(x) min(x))
# Rendondeamos
avenidaMasCercana <- round(avenidaMasCercana,0)


## -------------------------------------------------------------------------------------------------------------
manzanas <- manzanas %>% mutate(distanciaAvenida=avenidaMasCercana)


## -------------------------------------------------------------------------------------------------------------
manzanasCentroides <- st_join(manzanasCentroides,coberturaHospitales)


## -------------------------------------------------------------------------------------------------------------
manzanasCentroides <- manzanasCentroides %>% mutate(cobertura=ifelse(is.na(cobertura),FALSE,cobertura))


## -------------------------------------------------------------------------------------------------------------
radiosCensales <- radiosCensales %>% mutate(densidadPob=POBLACION/AREA_KM2)
radiosCensales <- radiosCensales %>% st_transform(radiosCensales,crs=st_crs(hospitales))
manzanasCentroides <- st_join(manzanasCentroides,radiosCensales)


## -------------------------------------------------------------------------------------------------------------
# Elegimos solo las variables que queremos unir, antes lo convertimos en data frame para poder perder la columna geometry
manzanasCentroides <- manzanasCentroides %>% 
                      as.data.frame() %>%
                      select(SM,densidadPob,HOGARES_NBI,cobertura)
manzanas <-  left_join(manzanas,manzanasCentroides,by="SM")


## -------------------------------------------------------------------------------------------------------------
# Queremos que nos muestre en que porcentaje de estos está cada observación...
quiebres <- c(0,0.2,0.4,0.6,0.8,1)

manzanas <- manzanas %>%
            mutate(cat_densidad=cut(densidadPob,breaks = quantile(densidadPob,quiebres,na.rm = TRUE ),include.lowest = TRUE),
                   cat_NBI=cut(HOGARES_NBI,breaks = quantile(HOGARES_NBI,quiebres,na.rm = TRUE ),include.lowest = TRUE),
                   cat_distanciaAv=cut(-distanciaAvenida,breaks = quantile(-distanciaAvenida,quiebres,na.rm = TRUE ),include.lowest = TRUE))


## -------------------------------------------------------------------------------------------------------------
ggplot() +
  geom_sf(data=manzanas %>% filter(!is.na(cat_NBI)) ,aes(fill=cat_NBI), color=NA) +
  scale_fill_viridis_d() +
  theme_minimal() +
  coord_sf(datum=NA)


## -------------------------------------------------------------------------------------------------------------
# En el caso de cobertura convertira 0 cuando era FALSE y 1 cuando era TRUE
manzanas <- manzanas %>%
            mutate(cat_densidad=as.numeric(cat_densidad),
                   cat_NBI=as.numeric(cat_NBI),
                   cat_distanciaAv=as.numeric(cat_distanciaAv),
                   cat_cobertura=as.numeric(!cobertura))


## -------------------------------------------------------------------------------------------------------------
manzanas <- manzanas %>%
            mutate(IDS=cat_densidad*0.1+cat_NBI*0.3+cat_distanciaAv*0.1+cat_cobertura*0.5,
                   IDS=ifelse(IDS>quantile(IDS,probs = 0.9,na.rm = TRUE),TRUE,FALSE))


## -------------------------------------------------------------------------------------------------------------
# Podemos agrupar a los radioscensales por barrio para que nos queden los polígonos de los barrios.
# Tambien es posible bajarlos directamente desde la página del GCBA
barrios <- radiosCensales %>% group_by(BARRIO) %>% summarise(n())
ggplot() +
  geom_sf(data=barrios) +
  geom_sf(data=manzanas %>% filter(!is.na(IDS)) ,aes(fill=IDS), color=NA) +
  scale_fill_manual(values = c(NA,"#f03b20")) +
  guides(fill=FALSE) +
  theme_minimal() +
  coord_sf(datum=NA) +
  labs(title="Índice de Demanda de Salud",
       subtitle="Ciudad de Buenos Aires")

