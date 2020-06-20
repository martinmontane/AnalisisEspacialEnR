## -------------------------------------------------------------------------------------------------------------
load("datosCordoba.RData")


## -------------------------------------------------------------------------------------------------------------
library(tidyverse)
glimpse(datosCordoba)


## -------------------------------------------------------------------------------------------------------------
datosCordoba %>%
  select(type,type_i18n,country,place.l2,property.operation,property.operation_i18n,property.type,property.operation_i18n, property.currency, property.price_period) %>% 
  map(function(x) table(x,useNA = "always")) 


## -------------------------------------------------------------------------------------------------------------
datosCordoba <- datosCordoba %>% 
                filter(property.currency == "USD" & property.operation=="Venta" & property.type %in% c("Casa","Departamento"))


## -------------------------------------------------------------------------------------------------------------
datosCordoba %>%
  select(property.rooms,
         property.surface_covered,
         property.price,
         property.surface_total,
         property.bathrooms,
         property.bedrooms) %>% 
  map(function(x) class(x))


## -------------------------------------------------------------------------------------------------------------
# Primero convertimos nuestro data.frame en tibble(), solo para aprovechar que se ve mejor en la consola
datosCordoba <- datosCordoba %>% as_tibble()
datosCordoba <- datosCordoba %>% 
  mutate_at(.vars = c("property.rooms",
                      "property.surface_covered",
                      "property.price",
                      "property.surface_total",
                      "property.bedrooms",
                      "property.bathrooms"),as.numeric)


## -------------------------------------------------------------------------------------------------------------
datosCordoba %>%
    select(property.rooms,
         property.surface_covered,
         property.price,
         property.surface_total,
         property.bathrooms,
         property.bedrooms) %>% 
  map(function(x) class(x))


## -------------------------------------------------------------------------------------------------------------
datosCordoba <- datosCordoba %>% 
                mutate(year=substr(created_on,start = 1,stop = 4))


## -------------------------------------------------------------------------------------------------------------
datosCordoba <- datosCordoba %>% 
                filter(!is.na(place.lat) & !is.na(place.lon))


## -------------------------------------------------------------------------------------------------------------
library(sf)
cordobaSF <- st_as_sf(datosCordoba,coords = c("place.lon","place.lat"),crs=4326)


## -------------------------------------------------------------------------------------------------------------
library(leaflet)
leaflet(cordobaSF) %>% 
  addTiles() %>% 
  addCircles()


## -------------------------------------------------------------------------------------------------------------
library(osmdata)
polyCordoba <- getbb(place_name = "Córdoba Capital, Argentina",format_out = "sf_polygon")


## -------------------------------------------------------------------------------------------------------------
leaflet(polyCordoba) %>% 
  addTiles() %>% 
  addPolygons()


## -------------------------------------------------------------------------------------------------------------
polyCordoba <- st_transform(polyCordoba,5343)
cordobaSF <- st_transform(cordobaSF,5343)


## -------------------------------------------------------------------------------------------------------------
cordobaSF <-  cordobaSF %>% 
              mutate(cordobaCapital=as.logical(st_intersects(cordobaSF,polyCordoba,sparse=FALSE)))


## -------------------------------------------------------------------------------------------------------------
cordobaSF <-  cordobaSF %>% 
              mutate(colorCordoba = ifelse(cordobaCapital==TRUE,"green","red"))
leaflet(cordobaSF %>% st_transform(4326)) %>% 
  addTiles() %>% 
  addCircles(color = ~colorCordoba)


## -------------------------------------------------------------------------------------------------------------
cordobaSF <- cordobaSF %>%
             filter(cordobaCapital==TRUE & property.price>0 & property.surface_covered>0 & property.surface_total> 0)


## -------------------------------------------------------------------------------------------------------------
cordobaSF %>% 
  map_dfr(function(x) sum(is.na(x))) %>%
  pivot_longer(cols = 1:ncol(.)) %>%
  arrange(desc(value))


## -------------------------------------------------------------------------------------------------------------
cordobaSF %>% slice(1) %>% pull(property.description)


## -------------------------------------------------------------------------------------------------------------
cordobaSF %>% slice(1) %>% pull(property.title)


## -------------------------------------------------------------------------------------------------------------
cordobaSF <- cordobaSF %>%
                mutate(gimnasio = ifelse(grepl(pattern = "gym|gimn",x = property.description) |
                                         grepl(pattern = "gym|gimn", x = property.title), 1, 0),
                       cochera = ifelse(grepl(pattern = "coch|garage",x = property.description) |
                                         grepl(pattern = "coch|garage", x = property.title), 1, 0),
                       pileta = ifelse(grepl(pattern = "pileta|piscina",x = property.description) |
                                       grepl(pattern = "pileta|piscina", x = property.title), 1, 0))


## -------------------------------------------------------------------------------------------------------------
cordobaSF <- cordobaSF %>%
             mutate(property.description=str_replace_all(string = property.description,
                                                         pattern = regex("un|uno",ignore_case = TRUE),
                                                         "1")) %>% 
  mutate(property.description=str_replace_all(string = property.description,
                                              pattern = regex("dos",ignore_case = TRUE),
                                              "2")) %>% 
  mutate(ambientes=as.numeric(str_extract(pattern=regex("(\\d)(?= dorm)",ignore_case = TRUE),string = property.description)))


## -------------------------------------------------------------------------------------------------------------
cordobaSF <- cordobaSF %>%
             select(place.l4,property.rooms,property.surface_covered,property.price,property.surface_total,year,gimnasio,cochera,pileta,ambientes)


## -------------------------------------------------------------------------------------------------------------
datosCordoba <- as_tibble(cordobaSF) %>% select(-geometry)
# Eliminamos el cordobaSF
rm(cordobaSF)

## -------------------------------------------------------------------------------------------------------------
datosCordoba <- datosCordoba %>%
  filter(!is.na(property.surface_covered)) %>% 
  mutate_at(.vars = c("place.l4","year","cochera","pileta"),as.factor) 
datosCordoba <- na.roughfix(datosCordoba)


## -------------------------------------------------------------------------------------------------------------
knitr::include_graphics("data/kfolds.png")

## Intro tidymodels

set.seed(10)
# Guardamos un 10% de los datos para después de haber entrenado el modelo
datosCordoba <- initial_split(datosCordoba, prop = .1)



## -------------------------------------------------------------------------------------------------------------
library(caret)
grupos <- createFolds(datosCordoba %>% pull(property.price), k = 5, list = FALSE)
table(grupos)


## -------------------------------------------------------------------------------------------------------------
datosCordoba <- datosCordoba %>% 
                mutate(grupoCV = grupos)


## -------------------------------------------------------------------------------------------------------------
mtryValores <- c(2:5)
ntreeValores <- seq(100,500,by=100)
grilla <- expand.grid(mtryValores,ntreeValores)
colnames(grilla) <- c("mtryValores","ntreeValores")


## -------------------------------------------------------------------------------------------------------------
library(randomForest)
datosCordobaFilled <- na.roughfix(datosCordoba)
salidaArbol <- map(1:nrow(grilla), function(combinacion){
    cat("mtry: ",grilla %>% slice(combinacion) %>% pull(mtryValores),". ntree: ",grilla %>% slice(combinacion) %>% pull(ntreeValores),"\r")
  mean(map_dbl(1:5, function(k){
    # Generamos el grupo de entrenamiento
    dataTraining <- datosCordobaFilled %>% filter(grupoCV != k)  %>% select(-grupoCV)
    # Entrenamos el random forest
    rf <- randomForest(formula= property.price ~.,
                       data=dataTraining,
                       mtry=grilla %>% slice(combinacion) %>% pull(mtryValores),
                       ntree=grilla %>% slice(combinacion) %>% pull(ntreeValores))
    # Predecimos sobre testing
    dataTesting <- dataTraining <-datosCordobaFilled %>% filter(grupoCV == k)  %>% select(-grupoCV)
    pred <- predict(rf,newdata = dataTesting)
    obs <- dataTesting %>% pull(property.price)
    mse <- sqrt(sum((obs-pred)^2)/length(obs))
  }))
})

