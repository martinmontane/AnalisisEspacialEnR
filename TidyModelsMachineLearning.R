# Carga de librerías
library(tidyverse)

# Cargamos los datos de Córdoba
load("datosCordoba.RData")
glimpse(datosCordoba)

# Nos fijamos si hay NAs en las columnas y vemos también qué valores toman
datosCordoba %>%
  select(type,type_i18n,country,place.l2,property.operation,property.operation_i18n,property.type,property.operation_i18n, property.currency, property.price_period) %>% 
  map(function(x) table(x,useNA = "always")) 

# Nos quedamos solo con los casos en los cuales el anuncio es en dólares, la operación es de venta y es una casa o un departamento
datosCordoba <- datosCordoba %>% 
                filter(property.currency == "USD" & property.operation=="Venta" & property.type %in% c("Casa","Departamento"))

# Nos fijamos si las variables numéricas tienen la clase que corresponde
datosCordoba %>%
  select(property.rooms,
         property.surface_covered,
         property.price,
         property.surface_total,
         property.bathrooms,
         property.bedrooms) %>% 
  map(function(x) class(x))

# Primero convertimos nuestro data.frame en tibble(), solo para aprovechar que se ve mejor en la consola
datosCordoba <- datosCordoba %>% as_tibble()
# Y ahora convertimos a las columnas como numéricas
datosCordoba <- datosCordoba %>% 
  mutate_at(.vars = c("property.rooms",
                      "property.surface_covered",
                      "property.price",
                      "property.surface_total",
                      "property.bedrooms",
                      "property.bathrooms"),as.numeric)

# Nos fijamos a ver si funcionó...
datosCordoba %>%
    select(property.rooms,
         property.surface_covered,
         property.price,
         property.surface_total,
         property.bathrooms,
         property.bedrooms) %>% 
  map(function(x) class(x))

# Creamos una variable de años
datosCordoba <- datosCordoba %>% 
                mutate(year=substr(created_on,start = 1,stop = 4))

# Eliminamos todos los casos donde no haya datos de latitud o longitud
datosCordoba <- datosCordoba %>% 
                filter(!is.na(place.lat) & !is.na(place.lon))

# Cargamos sf y los cnvertimos en un objeto sf
library(sf)
cordobaSF <- st_as_sf(datosCordoba,coords = c("place.lon","place.lat"),crs=4326)

library(leaflet)
leaflet(cordobaSF) %>% 
  addTiles() %>% 
  addCircles()

library(osmdata)
polyCordoba <- getbb(place_name = "Córdoba Capital, Argentina",format_out = "sf_polygon")


leaflet(polyCordoba) %>% 
  addTiles() %>% 
  addPolygons()

polyCordoba <- st_transform(polyCordoba,5343)
cordobaSF <- st_transform(cordobaSF,5343)

cordobaSF <-  cordobaSF %>% 
              mutate(cordobaCapital=as.logical(st_intersects(cordobaSF,polyCordoba,sparse=FALSE)))

cordobaSF <-  cordobaSF %>% 
              mutate(colorCordoba = ifelse(cordobaCapital==TRUE,"green","red"))
leaflet(cordobaSF %>% st_transform(4326)) %>% 
  addTiles() %>% 
  addCircles(color = ~colorCordoba)
# Nos quedamos con los casos de Córdoba capital y que tienen precio y superficie mayor a cero
cordobaSF <- cordobaSF %>%
             filter(cordobaCapital==TRUE & property.price>0 & property.surface_covered>0 & property.surface_total> 0)

# Esto nos sirve para tener en un solo tibble cuantos NA faltan por cada una de las variables
cordobaSF %>% 
  map_dfr(function(x) sum(is.na(x))) %>%
  pivot_longer(cols = 1:ncol(.)) %>%
  arrange(desc(value))

cordobaSF %>% slice(1) %>% pull(property.description)


cordobaSF %>% slice(1) %>% pull(property.title)

cordobaSF <- cordobaSF %>%
                mutate(gimnasio = ifelse(grepl(pattern = "gym|gimn",x = property.description) |
                                         grepl(pattern = "gym|gimn", x = property.title), 1, 0),
                       cochera = ifelse(grepl(pattern = "coch|garage",x = property.description) |
                                         grepl(pattern = "coch|garage", x = property.title), 1, 0),
                       pileta = ifelse(grepl(pattern = "pileta|piscina",x = property.description) |
                                       grepl(pattern = "pileta|piscina", x = property.title), 1, 0))

cordobaSF <- cordobaSF %>%
             mutate(property.description=str_replace_all(string = property.description,
                                                         pattern = regex("un|uno",ignore_case = TRUE),
                                                         "1")) %>% 
  mutate(property.description=str_replace_all(string = property.description,
                                              pattern = regex("dos",ignore_case = TRUE),
                                              "2")) %>% 
  mutate(ambientes=as.numeric(str_extract(pattern=regex("(\\d)(?= dorm)",ignore_case = TRUE),string = property.description)))


cordobaSF <- cordobaSF %>%
             select(place.l4,property.rooms,property.surface_covered,property.price,property.surface_total,year,gimnasio,cochera,pileta,ambientes)


datosCordoba <- as_tibble(cordobaSF) %>% select(-geometry)
# Eliminamos el cordobaSF
rm(cordobaSF)

## -------------------------------------------------------------------------------------------------------------
datosCordoba <- datosCordoba %>%
  filter(!is.na(property.surface_covered)) %>% 
  mutate_at(.vars = c("place.l4","year","cochera","pileta"),as.factor) 

# Completamos los casos que falta en ambientes con los datos de property rooms
datosCordoba  <- datosCordoba %>% mutate(ambientes=ifelse(is.na(ambientes),property.rooms,ambientes))

datosCordoba <- datosCordoba %>% filter(!is.na(ambientes)) %>% select(-property.rooms)

## -------------------------------------------------------------------------------------------------------------
knitr::include_graphics("data/kfolds.png")

## Intro tidymodels
library(tidymodels)
set.seed(10)
# Guardamos un 10% de los datos para después de haber entrenado el modelo
cordobaSplit <- initial_split(datosCordoba,prop = 0.3)
cordobaTrain <- training(cordobaSplit)
cordobaTesting <- testing(cordobaSplit)

# Recipes
cordobaRecipe <- recipe(property.price ~.,data=cordobaTrain)

treePrep <- prep(cordobaRecipe)


tuneSpec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>% 
  set_mode("regression") %>% 
  set_engine("ranger")

tuneWf <-workflow() %>% 
  add_recipe(treePrep) %>% 
  add_model(tuneSpec)


set.seed(234)
trees_folds <- vfold_cv(cordobaTrain)

doParallel::registerDoParallel()
set.seed(345)
tuneRes <- tune_grid(
  tuneWf,
  resamples=trees_folds,
  grid = 20
)

tuneRes %>% select_best(metric = "rsq")

tuneRes %>% 
  collect_metrics() %>% 
  filter(.metric=="rmse") %>% 
  ggplot() + geom_point(aes(x=mtry,color=factor(min_n),y=mean))


set.seed(456)
regular_res <- tune_grid(
  tuneWf,
  resamples=trees_folds,
  grid=rf_grid
)

bestRMSE <- select_best(regular_res, metric="rmse")
final_rf <- finalize_model(
  tuneSpec,
  bestRMSE
)

library(vip)

final_rf %>% 
  set_engine("ranger",importance="permutation") %>% 
  fit(property.price ~., data=cordobaTrain) %>% 
  vip(geom="point")


# Dos formas de agregar información espacial: el precio de los vecinos y latitud y longitud
...


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

