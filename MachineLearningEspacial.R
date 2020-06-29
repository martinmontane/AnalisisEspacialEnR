## --------------------------------------------------------------------------------------------------------------
load("datosCordoba.RData")


## --------------------------------------------------------------------------------------------------------------
library(tidyverse)
glimpse(datosCordoba)


## --------------------------------------------------------------------------------------------------------------
datosCordoba %>%
  select(type,type_i18n,country,place.l2,property.operation,property.operation_i18n,property.type,property.operation_i18n, property.currency, property.price_period) %>% 
  map(function(x) table(x,useNA = "always")) 


## --------------------------------------------------------------------------------------------------------------
datosCordoba <- datosCordoba %>% 
                filter(property.currency == "USD" & property.operation=="Venta" & property.type %in% c("Casa","Departamento"))


## --------------------------------------------------------------------------------------------------------------
datosCordoba %>%
  select(property.rooms,
         property.surface_covered,
         property.price,
         property.surface_total,
         property.bathrooms,
         property.bedrooms) %>% 
  map(function(x) class(x))


## --------------------------------------------------------------------------------------------------------------
# Primero convertimos nuestro data.frame en tibble(), solo para aprovechar que se ve mejor en la consola
datosCordoba <- datosCordoba %>% as_tibble()
datosCordoba <- datosCordoba %>% 
  mutate_at(.vars = c("property.rooms",
                      "property.surface_covered",
                      "property.price",
                      "property.surface_total",
                      "property.bedrooms",
                      "property.bathrooms"),as.numeric)


## --------------------------------------------------------------------------------------------------------------
datosCordoba %>%
    select(property.rooms,
         property.surface_covered,
         property.price,
         property.surface_total,
         property.bathrooms,
         property.bedrooms) %>% 
  map(function(x) class(x))


## --------------------------------------------------------------------------------------------------------------
datosCordoba <- datosCordoba %>% 
                mutate(year=substr(created_on,start = 1,stop = 4))


## --------------------------------------------------------------------------------------------------------------
datosCordoba <- datosCordoba %>% 
                filter(!is.na(place.lat) & !is.na(place.lon))


## --------------------------------------------------------------------------------------------------------------
library(sf)
cordobaSF <- st_as_sf(datosCordoba,coords = c("place.lon","place.lat"),crs=4326)


## --------------------------------------------------------------------------------------------------------------
library(leaflet)
leaflet(cordobaSF) %>% 
  addTiles() %>% 
  addCircles()


## --------------------------------------------------------------------------------------------------------------
library(osmdata)
polyCordoba <- getbb(place_name = "Córdoba Capital, Argentina",format_out = "sf_polygon")


## --------------------------------------------------------------------------------------------------------------
leaflet(polyCordoba) %>% 
  addTiles() %>% 
  addPolygons()


## --------------------------------------------------------------------------------------------------------------
polyCordoba <- st_transform(polyCordoba,5343)
cordobaSF <- st_transform(cordobaSF,5343)


## --------------------------------------------------------------------------------------------------------------
cordobaSF <-  cordobaSF %>% 
              mutate(cordobaCapital=as.logical(st_intersects(cordobaSF,polyCordoba,sparse=FALSE)))


## --------------------------------------------------------------------------------------------------------------
cordobaSF <-  cordobaSF %>% 
              mutate(colorCordoba = ifelse(cordobaCapital==TRUE,"green","red"))
leaflet(cordobaSF %>% st_transform(4326)) %>% 
  addTiles() %>% 
  addCircles(color = ~colorCordoba)


## --------------------------------------------------------------------------------------------------------------
cordobaSF <- cordobaSF %>%
             filter(cordobaCapital==TRUE & property.price>0 & property.surface_covered>0 & property.surface_total> 0)


## --------------------------------------------------------------------------------------------------------------
cordobaSF %>% 
  map_dfr(function(x) sum(is.na(x))) %>%
  pivot_longer(cols = 1:ncol(.),values_to="n_faltantes") %>%
  arrange(desc(n_faltantes))


## --------------------------------------------------------------------------------------------------------------
cordobaSF %>% slice(1) %>% pull(property.description)


## --------------------------------------------------------------------------------------------------------------
cordobaSF %>% slice(1) %>% pull(property.title)


## --------------------------------------------------------------------------------------------------------------
cordobaSF <- cordobaSF %>%
                mutate(gimnasio = ifelse(str_detect(pattern = regex("gym|gimn", ignore_case = TRUE),
                                                    string = property.description) |
                                         str_detect(pattern = regex("gym|gimn", ignore_case = TRUE),
                                                    string = property.title), 1, 0),
                       cochera = ifelse(str_detect(pattern = regex("coch|garage", ignore_case = TRUE),
                                                    string = property.description) |
                                         str_detect(pattern = regex("coch|garage", ignore_case = TRUE),
                                                    string = property.title), 1, 0),
                       pileta =ifelse(str_detect(pattern = regex("pileta|piscina", ignore_case = TRUE),
                                                    string = property.description) |
                                         str_detect(pattern = regex("pileta|piscina", ignore_case = TRUE),
                                                    string = property.title), 1, 0))


## --------------------------------------------------------------------------------------------------------------
cordobaSF <- cordobaSF %>%
             mutate(property.description=str_replace_all(string = property.description,
                                                         pattern = regex("un|uno",ignore_case = TRUE),
                                                         "1"),
                    property.description=str_replace_all(string = property.description,
                                              pattern = regex("dos",ignore_case = TRUE),
                                              "2"),
                    property.description=str_replace_all(string = property.description,
                                                         pattern = regex("tres",ignore_case = TRUE),
                                                         "1")) %>% 
  mutate(ambientes=str_extract(pattern=regex("(\\d)(?= dorm)",ignore_case = TRUE),string = property.description))


## --------------------------------------------------------------------------------------------------------------
cordobaSF <- cordobaSF %>%
             select(place.l4,ambientes,property.surface_covered,property.price,property.surface_total,year,gimnasio,cochera,pileta)


## --------------------------------------------------------------------------------------------------------------
anunciosCoords <- st_coordinates(cordobaSF) %>% as.data.frame()
datosCordoba <- as_tibble(cordobaSF) %>% select(-geometry) %>% bind_cols(anunciosCoords)
# Eliminamos algunos objetos que ya no nos sirven
rm(cordobaSF, anunciosCoords, polyCordoba)


## --------------------------------------------------------------------------------------------------------------
datosCordoba <- datosCordoba %>%
  filter(!is.na(property.surface_covered)) %>% 
  mutate_at(.vars = c("place.l4","year"),as.factor) 

datosCordoba <- datosCordoba %>% 
                filter(!is.na(ambientes))

## Tidy models

# Intro tidymodels
library(tidymodels)
set.seed(10)

cordobaSplit <- initial_split(datosCordoba)
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

rf_grid <- grid_regular(
  mtry(range = c(2,5)),
  min_n(range = c(3,100)),
  levels=20
)
set.seed(456)
regular_res <- tune_grid(
  tuneWf,
  resamples=trees_folds,
  grid=rf_grid
)
save(file="regularGrid.RData",
     regular_res,
     cordobaRecipe,
     cordobaSplit,
     cordobaTrain,
     cordobaTesting,
     trees_folds,
     tuneWf,
     treePrep,
     tuneSpec,
     rf_grid)

regular_res %>% 
  collect_metrics() %>% 
  filter(.metric=="rsq") %>% 
  ggplot() + geom_point(aes(x=min_n,color=factor(mtry),y=mean))

tuneRes %>% select_best(metric = "rsq")

bestRMSE <- select_best(regular_res, metric="rsq")
final_rf <- finalize_model(
  tuneSpec,
  bestRMSE
)

library(vip)

final_rf %>% 
  set_engine("ranger",importance="permutation") %>% 
  fit(property.price ~., data=cordobaTrain) %>% 
  vip(geom="point")



