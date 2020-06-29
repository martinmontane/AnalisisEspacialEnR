salud <- read_csv("data/HospitalesYCentrosSalud.csv")

salud <- salud %>%
  mutate(id=row_number(),
         provincia="Ciudad de Buenos Aires")
centrosBarriales <- read_sf("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-medicos-barriales/centros-medicos-barriales.geojson") %>% 
  mutate(NOMBRE=gsub("N°","Nº",NOMBRE)) %>% 
  rename(nombre=NOMBRE)
CESAC <- read_sf("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-de-salud-y-accion-comunitaria-cesac/centros-de-salud-y-accion-comunitaria.geojson") %>% 
  mutate(nombre=gsub("Centro de Salud Nivel 1 ","",nombre)) %>% 
  mutate(nombre=gsub("N°|N°","Nº",nombre))
hospitales <- read_sf("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/hospitales/hospitales.geojson") %>% mutate(NOMBRE=tolower(NOMBRE))  %>% 
  rename(nombre=NOMBRE)
centrosDeSaludCABA <- bind_rows(centrosBarriales,CESAC,hospitales)
salud <- salud %>% 
  mutate(Establecimiento=gsub("N°","Nº",Establecimiento)) %>% 
  mutate(Hospital=ifelse(substr(Establecimiento,1,8) %in% "Hospital",TRUE,FALSE)) %>% 
  mutate(Establecimiento=ifelse(Hospital,tolower(Establecimiento),Establecimiento))
centrosDeSaludCABA <- centrosDeSaludCABA %>% mutate(longGCBA=st_coordinates(centrosDeSaludCABA)[,1],
                          latGCBA=st_coordinates(centrosDeSaludCABA)[,2])

salud <- left_join(salud,centrosDeSaludCABA %>% select(nombre,latGCBA,longGCBA),by=c("Establecimiento"="nombre"))
salud <- salud %>% filter(!is.na(latGCBA))
salud <- data.frame(salud)
salud$geometry <- NULL
saludGCBA <- salud
save(file = "saludGCBA.RData", saludGCBA)
