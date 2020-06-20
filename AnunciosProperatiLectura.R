library(jsonlite)
library(data.table)
# library(purrr)

files <- list.files("C:/Users/marti/OneDrive/Documents/AnunciosProperati03_06_2020/",pattern = "*.json")
datos <- lapply(c(1:length(files)), function(x){
  as.data.table(stream_in(file(paste("C:/Users/marti/OneDrive/Documents/AnunciosProperati03_06_2020/",files[[x]],sep=""))))
})

datos <- rbindlist(datos,use.names = TRUE,fill = TRUE)
# save(file = "C:/Users/marti/OneDrive/Documents/AnunciosProperati03_06_2020/datosCompletos.RData",datos)

datosCordoba <- datos[place.l1=="Argentina" & place.l2 %in% "Córdoba"]
datosCordoba <- datosCordoba[,!grepl(pattern = "*development*",ignore.case = TRUE,x = colnames(datosCordoba)),with=FALSE]
datosCordoba <- datosCordoba[!is.na(property.surface_covered) & !is.na(property.price) & !is.na(property.surface_total)]
save(file = "datosCordoba.RData",datosCordoba)
datosArgentina <- datos[place.l1=="Argentina"]
datosArgentina <- datosArgentina[sample.int(nrow(datosArgentina),size = 0.3*nrow(datosArgentina))]
datosArgentina$place.l1 <- NULL
datosArgentina <- datosArgentina[type=="Propiedad"] 
datosArgentina <- datosArgentina[,c("type","country","place.l2","place.l3","place.l4","created_on","place.lat","place.lon","property.operation",'property.type',"property.rooms",'property.surface_covered',"property.price","property.currency","property.price_period","property.surface_total")]
save(file="datosArgentina.RData",datosArgentina)
write.table("datosArgentina.csv",sep=";",row.names=FALSE,x=datosArgentina)



load("datosArgentina.RData")
