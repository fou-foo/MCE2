setwd("C:\\Users\\fou-f\\Documents\\GitHub\\MCE2\\4\\Lab\\Estancia observatorio econometrico\\MapaTestComplejo\\Code\\Objetos") # fijamos directoria de descarga de todos los datos del portal
#este objeto solo sirve para referencias puntos que fueron deinterees en el proyecto
load('puntos.Rdata') # este obejto contiene las coordenadas del objeto anterior
################## definicion de colores
###############
#morado <- rgb(t(col2rgb('purple')/255), alpha= 0.5)
#naranja <- rgb(t(col2rgb('orange')/255), alpha= 0.5)
#chido <- rgb(t(col2rgb('#0089E0')/255), alpha=0.5)
#color1 <- rgb(t(col2rgb('#FA00A4')/255), alpha=0.5)
#color2 <- rgb(t(col2rgb('#27D6C2')/255), alpha=0.5)
#color3 <- rgb(t(col2rgb('#442CBC')/255), alpha=0.5)
row.names(puntos) <- NULL
library(geoR)
library(sp)
library(rgdal)
class(puntos)
names(puntos)
y <- SpatialPointsDataFrame(coords = puntos[, c('long', 'lat')],
                            data = data.frame(puntos[,2] ), proj4string = CRS("+proj=longlat +datum=WGS84"))
class(y)
names(y) <- c('viajes')
# el mapa de CDMX fue facilitado por el Dr. Victor, pero TIENE EN UN CD EL DE MEXICO A NIVEL AGEB
mex <- readOGR(dsn="C:\\Users\\fou-f\\Documents\\GitHub\\MCE2\\4\\Lab\\Estancia observatorio econometrico\\MapaTestComplejo\\Mapa\\df\\df_municipal.shp")
plot(mex)
# esta linea hace seleccciona solo los 'municipios' que antes eran 'delegaciones' de CDMX
CDMX <- mex[as.character(mex@data$NOMBRE) %in% c('Miguel Hidalgo','CuauhtÃ©moc','Benito JuÃ¡rez'),]
plot(CDMX) # la region de interes, en el caso de la frontera norte habria que seleccionar estados o agebs (enlistados, es una sola linea de codigo)
library(rgeos)
Region <- gUnionCascaded(CDMX) # esta linea es importante porque hacerlo 'a mano me llevo ~8hrs' (no sabia que ya estaba implementado, hasta que Victor me dijo )
plot(Region) # solo quitar lineas es complicado, definir la frontera norte se puede, pero si requiere de varias horas de leer documentacion y programar
