# si te bajas los datos del drive esta parte no hace falta 
#setwd("~/Desktop/MCE2/4/Consultoria/Cervezas")
library(lubridate) #esta sí 
#dir()
#cervezas <- read.csv('Datos_Cerveza_UnaPlaza.txt', sep='|', stringsAsFactors = FALSE)
#cervezas$loc <- as.character(cervezas$loc)
#cervezas$id_fecha <- ymd(cervezas$id_fecha)
#cervezas$id_producto <- as.character(cervezas$id_producto)
#sapply(cervezas, class)
#catalogo <- read.csv('CATSKU_BI.txt', sep='|', stringsAsFactors = FALSE)
#catalogo$PRODUCTO_KEY <- as.character(catalogo$PRODUCTO_KEY)
#catalogo$UPC_CVE <- as.character(catalogo$UPC_CVE)
#sapply(catalogo, class)
#sku.cervezas <- unique(cervezas$id_producto)
#library(dplyr)
#catalogo %>% filter(PRODUCTO_KEY %in% sku.cervezas) -> catalogo.cervezas
#save(catalogo.cervezas, file = 'catalogoCervezas.rdata')
#save(cervezas, file = 'cervezas.rdata')
###########################################
load( file = 'catalogoCervezas.rdata') #catalogo con los detalles SOLO de las cervezas
load( file = 'cervezas.rdata') # todos los datos que nos paso Ramon 
library(ggplot2)
names(cervezas)
sapply(cervezas, class)
summary(cervezas)
library(dplyr)
names(cervezas)
cervezas$loc <- NULL #esta columna no se ocupa 
gc() #libera memoria
Sys.sleep(10) # el garbach collector de R no es determinista por eso siempre le doy 10 segundos para que sí libere espacio
nulos <- cervezas[is.na(cervezas$montomargen), ] #estos registros no tienen margen vamos a intentar imputarlos
imputacion <- cervezas[ cervezas$id_producto %in% unique(nulos$id_producto), ]
# pues para poder hacer una imputacion tendriamos que ver no solo el precio individual para tener el margen de utilidad sino 
# tambien el tiempo -- YO DIGO QUE ESO PONGAMOS AL JONATHAN HA HACERLO---
remove(imputacion, nulos)
cervezas <- na.omit(cervezas)
gc()
Sys.sleep(10)
perdidas <- cervezas %>%filter(montomargen<0) # aqui ni ganancia hay 
perdidas <- merge(perdidas, catalogo.cervezas,  all.x = TRUE, by.x='id_producto', by.y = 'PRODUCTO_KEY' )
# hechale un ojo al dataframe 'perdidas' no tienen descripcion y no parecen en el catalogo
#mira
perdidas1 <- cervezas %>%filter(montomargen<0) # aqui ni ganancia hay 
perdidas2 <- merge(perdidas1, catalogo,  all.x = TRUE, by.x='id_producto', by.y = 'PRODUCTO_KEY' )
perdidas2 <- na.omit(perdidas2)
ggplot(perdidas2, aes(id_fecha, monto, color=PRODUCTO_DES))+geom_line()+theme_minimal()
sapply(perdidas2[, !(sapply(perdidas2, class) %in% c('character', 'Date')) ], sum) # pues no luce que sea una gran perdida
################
# le quitamos a las cervezas los nulos y los negativos
cervezas <- na.omit(cervezas)
cervezas <- cervezas%>% filter(montomargen>=0)
remove(catalogo, perdidas, perdidas1, perdidas2)
gc()
Sys.sleep(10)
# vamos ir reduciendo registros primero vamos a ver el acumulado diario de las cervezas
cervezas <- cervezas %>% group_by(id_producto, id_fecha) %>%summarise(cantidad=sum(cantidad, na.rm=TRUE),
                                                          monto=sum(monto, na.rm = TRUE), 
                                                          montomargen=sum(montomargen, na.rm = TRUE))
gc()
Sys.sleep(10)
p1 <- ggplot(cervezas, aes(x=id_fecha, y=cantidad, color=factor(id_producto)))+
    geom_line()+theme_minimal()
p1
# se nota que gace falta segmentar
ggplot(cervezas, aes(x=id_fecha, y=cantidad))+
  geom_line()+theme_minimal()+ facet_grid(~id_producto)
# yo digo que ahuevo que hay que segmentar 
