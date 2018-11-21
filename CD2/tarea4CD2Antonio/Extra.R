setwd("~/Desktop/Third/CD2/Tarea4")
library(lattice)
library(sp)
library(ggmap)
library(ggplot2)
library(easyGgplot2)
library(rgdal)
library(gstat)
library(geoR)
####################################
# lectura de datos y definicion de colores
morado <- rgb(t(col2rgb('purple')/255), alpha= 0.5)
naranja <- rgb(t(col2rgb('orange')/255), alpha= 0.5)
chido <- rgb(t(col2rgb('#0089E0')/255), alpha=0.5)
color1 <- rgb(t(col2rgb('#FA00A4')/255), alpha=0.5)
color2 <- rgb(t(col2rgb('#27D6C2')/255), alpha=0.5)
color3 <- rgb(t(col2rgb('#442CBC')/255), alpha=0.5)
############################
casos <- readOGR(dsn="obesidad/casos.shp")
class(casos)
casos@coords[dup.coords(casos@coords),] #casos repetidos
casos@coords <- jitterDupCoords(casos@coords, max= 0.01) #suavizamos para no perder dos observaciones de las 81
ageb <- readOGR(dsn="obesidad/nl_ageb_urb.shp")
municipal <- readOGR(dsn="obesidad/nl_municipal.shp")
poligono <- readOGR(dsn="obesidad/poligono_region_mun.shp")
poligonoRegion <- readOGR(dsn="obesidad/poligono_region.shp")
###########################
# Grid, construccion de grid fino
polys <- poligono@polygons[[1]]@Polygons
coords.proj <- coordinates(polys[[1]])
p.map <- list(Polygons(list(Polygon(coords.proj)), "coords.polys"))
poly.map <- SpatialPolygons(p.map)
plot(poly.map)
class(poly.map)
plot( poly.map@polygons[[1]]@Polygons[[1]]@coords)
coords.borders <- poly.map@polygons[[1]]@Polygons[[1]]@coords
grid <- pred_grid(coords.borders,by=.0005)
points(grid, pch="+")
grid.fino <- locations.inside(grid, coords.borders)
plot(grid.fino)
grid <- SpatialPoints(grid.fino, proj4string = casos@proj4string)
plot(grid)
gridded(grid) <- TRUE
class(grid)
plot(grid)
#########################################################################
# EDA y kriging de Peso
#######################
#casos <- readOGR(dsn="obesidad/casos.shp")
casos@data[,c(1:12, 16:31)] <- NULL
names(casos@data)
sapply(casos@data, class)
centro <- apply(casos@coords, 2, mean)
summary(casos@coords)
m1 <- get_map(location = c(-100.45,25.69), zoom=12,
              maptype="toner",
              source="google")
ggmap(m1) + geom_point(data = as.data.frame(casos@coords),
                       aes(x=coords.x1, y=coords.x2,
                           colour=I(color3)))+
    xlim(c(-100.52, -100.37))+ ylim(c(25.64, 25.72))
m1
m2 <- get_map(location = c(-100.45,25.69), zoom=12,
              maptype="terrain",
              source="google")
ggmap(m2) + geom_point(data = as.data.frame(casos@coords),
                       aes(x=coords.x1, y=coords.x2,
                           colour=I(color3)))+
    xlim(c(-100.52, -100.37))+ ylim(c(25.64, 25.72))
m2
############
# Peso
summary(poligono)
plot(poligono, main='Ubicación')
points(casos@coords, col=chido, pch=20,
       cex=1)
p1 <- ggplot(casos@data, aes(x = Peso)) +geom_density(color='purple')+
    theme_minimal() + ggtitle('Distribución de la variable Peso')+
    xlab('Kg')+ ylab('')
coo <- as.data.frame(casos@coords)
p1
names(coo) <- c('x', 'y')
coo <- cbind(coo, casos@data)
names(coo)
p2 <- ggplot(coo, aes(x,Peso)) +geom_point(color=chido)+
    theme_minimal() + ggtitle('Distribución del Peso')+
    xlab('longitud')+ ylab('')+
    stat_smooth(aes(x = x, y = Peso, color=I(morado)),
                se=FALSE)
p2
p3 <- ggplot(coo, aes(y, Peso)) +geom_point(color=chido)+
    theme_minimal() + ggtitle('Distribución del Peso')+
    xlab('latitud')+ ylab('')+
    stat_smooth(aes(x = y, y = Peso, color=I(morado)),
                method='auto', se=FALSE)
p3
ggplot2.multiplot(p1, p2, p3, cols=3)
x <- casos
x@data$Tallacms <- x@data$Circintu <- NULL
x <- as.geodata(x)
a <-points(x, trend="cte",abs.residuals=TRUE,cex.max=4, col=color1)
hist(a$cex)
plot(poligonoRegion, main='Residuales',
     xlim=c(-100.5, -100.3),
     ylim=c(25.61, 25.72))
points(casos@coords, col=chido, cex=a$cex,pch=20 )
hist(a$cex)
a <-points(x, trend="1st", abs.residuals=TRUE,cex.max=4, col=color1)
hist(a$cex)
a <- points(x, trend="2nd", abs.residuals=TRUE,cex.max=4, col=color1)
hist(a$cex) #optamos por la media constante
#varigrama
names(x)
summary(x)
#hscat(Peso ~ 1, casos, (0:10) * 100)
hscat(Peso ~ 1, casos, 0:10*.1)
a <- variogram(Peso ~ 1, casos, cloud=TRUE)
plot(a)
a
v <- variogram(Peso ~ 1, casos)
plot(v)
a <- fit.variogram(v, vgm(10 , "Exp", 4))
a
plot(v,a, main='Semivariograma de Peso', xlab='Distancia')
media <- mean(x$data)
k.peso <- krige(formula = Peso~1, casos, grid, model = a)
s <- spplot(k.peso[,1], main='Peso estimado para los municipios')
s
spplot(k.peso["var1.pred"], main = "Predicciones con Kriging ordinario", xlab='Peso')
spplot(k.peso["var1.var"],  main = "Varianza con kriging ordinario", xlab='Peso')
#########################################################################
# EDA y kriging de Talla
#######################
casos <- readOGR(dsn="obesidad/casos.shp")
class(casos)
casos@coords[dup.coords(casos@coords),] #casos repetidos
casos@coords <- jitterDupCoords(casos@coords, max= 0.01) #suavizamos para no perder dos observaciones de las 81

casos@data[,c(1:12, 16:31)] <- NULL
names(casos@data)
sapply(casos@data, class)
class(casos)
casos@coords[dup.coords(casos@coords),] #casos repetidos
casos@coords <- jitterDupCoords(casos@coords, max= 0.01) #suavizamos para no perder dos observaciones de las 81
summary(poligonoRegion)
plot(poligonoRegion, main='Talla por ubicación',
     xlim=c(-100.5, -100.3),
     ylim=c(25.61, 25.72))
points(casos@coords, col=color1, pch=20,
       cex=((casos@data$Tallacms)-min(casos@data$Tallacms))/max(casos@data$Tallacms)*12)
p1 <- ggplot(casos@data, aes(x = Tallacms)) +geom_density(color='purple')+
    theme_minimal() + ggtitle('Distribución de la variable Talla')+
    xlab('cm')+ ylab('')
coo <- as.data.frame(casos@coords)
p1
names(coo) <- c('x', 'y')
coo <- cbind(coo, casos@data)
names(coo)
p2 <- ggplot(coo, aes(x,Tallacms)) +geom_point(color=chido)+
    theme_minimal() + ggtitle('Distribución de la Talla')+
    xlab('longitud')+ ylab('cm')+
    stat_smooth(aes(x = x, y = Tallacms, color=I(morado)),
                se=FALSE)
p2
p3 <- ggplot(coo, aes(y, Tallacms)) + geom_point(color=chido)+
    theme_minimal() + ggtitle('Distribución de la Talla')+
    xlab('latitud')+ ylab('')+
    stat_smooth(aes(x = y, y = Tallacms, color=I(morado)),
                method='auto', se=FALSE)
p3
ggplot2.multiplot(p1, p2, p3, cols=3)
x <- casos
x@data$Peso <- x@data$Circintu <- NULL
x <- as.geodata(x)
a <- points(x, trend="cte",abs.residuals=TRUE,cex.max=4, col=color1)
hist(a$cex)
plot(poligonoRegion, main='Residuales',
     xlim=c(-100.5, -100.3),
     ylim=c(25.61, 25.72))
points(casos@coords, col=chido, cex=a$cex,pch=20 )
hist(a$cex)
a <-points(x, trend="1st", abs.residuals=TRUE,cex.max=4, col=color1)
hist(a$cex)
a <- points(x, trend="2nd", abs.residuals=TRUE,cex.max=4, col=color1)
hist(a$cex) #optamos por la media constante
#varigrama
hscat(Tallacms ~ 1, casos, 1:7)
a <- variogram(Tallacms ~ 1, casos, cloud=TRUE)
plot(a)
v <- variogram(Tallacms ~ 1, casos)
plot(v)
a <- fit.variogram(v, vgm(10 , "Sph", 2, 40))
a
summary(a)
plot(v,a, main='Semivariograma de Talla', xlab='Distancia')
media <- mean(x$data)
k.peso <- krige(formula = Tallacms~1, casos, grid, model = a)
spplot(k.peso["var1.pred"], main = "Predicciones con Kriging ordinario y nuget", xlab='Talla')
spplot(k.peso["var1.var"],  main = "Varianza con kriging ordinario y nuget", xlab='Talla')
#########################################################################
# EDA y kriging de Circintu
#######################
casos <- readOGR(dsn="obesidad/casos.shp")
class(casos)
casos@coords[dup.coords(casos@coords),] #casos repetidos
casos@coords <- jitterDupCoords(casos@coords, max= 0.01) #suavizamos para no perder dos observaciones de las 81

casos@data[,c(1:12, 16:31)] <- NULL
names(casos@data)
sapply(casos@data, class)
class(casos)
casos@coords[dup.coords(casos@coords),] #casos repetidos
casos@coords <- jitterDupCoords(casos@coords, max= 0.01) #suavizamos para no perder dos observaciones de las 81
summary(poligonoRegion)
plot(poligonoRegion, main='Circunferencia de cintura por ubicación',
     xlim=c(-100.5, -100.3),
     ylim=c(25.61, 25.72))
points(casos@coords, col=color3, pch=20,
       cex=((casos@data$Circintu)-min(casos@data$Circintu))/max(casos@data$Circintu)*8)
p1 <- ggplot(casos@data, aes(x = Circintu)) +geom_density(color='purple')+
    theme_minimal() + ggtitle('Distribución de la variable circunferencia de cintura')+
    xlab('cm')+ ylab('')
coo <- as.data.frame(casos@coords)
p1
names(coo) <- c('x', 'y')
coo <- cbind(coo, casos@data)
names(coo)
p2 <- ggplot(coo, aes(x, Circintu)) +geom_point(color=chido)+
    theme_minimal() + ggtitle('Distribución de la circunferencia de cintura')+
    xlab('longitud')+ ylab('cm')+
    stat_smooth(aes(x = x, y = Circintu, color=I(morado)),
                se=FALSE)
p2
p3 <- ggplot(coo, aes(y, Circintu)) + geom_point(color=chido)+
    theme_minimal() + ggtitle('Distribución de la circunferencia de cintura')+
    xlab('latitud')+ ylab('')+
    stat_smooth(aes(x = y, y = Circintu, color=I(morado)),
                method='auto', se=FALSE)
p3
ggplot2.multiplot(p1, p2, p3, cols=3)
x <- casos
x@data$Peso <- x@data$Tallacms <- NULL
x <- as.geodata(x)
a <- points(x, trend="cte",abs.residuals=TRUE,cex.max=4, col=color1)
hist(a$cex)
plot(poligonoRegion, main='Residuales',
     xlim=c(-100.5, -100.3),
     ylim=c(25.61, 25.72))
points(casos@coords, col=chido, cex=a$cex,pch=20 )
hist(a$cex)
a <-points(x, trend="1st", abs.residuals=TRUE,cex.max=4, col=color1)
hist(a$cex)
a <- points(x, trend="2nd", abs.residuals=TRUE,cex.max=4, col=color1)
hist(a$cex) #optamos por la media constante
#varigrama
hscat(Circintu ~ 1, casos, 1:7)
a <- variogram(Circintu ~ 1, casos, cloud=TRUE)
plot(a)
v <- variogram(Circintu ~ 1, casos)
plot(v)
a <- fit.variogram(v, vgm(15, "Sph", 1))
a
plot(v,a, main='Semivariograma de circunferencia de cintura', xlab='Distancia')
k.peso <- krige(Circintu ~ 1, casos, grid, model=a )
media <- mean(x$data)
spplot(k.peso["var1.pred"], main = "Predicciones con Kriging ordinario ", xlab='Circunferencia cintura')
spplot(k.peso["var1.var"],  main = "Varianza con kriging ordinario ", xlab='Circunferencia cintura')

