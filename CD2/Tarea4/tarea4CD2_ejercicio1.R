library(geoR)
data(parana)
?parana
summary(parana)
class(parana)
plot(parana, bor= borders)
str(parana)
summary(parana$coords)
plot(parana$coords)
parana$borders
par(mfrow=c(1,2))
plot(parana$borders, col='blue', type='l', lwd=1,
     main='Estaciones registradas en Paraná',
     xlab='Este', ylab='Norte')
points(parana$coords, pch=20, col='green4')
####### Distribucion de la senal
plot(density(parana$data), col='#008D43', main='Precipitación promedio',
     xlab='En el estado de Parana Brazil', ylab='')
rug(parana$coords, col='#008D43')
## localizacion y elevacion (1 grafica)
color <- t(col2rgb('lightblue')) / 255
color <- rgb(color, alpha=0.5)
points(parana, cex.min=.5,cex.max=3, col=color, main='Precipitación promedio registrada',
       xlab='Este', ylab='Norte')
## precipitacion y coordenadas
plot(parana$coords[,2], parana$data, xlab="Coordena Norte",main="Precipitación promedio ",
     pch=20, cex=2, col=color, ylab='')
lines(lowess(parana$data ~ parana$coords[,2]),col="purple",lwd=1.5)
plot(parana$coords[,1], parana$data, xlab="Coordena este", main="Precipitación promedio ",
     pch=20, cex=2, col=color,  ylab='')
lines(lowess(parana$data ~ parana$coords[,1]),col="purple",lwd=1.5)
## ajuste lineal y cuadratico. Residuales
datos <- parana
points(datos, cex.min=.5,cex.max=4, col=color)
color2 <- col2rgb('purple')
color2 <- rgb(t(color2/255), alpha = 0.5)
points(datos, trend="1st",abs.residuals=TRUE,cex.max=4, col=color2)
points(datos, trend="2nd", abs.residuals=TRUE,cex.max=4, col=color2)

## variograma empirico
var1 <- variog(datos, option="cloud")
var2 <- variog( datos, uvec=seq(0,20,by=0.5))
var3 <- variog(datos, trend="1st",uvec=seq(0,20,by=0.5))
var4 <- variog( datos ,trend="2nd",uvec=seq(0,20,by=0.5))
## cloud
?variog
plot(var1)
## mean bins
plot(var2,type="b",ylab="mean semivariogram")
## residuals linear-quadratic
plot(var3,type="b",ylab="mean semivariogram")
lines(var4,type="b",lty=2)
#######################################

