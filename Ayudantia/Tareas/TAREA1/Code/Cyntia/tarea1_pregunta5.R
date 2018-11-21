#Alumna: Ramirez Islas Cynthia Mariangel
#Tarea 1
#Ejercicio 5

###############################################################################
#Inciso a) Reproducir gráficas de  funciones de masa y de distribucion acumulada 
#Salida: Gráficas de distribución y probabilidad n=5,10,50

#Distribuciòn uniforme n=5
n=5
x<- 1:n
y<- rep(.10, n)
#Grafica de la función de distribucion  
plot(x,y,type="b", xlab="Espacio muestral",ylab="Probabilidad",main="PMF Uiforme discreta n=5", ylim = c(0.0, 1.0))
x<- 1:n+1
z<- .2*x
#Grafica de la función de distribucion acumulada
plot(x,z,type="s",xlab="Espacio muestral",ylab="Probabilidad Acumulada",main="CDF Uniforme discreta n=5", ylim = c(0.0, 1.0))

#Distribuciòn uniforme n=10
n=10
x<- 1:n
y<- rep(.10, n)
#Grafica de la función de distribucion  
plot(x,y,type="b", xlab="Espacio muestral",ylab="Probabilidad",main="PMF Uiforme discreta n=10", ylim = c(0.0, 1.0))
x<- 1:n+1
z<- .1*x
#Grafica de la función de distribucion acumulada
plot(x,z,type="s",xlab="Espacio muestral",ylab="Probabilidad Acumulada",main="CDF Uniforme discreta n=10", ylim = c(0.0, 1.0))

#Distribuciòn uniforme n=50
n=50
x<- 1:n
y<- rep(.02, n)
#Grafica de la función de distribucion  
plot(x,y,type="b", xlab="Espacio muestral",ylab="Probabilidad",main="PMF Uiforme discreta n=50", ylim = c(0.0, 1.0))
x<- 1:n+1
z<- .02*x
#Grafica de la función de distribucion acumulada
plot(x,z,type="s",xlab="Espacio muestral",ylab="Probabilidad Acumulada",main="CDF Uniforme discreta n=50", ylim = c(0.0, 1.0))


################################################################################
#Inciso b)Leer documentacion de la función sample

################################################################################
#Inciso C) Simulacion de la distibucion uniforme, 10,000 veces 

#tabkmuestra<-sample(runif(10,0,10), 10, TRUE, NULL)
tabkmuestra<- sample(c(1:10), 10, TRUE, rep(1/10,10))
#Fijando semilla 
(set.seed(13))
#Redondeo de la muestra
muestra<- round(tabkmuestra,digits = 0)
#Tabla de frecuencias absolutas
tabla1<- table(muestra)
#Tabla de frecuencias relativas
tabla2<- prop.table(table(muestra))
tabla2<- tabla2*100
marco1<- data.frame(tabla1)
marco2<- data.frame(tabla2)
#Dando formato a las tablas
names(marco1)<- c("Valor","Frecuencia absoluta" )
names(marco2)<- c("Valor","Frecuencia relativa" )

#Tabla de Frecuencias absoluta y relativa 
tablaf<- merge.data.frame(marco1,marco2, by.x = "Valor")
tablaf
#Cálculo de promedio y varianza
promedio<- mean(muestra)
varianza<- var(muestra)
promedio
varianza
#Gráfica de la frecuencia Absoluta
plot(marco1)
#creo que te falto indicar la columna
hist(marco1$`Frecuencia absoluta`)
#Gráfica de la frecuencia Relativa
plot(marco2)
# te falto simular el experimento varias veces 
# te doy la mitad del acierto
