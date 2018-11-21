######################################################################################################################################
#Ana Beatriz Rodríguez Mendoza.
#Inferencia Estadística.
#Parte 2 de la tarea 2.
#TExto sin acentos.
#######################################################################################################################################


###############################################################################################################################
#4a) Considere una moneda desequilibrada que tiene probabilidad p de obtener aaguila. Usando
#el comando sample, escriba una funcion que simule N veces lanzamientos de esta moneda
#hasta obtener un aguila. La funcion debera recibir como parametros a la probabilidad
#p de obtener aguila y al numero N de veces que se repite el experimento; y tendra que
#regresar un vector de longitud N que contenga el numero de lanzamientos hasta obtener
#un aguila en cada uno de los N experimentos.

# Función que tira una moneda hasque que sale aguila
hasta_aguila<-function(p){
  i <- 0
  continue <- TRUE
  
  while(continue)
  {
    xi <- sample(c(1:0),1, TRUE, c(p,(1-p)))
    
    
    if(xi == 1)
    {
      continue <- FALSE
    }
    i <-i+1
  }
  return(i)
}

#Función que da el numero de lanzamientos hasta obtener un aguila en cada uno de los N experimentos.

repeticiones_hasta_aguila<-function(p, N){
  y <- vector()
  for(i in 1:N){
    
    y[i] <- hasta_aguila(p)
  }
  return(y)
}

######################################################################################
#Inciso b) Usando la funcion anterior simule N = 10^4 veces una variable aleatoria Geom(p) para p = 0.5, 0.1 y .01
#Grafique las frecuencias normalizadas en color azul. Sobre esta  ultima figura
#empalme en rojo la grafica de la función de masa correspondiente. Que observa?


#Simulación y gráfica de N = 10^4 veces una variable aleatoria Geom(p) para p = 0.5
simulacion1<-repeticiones_hasta_aguila(.5, 10000)
frecuencia<- data.frame(table(simulacion1))
print (frecuencia)
#Gráfica de frecuencias 
plot(frecuencia, main="Gráfica de frecuencias p=.5", xlab="# de lanz")
probabilidad<- prop.table(table(simulacion1))
#Gráfica de frecuencias normalizadas
plot(probabilidad, main="Dist. Geométrica p=0.5", xlab="# de lanzamientos ", ylab="Proporciones", col="blue")
lines(dgeom(0:10000,.5), col="red")

#Simulación y gráfica de N = 10^4 veces una variable aleatoria Geom(p) para p = 0.1
simulacion2<-repeticiones_hasta_aguila(.1, 10000)
frecuencia2<- data.frame(table(simulacion2))
print (frecuencia2)
plot(frecuencia2, main="Gráfica de frecuencias p=0.1", xlab="# de lanz")
probabilidad2<- prop.table(table(simulacion2))
#Gráfica de frecuencias normalizadas
plot(probabilidad2, main="Dist. Geométrica p=0.1 ", xlab="# de lanzamientos ", ylab="Proporciones", col="blue")
lines(dgeom(0:10000,.1), col="red")

#Simulación y gráfica de N = 10^4 veces una variable aleatoria Geom(p) para p = 0.01
simulacion3<-repeticiones_hasta_aguila(.01, 10000)
frecuencia3<- data.frame(table(simulacion3))
print(frecuencia3)
plot(frecuencia3, main="Gráfica de frecuencias p=0.01", xlab="# de lanz")
probabilidad3<- prop.table(table(simulacion3))
#Gráfica de frecuencias normalizadas
plot(probabilidad3, main="Dist. Geométrica p=0.01 ", xlab="# de lanzamientos ", ylab="Proporciones", col="blue")
lines(dgeom(0:10000,.01), col="red")

#¿Qué observa?
#Podemos notar que la simulación de 10^4 veces se asemeja mucho a la gráfica de la función de masa 
#de la distribución geométrica con la probabilidad correspondiente en cada caso

###########################################################################
#Inciso c) Repita el inciso anterior para N = 10^6. Además calcule el promedio 
#y la desviación estándar de las simulaciones que realizó

#Simulación y gráfica de N = 10^6 veces una variable aleatoria Geom(p) para p = 0.5
simulacion4<-repeticiones_hasta_aguila(.5, 1000000)
frecuencia4<- data.frame(table(simulacion4))
print (frecuencia4)
#Gráfica de frecuencias 
plot(frecuencia, main="Gráfica de frecuencias p=.5", xlab="# de lanz")
probabilidad4<- prop.table(table(simulacion4))
#Gráfica de frecuencias normalizadas
plot(probabilidad, main="Dist. Geométrica p=0.5", xlab="# de lanzamientos ", ylab="Proporciones", col="blue")
lines(dgeom(0:1000000,.5), col="red")
#Cálculo de promedio y desviación estándar 
media1<- mean(simulacion4)
print (media1)
desvest1<-sd(simulacion4)
print(desvest1)

#Simulación y gráfica de N = 10^6 veces una variable aleatoria Geom(p) para p = 0.1
simulacion5<- repeticiones_hasta_aguila(.1, 1000000)
frecuencia5<- data.frame(table(simulacion5))
print (frecuencia5)
plot(frecuencia2, main="Gráfica de frecuencias p=0.1", xlab="# de lanz")
probabilidad2<- prop.table(table(simulacion5))
#Gráfica de frecuencias normalizadas
plot(probabilidad2, main="Dist. Geométrica p=0.1 ", xlab="# de lanzamientos ", ylab="Proporciones", col="blue")
lines(dgeom(0:1000000,.1), col="red")
#Cálculo de promedio y desviación estándar 
media2<- mean(simulacion5)
print (media2)
desvest2<-sd(simulacion5)
print(desvest2)

#Simulación y gráfica de N = 10^4 veces una variable aleatoria Geom(p) para p = 0.01
frecuencia6<- data.frame(table(repeticiones_hasta_aguila(.01, 1000000)))
print(frecuencia3)
plot(frecuencia3, main="Gráfica de frecuencias p=0.01", xlab="# de lanz")
probabilidad3<- prop.table(table(simulacion6))
#Gráfica de frecuencias normalizadas
plot(probabilidad3, main="Dist. Geométrica p=0.01 ", xlab="# de lanzamientos ", ylab="Proporciones", col="blue")
lines(dgeom(0:1000000,.01), col="red")
#Cálculo de promedio y desviación estándar 
media3<- mean(simulacion6)
print (media3)
desvest3<-sd(simulacion6)
print(desvest3)

#¿Qué observa?
#Podemos notar que la media de las simulaciones anteriores son muy cercanas a las medias 
#de la distribución geométrica con probabilidad p, las cuales son muy cercanas a 1/p respectivamente  

###########################################################################################################################################
#5. Usando las ideas del inciso anterior escriba una funcion en R que simule N veces los lanzamientos
#de moneda hasta obtener r aguilas. La funcion debera recibir como parametros a
#la probabilidad p de obtener aguila, al numero r de aguilas a observar antes de detener el
#experimento y al numero N de veces que se repite el experimento; y tendra que regresar un
#vector de longitud N que contenga el numero de lanzamientos hasta obtener las r aguilas en
#cada uno de los N experimentos. 

#Funcion hasta_aguila 
# Entrada: p<- probabilidad  
# Salida: número de lanzamientos hasta obtener 1 águilas
hasta_raguilas<-function(p,r){
  i <- 0
  continue <- TRUE
  lanz<-1
  
  while(continue)
  {
    xi <- sample(c(1:0),1, TRUE, c(p,(1-p)))
    
    
    if(xi == 1)
    {
      i<-i+1
    }
    if(i == r)
    {
      continue <- FALSE
    }
    
    lanz <-lanz+1
  }
  return(lanz)
}


#Función: repeticiones_hasta_raguilas 
# Entrada: p<- probabilidad, r<-número de aguilas a observar, N<-número de repeticiones 
# Salida: Vector con que contiene el número de lanzamientos hasta obtener
# r águilas en cada uno de los N experimentos
repeticiones_hasta_raguilas<-function(p,r, N){
  z <- vector()
  for(i in 1:N){
    
    z[i] <- hasta_raguilas(p,r)
  }
  return(z)
}


#Grafique las frecuencias normalizadas de los experimentos
#para N = 10^6, p = 0.2 y r = 2 y comparelos contra la funcion de masa de la distribucion
#mas adecuada para modelar este tipo de experimentos.
sim1<-repeticiones_hasta_raguilas(0.2, 2,1000000)
#Gráfica de frecuencias
frec1<- data.frame(table(sim1))
print (frec1)
proba1<-prop.table(table(sim1))
#Gráfica de frecuencias normalizadas
plot(proba1, main="Dist. Binomial Neg p=0.2, r=2", xlab="# de lanzamientos ", ylab="Proporciones", col="blue")
lines(dnbinom(0:1000000,2,.2), col="red")

#Grafique las frecuencias normalizadas de los experimentos para 
#N = 10^6, p = 0.2 y r = 7
sim2<-repeticiones_hasta_raguilas(0.2, 7,1000000)
#Gráfica de frecuencias
frec2<- data.frame(table(sim2))
print (frec2)
proba2<-prop.table(table(sim2))
#Gráfica de frecuencias normalizadas
plot(proba2, main="Dist. Binomial Neg p=0.2, r=7", xlab="# de lanzamientos ", ylab="Proporciones", col="blue")
lines(dnbinom(0:1000000,7,.2), col="red")


#Grafique las frecuencias normalizadas de los experimentos para 
#N = 10^6, p = 0.1 y r = 2 
sim3<-repeticiones_hasta_raguilas(0.1, 2,1000000)
#Gráfica de frecuencias
frec3<- data.frame(table(sim3))
print (frec3)
proba3<-prop.table(table(sim3))
#Gráfica de frecuencias normalizadas
plot(proba3, main="Dist. Binomial Neg p=0.1, r=2", xlab="# de lanzamientos ", ylab="Proporciones", col="blue")
lines(dnbinom(0:1000000,2,.1), col="red")

#Grafique las frecuencias normalizadas de los experimentos para 
#N = 10^6, p = 0.1 y r = 7
sim4<-repeticiones_hasta_raguilas(0.1, 7,1000000)
#Gráfica de frecuencias
frec4<- data.frame(table(sim4))
print (frec4)
proba4<-prop.table(table(sim4))
#Gráfica de frecuencias normalizadas
plot(proba4, main="Dist. Binomial Neg p=0.1, r=7", xlab="# de lanzamientos ", ylab="Proporciones", col="blue")
lines(dnbinom(0:1000000,7,.1), col="red")


#La simulado se asemeja mucho a la gráfica de la función de masa 
#de la distribución binomial negativa  





##########################################################################################################################################
#9. Escriba una funcion en R que simule una aproximacion al proceso Poisson a partir de las 5
#hipotesis que usamos en clase para construir tal proceso. Usando esta funcion, simule tres
#trayectorias de un proceso Poisson lamda = 2 sobre el intervalo [0, 10] y grafiquelas. Ademas
#simule 10000 veces un proceso de Poisson N con lamda = 0.5 y hasta el tiempo t = 1. Haga un
#histograma de N(1) en su simulacion anterior y compare contra la distribucion de Poisson
#correspondiente.

#Simular N(t) con el siguiente algoritmo.
#1. Simular k=N(t)~Poisson(??t)
#2. Simular k variables aleatorias U(0,t)
#3. Ordenar las k simulaciones uniformes como S1,...,S


ProcesoPois<- function(t,lambda){
  N<- rpois(1,t*lambda) #Paso 1
  C<- sort(runif(N,0,t)) #Paso 2 y 3 
  data.frame(x=c(0,0,C),y=c(0,0:N)) 
}
#La función anterior nos devuelve una dataframe con las cordenadas lista para graficar el proceso, 
#veamos un ejemplo de un proceso con tres trayectorias diferentes de intesidad 2 al tiempo 10.

library(ggplot2)
P1<-ProcesoPois(10,2)
qplot(x,y,data=P1,xlab="Tiempo",ylab="N(t)",main="Proceso de Poisson",geom=c("step","point"),xlim = c(0,10))

P2<-ProcesoPois(10,2)
qplot(x,y,data=P2,xlab="Tiempo",ylab="N(t)",main="Proceso de Poisson",geom=c("step","point"),xlim = c(0,10))

P3<-ProcesoPois(10,2)
qplot(x,y,data=P3,xlab="Tiempo",ylab="N(t)",main="Proceso de Poisson",geom=c("step","point"),xlim = c(0,10))

#Simulaciones de N procesos de Poisson con la siguiente función
library(plyr)
NPois<-function(n,t,rate){
  C<- lapply(1:n, function(n) data.frame(ProcesoPois(t,rate),simulacion=n)) #Genera N dataframes con los procesos
  C<-ldply(C, data.frame) #Une en una sola dataframe
  C$simulacion<-factor(C$simulacion) #Convierte en factores
  qplot(x,y,data=C,geom=c("step","point"),color=simulacion,xlab="Tiempo",ylab="N(t)",main=sprintf("%d Simulaciones del Proceso de Poisson de Intensidad %.2f",n,rate))
}

#Simulacion de 10000 procesos de Poisson con densidad  0.5 al tiempo 1:
NPois(10000,1,0.5) 

#curve(dpois(x,0.5),xlim=c(0,2),col="blue",lwd=2,add=TRUE)
#curve(ppois(x,0.5),xlim=c(0,2),col="red",lwd=2,lty=2,add=TRUE)
############################################################################################################################################################################
