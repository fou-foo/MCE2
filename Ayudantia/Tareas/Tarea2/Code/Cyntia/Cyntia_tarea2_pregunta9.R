#Alumna: Ramirez Islas Cynthia Mariangel
#Tarea 2 

#Ejercicio 9 Escriba una funcion en R que simule una aproximacion al proceso Poisson a partir de las 5
#hipotesis que usamos en clase para construir tal proceso. Usando esta funcion, simule tres
#trayectorias de un proceso Poisson lamda = 2 sobre el intervalo [0, 10] y grafiquelas. Ademas
#simule 10000 veces un proceso de Poisson N con lamda = 0.5 y hasta el tiempo t = 1. Haga un
#histograma de N(1) en su simulacion anterior y compare contra la distribucion de Poisson
#correspondiente.

#Simularemos N(t) con el siguiente algoritmo.
#1. Simularemos k=N(t)~Poisson(??t)
#2. Simularemos k variables aleatorias U(0,t)
#3. Ordenaremos las k simulaciones uniformes como S1,...,S


#Funcion PPois
#Entrada:t y lambda 
PPois<- function(t,lambda){
  #Paso 1
  N<- rpois(1,t*lambda) 
  C<- sort(runif(N,0,t)) 
  #Paso 2 y 3 
  data.frame(x=c(0,0,C),y=c(0,0:N)) 
}

fija una semilla
set.seed(0)
library(ggplot2)
#Gráfica de trayectoria
P<-PPois(10,2)
qplot(x,y,data=P,xlab="Tiempo",ylab="N(t)",main="Proceso Poisson",geom=c("step","point"),xlim = c(0,10))
#Gráfica de trayectoria
L<-PPois(10,2)
qplot(x,y,data=L,xlab="Tiempo",ylab="N(t)",main="Proceso Poisson",geom=c("step","point"),xlim = c(0,10))
#Gráfica de trayectoria
K<-PPois(10,2)
qplot(x,y,data=K,xlab="Tiempo",ylab="N(t)",main="Proceso Poisson",geom=c("step","point"),xlim = c(0,10))

#Simulación de N procesos de Poisson con la siguiente función
library(plyr)
NPois<-function(n,t,rate){
  C<- lapply(1:n, function(n) data.frame(ProcesoPois(t,rate),simulacion=n)) #Genera N dataframes con los procesos
  C<-ldply(C, data.frame) #Acomoda en un solo dataframe
  C$simulacion<-factor(C$simulacion) #Convierte en factores
  qplot(x,y,data=C,geom=c("step","point"),color=simulacion,xlab="Tiempo",ylab="N(t)",main=sprintf("%d Simulaciones del ProcesoPoisson, intensidad %.2f",n,rate))
}

NPois(100,1,0.5) 

curve(dpois(x,0.5),xlim=c(0,2),col="blue",lwd=2,add=TRUE)
curve(ppois(x,0.5),xlim=c(0,2),col="red",lwd=2,lty=2,add=TRUE)
################################ 
En la construccion de la funcion 'Npois' creo que te equivocaste
NPois<-function(n,t,rate){
  #en la linea de abajo 
  C<- lapply(1:n, function(n) data.frame(PPois(t,rate),simulacion=n)) #Genera N dataframes con los procesos 
  C<-ldply(C, data.frame) #Acomoda en un solo dataframe
  C$simulacion<-factor(C$simulacion) #Convierte en factores
  qplot(x,y,data=C,geom=c("step","point"),color=simulacion,xlab="Tiempo",ylab="N(t)",main=sprintf("%d Simulaciones del ProcesoPoisson, intensidad %.2f",n,rate))
}

NPois(100,1,0.5) 

curve(dpois(x,0.5),xlim=c(0,2),col="blue",lwd=2,add=TRUE)
curve(ppois(x,0.5),xlim=c(0,2),col="red",lwd=2,lty=2,add=TRUE)
### Me parece que no entendiste la aproxximación Cyntia
#lo vemos en una ayudantia
