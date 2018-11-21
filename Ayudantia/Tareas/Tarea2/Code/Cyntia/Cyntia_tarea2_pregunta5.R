#Alumna: Ramirez Islas Cynthia Mariangel
#Tarea 2 

#Ejercicio 5
#Escriba una función en R que simule N veces los lanzamientos
#de una moneda hasta obtener r águilas.

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


#Grafique las frecuencias normalizadas de los experimentos para 
#N = 10^6, p = 0.2 y r = 2 
siempre que simules fija una semilla
set.seed(0)
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
lines(dnbinom(-7:1000000,7,.2), col="red")
checa los indices de 'lines' las graficas no se translapan en el codigo que me enviaste

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
checa los indices de esta ultima linea
plot(proba4, main="Dist. Binomial Neg p=0.1, r=7", xlab="# de lanzamientos ", ylab="Proporciones", col="blue")
lines(dnbinom(-7:1000000,7,.1), col="red")

#La simulado se asemeja mucho a la gráfica de la función de masa 
#de la distribución binomial negativa, sin embargo la reparametrizacion de R 
  #de la distribucion binomial negativa es un poco distinta por lo que la gráfica
  #se muestra un poco desfasada
Bien general un poco lento y las graficas no se enpalman de doy .9 de este ejercicio 


