#Alumna: Ramirez Islas Cynthia Mariangel
#Tarea 2 

#Ejercicio 4
#Insciso a) Considere una moneda desequilibrada que tiene probabilidad p de obtener águila. Usando
#el comando sample, escriba una función que simule N veces lanzamientos de esta moneda
#hasta obtener un águila.

#Funcion hasta_aguila 
# Entrada: p<- probabilidad  
# Salida: número de lanzamientos hasta obtener un águila
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

#Función: repeticiones_hasta_aguila 
# Entrada: p<- probabilidad, N<-número de repeticiones 
# Salida: Vector con que contenga el numero de lanzamientos hasta obtener
#un águila en cada uno de los N experimentos
repeticiones_hasta_aguila<-function(p, N){
y <- vector()
for(i in 1:N){
  
  y[i] <- hasta_aguila(p)
}
return(y)
}

######################################################################################
#Inciso b) Simule N = 10^4 veces una variable aleatoria Geom(p) para p = 0.5, 0.1 y .01
#Grafique las frecuencias normalizadas en color azul. Sobre esta  ultima figura
#empalme en rojo la grafica de la función de masa correspondiente.

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
nunca delaraste el vecor de simulacion6 no pude reproducir la ultima grafica
#¿Qué observa?
#Podemos notar que la media de las simulaciones anteriores son muy cercanas a las medias 
#de la distribución geométrica con probabilidad p, es decir 1/p respectivamente  

Bien Cyntia solo por el error de no guardar el vector simulacion6 te doy .9 del ejercicio 
>> justo es la simulacion que más tarda