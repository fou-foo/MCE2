#5a) Escriba un programa que reproduzca las gráficas de las funciones de distribucion acumulada
#y de masa de una distribución uniforme

x<-0:10
a<- dunif(x,min = 0, max = 10)
plot(x,a,type = "b", main = "PMF Uniforme Discreta", xlab = "Espacio Muestral",ylab = "Probabilidad")
b<-punif(x,min = 0, max = 10)
plot(b, type = "b", main = "CDF Uniforme Discreta", xlab = "Espacio Muestral", ylab = "Probabilidad Acumulada")
# te faltaron dos graficas ademas la segunda grafica es la de una v.a. continua y uniforme, este detalle lo hablamos en la segunda ayudantia


#5c) Simular una muestra de tamaño 10 000 de la distribucion U(1,...,10). Fijando la semilla en 13
#y calcular la media y la varianza

set.seed(13)
muestra<-sample(seq(1,10,by=1),10000, replace = T)
table(muestra)
#El valor de la media es
mean(muestra)
#El valor de la varianza es
var(muestra)

#5d) Graficar las frecuencias de la simulacion anterior

hist(muestra, main = "Histograma de Frecuencias", xlab = "Espacio Muestral", ylab = "Frecuencia", col = "gray")
Parece que la funcion de densidad no esta definida en el codigo 
checa esta linea 
barplot(table(muestra))
#te doy 3.3/4 para este ejercicio

#6a) Simular 10 lanzaminetos de una moneda equilibrada y contar el numero de aguilas que obtiene.

y<-sample(x=c("A","S"), size = 10, replace = T, prob = c(0.5,0.5))
y 
c<-sum(y=="A")  #numeros de aguilas obtenidas
c 
#Repetir el proceso un millon de veces
sim<-replicate(10e5,sample(x=c(1,0), size = 10, replace = T, prob = c(0.5,0.5)))
sumA<-apply(X = sim,MARGIN = 2,FUN =  sum)
#Los primeros tres resultados son:
sumA[1:3]
#Graficar el las frecuencias del numero de aguilas obtenidas en el millon de experimentos.
Tab<-table(sumA)            
Tab
plot(Tab)
#Graficar las proporciones del numero de aguilas obtenidas
plot(Tab/10e5,col="red")

#6b)Graficar la funcion de masa de una distribucion B(10,0.5) sobre la grafica de las proporciones del inciso anterior.
denbin<-dbinom(x = 1:10,size = 10,prob = 0.5)
lines(denbin,col="blue",type = "b",lwd=1)

#6c)Repetir los dos incisos anteriores para una moneda desequilibrada que tiene 
#probabilidad p=0.3 de obtener un aguila cuando se lanza.

w<-sample(x=c("A","S"), size = 10, replace = T, prob = c(0.3,0.7))
y 
d<-sum(y=="A")
d
#Repetir el proceso un millon de veces
sim<-replicate(10e5,sample(x=c(1,0), size = 10, replace = T, prob = c(0.3,0.7)))
sumA2<-apply(X = sim,MARGIN = 2,FUN =  sum)
#Mostrar los primeros 3 resultados:
sumA2[1:3]
#Graficar las frecuencias
Tab2<-table(sumA2)            
Tab2
plot(Tab2)
#Graficar las proporciones
plot(Tab2/10e5,col="red")
#Graficar la funcion de masa sobre una distribucion B(10,0.3) sobre la grafica de las proporciones.
denbin<-dbinom(x = 1:10,size = 10,prob = 0.3)
lines(denbin,col="blue",type = "b",lwd=1)

#¿Que observa? R: que no importa el valor de p, el experimento tiende a una distribucion binomial
# siempre que se simule un numero grande de veces

#7Suponga que X tiene una distribucion B(123,0.31)
#a)Calcular las probabilidades directamente de la funcion de masa.
#Para calcular las combinaciones se debe instalar la libreria gtools
library(gtools)
probin<-function(n=123,p=0.31,x){
  pro<-(factorial(n)/(factorial(n-x)*factorial(x)))*p^x*(1-p)^(n-x)
  return(pro)
}

#I)
x_0<-probin(x = 0) #P(x=0)
x_0
x_123<-probin(x = 123) #P(x=123)
x_123
x_62<-probin(x = 62) #P(x=62)
x_62
#II)
x_0_10<-0   #P(0<=x<=10)
for (i in 0:10) {
  x_0_10<-probin(x = i)+x_0_10
}
x_0_10

x_1_10<-x_0_10-x_0  #P(0<x<=10)
x_1_10

x_0_9<-x_0_10-probin(x = 10)  #P(0<=x<10)
x_0_9

#III)
x_12_mas<-1-x_0_10-probin(x = 11)  #P(x>11)
x_12_mas

x_11_menos<-x_0_10   #P(x<=10)

#7b) Calcule las probabilidades anteriores usando las funciones pbinom y dbinom.

X_0<-dbinom(0,123,0.31)  #P(x=0)
X_0
X_123<-dbinom(123,123,0.31) #P(x=123)
X_123
X_62<-dbinom(62,123,0.31)  #P(x=62)
X_62

X_0_10<-pbinom(10,prob = 0.31,size = 123)  #P(0<=x<=10) = P(x<=10)
X_0_10

X_1_10<-X_0_10-X_0  #P(0<x<=10)
X_1_10

X_0_9<-pbinom(9,prob = 0.31, size = 123)  #P(0<=x<10)

X_12_mas<-1-pbinom(11,prob = 0.31, size = 123)  #P(x>11)

#7c) Escriba un programa que calcule los cuantiles de 0.25, 0.5 y 0.75

cuarbin<-function(n=123,p=0.31,PAcu){
  q<-0
  for(i in 1:n){
    if(pbinom(q = i-1,size = n,prob = p)<PAcu && pbinom(q = i,size = n,prob = p)>=PAcu ){
      q<-i
    }
  }
  return(q)
}

cuarbin(PAcu = 0.5)
qbinom(0.5,123,0.31) #Comprobacion

cuarbin(PAcu = 0.25)
qbinom(0.25,123,0.31) #Comprobacion

cuarbin(PAcu = 0.75)
qbinom(0.75,123,0.31) #Comprobacion
# bien

#8 Una urna contiene 46 bolas grises y 49 bolas blancas. Simule una extraccion sin remplazamiento 
#de 20 de estas bolas  cuente el numero de bolas grises que obtuvo.

bolas<-c(rep("G",46),rep("B",49))
ball<-sample(x=bolas, size = 20, replace = F)
ball
nG<-sum(ball=="G")
#El numero de bolas grises obtenido es:
nG
#Repita el proceso un millon de veces y muestre los primeros 3 resultados.
sball<-replicate(10e5,sample(x=c(rep(1,46),rep(0,49)), size = 20, replace = F))
sumball<-apply(X = sball,MARGIN = 2,FUN =  sum)
#Los primeros 3 resultados son:
sumball[1:3]
#Grafique las frecuencias de las bolas grises obtenidas en cada experimento.
Taball<-table(sumball)            
Taball
plot(Taball)

#¿Cual es la probabilidad de que al extraer 20 bolas de la urna 5 de ellas sean grises?

pro5<-Taball[5]/10e5
#La probabilidad es:
pro5
#Grafique la proporcion de bolas grises obtenidas y sobre la figura añada la correspondiente funcion de masa
#de la distribucion Hipergeometrica asociada al experimento.
Taball<-c(0,Taball,0,0)
plot(Taball/10e5,type = "h", col= "red")
dhiper<-dhyper(0:20,m = 46,n = 49,k = 20)
lines(dhiper,type = "p", col="blue")
#bien 
