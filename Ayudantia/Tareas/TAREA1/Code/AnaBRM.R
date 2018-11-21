######################################################################################################################################
#Ana Beatriz Rodríguez Mendoza
#Inferencia Estadística
#Parte 2 de la tarea 1
#######################################################################################################################################




#######################################################################################################################################
#5.	Para el siguiente ejercicio es necesario usar el programa R.
#a)Escriba un programa en R que reproduzca las gráficas de las funciones de distribución acumulada y 
# de masa de la distribución uniforme que aparecen en las notas del curso.
#######################################################################################################################################

#Funciones para n=5.
x5 = dunif(x=1:5, min = 0, max = 5, log = FALSE) #Función distribución.
y5 = punif(q=1:5, min = 0, max = 5, lower.tail = TRUE, log.p = FALSE) #Función acumulada.
plot(x5,type="b",ylim = c(0,1),main="PMF Uniforme Discreta para n = 5.",ylab = "Probabilidad",xlab = "Espacio Muestral",col="purple")
plot(y5,type="s",ylim = c(0,1),main="CDF Uniforme Discreta para n = 5.",ylab = "Probabilidad",xlab = "Espacio Muestral",col="purple")

#Funciones para n=10.
x10 = dunif(x=1:10, min = 0, max = 10, log = FALSE)
y10 = punif(q=1:10, min = 0, max = 10, lower.tail = TRUE, log.p = FALSE)
plot(x10,type="b",ylim = c(0,1),main="PMF Uniforme Discreta  n = 10.",ylab = "Probabilidad",xlab = "Espacio Muestral",col="blue")
plot(y10,type="s",ylim = c(0,1),main="CDF Uniforme Discreta  n = 10.",ylab = "Probabilidad",xlab = "Espacio Muestral",col="blue")

#Funciones para n=50.
x50 = dunif(x=1:50, min = 0, max = 50, log = FALSE) 
y50 = punif(q=1:50, min = 0, max = 50, lower.tail = TRUE, log.p = FALSE)
plot(x50,type="b",ylim = c(0,1),main="PMF Uniforme Discreta  n = 50.",ylab = "Probabilidad",xlab = "Espacio Muestral",col="pink")
plot(y50,type="s",ylim = c(0,1),main="CDF Uniforme Discreta  n = 50.",ylab = "Probabilidad",xlab = "Espacio Muestral",col="pink")


#######################################################################################################################################
#5.c)	Usando la función sample simule una muestra de tamaño 10 000 de la distribución U(1, …, 10). Fijando 
#la semilla en 13 (set.seed(13)), muestre los resultados de la simulación en una tabla de frecuencia y 
#calcule la media y la varianza. Sugerencia: Use la función table.
#######################################################################################################################################

set.seed(13) #Semilla
x=sample(1:10,size = 10000, replace = TRUE, prob=rep(0.1, times = 10))
y=table(x) # Genera la tabla de frecuencias.
median.default(x) #Media
var(x)  #Varianza

########################################################################################################################################
#	5.d) Grafique las frecuencias de la simulación anterior.
###########################################################################################################################################

hist(x, breaks = 15, freq = FALSE,xlim = c(0,10),ylim = c(0,.6),col="blueviolet",
     density=30, main="FRECUENCIAS", xlab=" variables aleatorias ")
parece que la funcion no esta definida en todo el soporte checa la siguiente linea
barplot(table(x))
#te doy 3.5/4 del ejercicio
#########################################################################################################################################
#6.	Para el siguiente ejercicio también es necesario R.
#6.a)	Usando la función sample, simule 10 lanzamientos de una moneda equilibrada y cuente el número de águilas que obtiene.
# Repita este proceso 10^{6} veces y muestre sus primeros 3 resultados. Grafique las frecuencias del número de águilas 
# obtenidas en 10^{6} experimentos. También grafique las proporciones (probabilidades) del número de águilas obtenidas.
##################################################################################################################################

#Para 10 lanzamientos
l=sample(c(0,1),size = 10, replace = TRUE, prob=c(0.5,0.5))
t=table(l)
names(t)<-c("Sol","Áquila")
t
#Repita 1 000 000 el proceso anterior
numaguilas<-0
for (i in 1:1000000) {
  lanzamientos=sample(c(0,1),size = 10, replace = TRUE, prob=c(0.5,0.5))
  numaguilas[i]<-c(sum(lanzamientos))
  if(i<=3){
    print(paste("El numero de aguilas y soles, respectivamente en el primer lanzamiento son: ",i))
    print(numaguilas[i])
    print(10 - numaguilas[i])
  }
}
f=data.frame(table(numaguilas))
lines(f, main="FRECUENCIAS", xlab="Número de veces que cayo águila.",
      ylab="Frecuencia", type="l", col="blue")
proba=prop.table(table(numaguilas))
plot(proba, main="PROBABILIDADES", xlab="Número de veces que cayo águila.", ylab="Probabilidad", type="l", col="pink",ylim=c(0,1))


#############################################################################################################################################
#6.b Usando la función dbinom grafique la función de masa de la distribución B(10, 0.5)
#sobre la gráfica de las proporciones que hizo en el inciso anterior. ¿Qué observa?
#############################################################################################################################################

binomial=dbinom(x=1:10,size = 10,prob=0.5) 
plot(binomial, main="Binomial(10,0.5)", ylab="función de masa",
     xlab="variables aleatorias", type="l", col="blue",ylim=c(0,1))
lines(proba, main="PROBABILIDADES", col="purple",ylim=c(0,1))

#Se observa que la simulación es muy buena y se aproxima mucho al valor de la teoría,
# a simple vista parecen iguales.

############################################################################################################################################
#6.c	Repita los dos incisos anteriores para una moneda desequilibrada que tiene la probabilidad 
# p = 0.3 de obtener un águila cuando se lanza. ¿Qué observa?
#############################################################################################################################################

l=sample(c(0,1),size = 10, replace = TRUE, prob=c(0.7,0.3))
t=table(l)
names(t)<-c("Sol","Áquila")
t
#Repita 1 000 000 el proceso anterior
numaguilas<-0
for (i in 1:1000000) {
  lanzamientos=sample(c(0,1),size = 10, replace = TRUE, prob=c(0.7,0.3))
  numaguilas[i]<-c(sum(lanzamientos)) #Va guardado la suma dependiendo su indice.
  if(i<=3){
    print(paste("El numero de aguilas y soles, respectivamente en el primer lanzamiento son: ",i))
    print(numaguilas[i])
    print(10 - numaguilas[i])
  }
}

f=data.frame(table(numaguilas))
plot(f, main="FRECUENCIAS", xlab="Número de veces que cayo águila.",
      ylab="Frecuencia", type="l", col="blue")
proba=prop.table(table(numaguilas))
plot(proba, main="PROBABILIDADES", xlab="Número de veces que cayo águila.", ylab="Probabilidad", type="l", col="pink",ylim=c(0,1))

#6.b.2 Usando la función dbinom grafique la función de masa de la distribución B(10, 0.3)
#sobre la gráfica de las proporciones que hizo en el inciso anterior. ¿Qué observa?

binomial=dbinom(x=1:10,size = 10,prob=0.3)
plot(binomial, main="Binomial(10,0.3)", ylab="función de masa",
     xlab="variables aleatorias", type="l", col="blue",ylim=c(0,1))
lines(proba, main="PROBABILIDADES", col="purple",ylim=c(0,1))
#10.	Se observa que otra vez la simulación quedo igual que la teoría, solo que en este caso 
#ya no es simétrica, toma un sesgo a la derecha., es decir, se acumulan las variables aleatorias a la izquierda. 


##########################################################################################################################################
#7.	Suponga que  X \sim B (123, 0.31). Resuelva lo siguiente:
#7.a)	Escriba un programa en R que calcule las siguientes probabilidades directamente de la función de masa:
#I) P(X = 0), P(X = 123) Y P(X = 62); II) P P(0 = X = 10), P(0 < X = 10), Y P(0 = X < 10); III) P(X > 11) y P(X = 10).
###########################################################################################################################################


#Función que calcula probabilidad de masa de una v.a. Binomial(123,0.31)
#Entrada: "X"  
#Salida: función de distribución, es decir, P[X=x]
distbinom<-function(x){
  n=123
  p=0.31
  
  fx=((factorial(n))/((factorial(x))*(factorial(n-x))))*(p^x)*((1-p)^(n-x))
  
  return(fx)
}


#P(X = 0)
distbinom(0)
#P(X = 123)
distbinom(123)
#P(X = 62)
distbinom(62)
#P(X = 62)
distbinom(62)

#P(0 <= X <=10)
sum(distbinom(0:10))
#P(0 < X <= 10)
sum(distbinom(0:10))-distbinom(0)
#P(0 <=X < 10)
sum(distbinom(0:10))-distbinom(10)

#P(X > 11)
1-sum(distbinom(0:11))
#P(X <= 10)
sum(distbinom(0:10))

############################################################################################################################################
#b)	Calcule las probabilidades del inciso anterior usando las funciones pbinom y dbinom.
#############################################################################################################################################


#P(X = 0)
dbinom(0,123,.31)
#P(X = 123)
dbinom(123,123,.31)
#P(X = 62)
dbinom(62,123,.31)

#P(0 <= X <=10)
pbinom(10,123,.31, TRUE,FALSE)
sum(dbinom(0:10,123,.31))

#P(0 < X <= 10)
pbinom(10,123,.31, TRUE,FALSE)-dbinom(0,123,.31)
sum(dbinom(0:10,123,.31))-dbinom(0,123,.31)

#P(0 <=X < 10)
pbinom(10,123,.31, TRUE,FALSE)-dbinom(10,123,.31)
sum(dbinom(0:10,123,.31))-dbinom(10,123,.31)

#P(X > 11)
1-pbinom(11,123,.31, TRUE,FALSE)
1-sum(dbinom(0:11,123,.31))

#P(X <= 10)
pbinom(10,123,.31, TRUE,FALSE)
sum(dbinom(0:10,123,.31))

###########################################################################################################################################
#c)	Escriba un programa en R que calcule los cuentiles de 0.25, 0.5 y 0.75. 
# ¿Existe alguna función en R que calcule los cuantiles?
##############################################################################################################################################
set.seed(13)
vec <- rbinom(100000, 123,.31)
#vec <- c(8,9,7,8,5,5,6,4,8,1,8,3,10)

vec <- sort(vec)

if(length(vec)%%2 == 0)
{
  p1 <- (1 *(length(vec) + 1))/4
  p2 <- (2 *(length(vec) + 1))/4
  p3 <- (3 *(length(vec) + 1))/4
  
  if(p1%%1 != 0)
  {
    q1 <- vec[floor(p1)] + ((vec[ceiling(p1)] - vec[floor(p1)])*(p1 - floor(p1)))
  }else
  {
    q1 <- vec[p1]
  }
  if(p2%%1 != 0)
  {
    q2 <- vec[floor(p2)] + ((vec[ceiling(p2)] - vec[floor(p2)])*(p2 - floor(p2)))
  }else
  {
    q2 <- vec[p2]
  }
  if(p3%%1 != 0)
  {
    q3 <- vec[floor(p3)] + ((vec[ceiling(p3)] - vec[floor(p3)])*(p3 - floor(p3)))
  }else
  {
    q3 <- vec[p3]
  }
  
}else{
  p1 <- (1 *(length(vec) + 1))/4
  p2 <- (2 *(length(vec) + 1))/4
  p3 <- (3 *(length(vec) + 1))/4
  q1 <- vec[p1]
  q2 <- vec[p2] 
  q3 <- vec[p3]
}

cat(sprintf("Cuartiles:\n25:%s 50:%s 75:%s", q1, q2, q3))

quantile(vec)
#bien


###################################################################################################################################################
#Cuantil 0.25
qbinom(.25,123,.31,TRUE, FALSE)
#Cuantil 0.5
qbinom(.5,123,.31,TRUE, FALSE)
#Cuantil 0.75
qbinom(.75,123,.31,TRUE, FALSE)


##############################################################################################################################################
#8.	Una urna contiene 46 bolas grises y 49 bolas blancas. Usando la función sample en R, simule una extracción sin remplazamiento 
#de 20 de estas bolas y cuente el número de bolas grises que obtuvo. Repita este proceso 10^{6} veces,
#muestre sus primeros 3 resultados y grafique las frecuencias de bolas grises obtenidas en cada experimento. 
#¿Cuál es la probabilidad de que al extraer 20 bolas de la urna 5 de ellas sean grises? También grafique la proporción de las
#bolas grises obtenidas en los experimentos anteriores y sobre esta figura añada la correspondiente función de masa de l
#a distribución Hipergeométrica asociada al experimento total. 
#############################################################################################################################################

N=95 #Número total
n=20 #Observaciones en estudio
k=46 #Total de éxitos(núm de bolas grises)


gris=46
blanco=49
ngris=0


for (i in 1:20) { #Saca 20 bolas 
  s=sample(c(0,1),size = 1, replace = TRUE, prob=c((blanco)/(N-i),(gris)/(N-i))) 
  ngris[i]<-c(sum(s))
  if(s==1){
    gris=gris-1
  }  else{
    blanco=blanco-1
  }
}
total=sum(ngris)
total #Número de bolas grises que salieron de las 20.

#############################################################################################################################################
ngris<-0
for (j in 1:1000000) {
   for (i in 1:20) {
    s=sample(c(0,1),size = 1, replace = FALSE, prob=c((blanco)/(N-i),(gris)/(N-i)))
    ngris[i]<-c(sum(s))
    if(s==1){
      gris=gris-1
    }    else{
      blanco=blanco-1
    }
  }
  if(j<=3){ #Muestra los primeros 3 resultados.
    print(paste("El numero de bolas grises y blancas, respectivamente en el primer lanzamiento son: ",i))
    print(ngris[i])
    print(20 - ngris[i])
  }
}
ngris #Grafica
f=data.frame(table(ngris))
plot(f, main="FRECUENCIAS", xlab="Número de veces que salio la bola gris.",
      ylab="Frecuencia", type="l", col="blue")
hip=dhyper(x=1:20,m=46,n=49,k=20)
lines(hip, main="Hipergeométrica(N=95,k=46,n=20)", ylab="función de masa",
     xlab="variables aleatorias", type="l", col="blue",ylim=c(0,1))
# te falto simular 10^6 veces

#############################################################################################################################################
#Probabilidad Hiper(x=5,m=46,n=49,k=20)
##############################################################################################################################################
dhyper(x=5,m=46,n=49,k=20)

#############################################################################################################################################
#Grafica de la función Hipergeométrica
############################################################################################################################################

y=dhyper(x=1:20,m=46,n=49,k=20)
plot(y,main="FUNCIÓN HIPERGEOMETRICA",type="l", col="pink",ylim=c(0,1))
#bien te doy 2/3