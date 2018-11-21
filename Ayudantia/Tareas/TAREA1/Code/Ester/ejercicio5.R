####################### #
# Tarea 1 - Ejercicio 5 #
####################### #
rm(list = ls())

# a) Graficas de distribución y distribución acumulada

#Funcion que dado el tamaño de muestra despliega graficas de función
#de distribución y distribución acumulada
plot_unif <- function(n){
  #input:  tamaño de muestra
  #output: graficas
  
  probabilidad <- 1/n
  #funcion de probabilidad para todos los valores
  funcion_prob <- rep(probabilidad, n)
  #probabilidad acumulada
  prob_acum <- cumsum(funcion_prob)
  
  #graficas
  par(mfrow=c(2,1))
  #funcion de distribución
  plot(x=1:n, y=funcion_prob, xlab = "x", ylab="f(x)", type="h",
       xlim = c(0,n), ylim=c(0, probabilidad+probabilidad/2), main = "Funcion de Distribución")
  #funcion acumulada
  plot(x=1:n, y=prob_acum,    xlab = "x", ylab="F(x)", type="p",
       xlim = c(0,n+1), ylim=c(0, 1), main="Distribución Acumulada")
  lines(1:n, prob_acum, type="s")

}

#Impresion de gráficas
plot_unif(5)
plot_unif(10)
plot_unif(50) # checa los limites del eje y en las funciones de masa

par(mfrow=c(1,1))

# c) Simulación de muestra uniforme, mostrar media y varianza

#semilla
set.seed(13)

#generacion de datos
datos <- sample(1:10, 10000, replace = T)
muestra <- table(datos)

#graficas
plot(muestra)
hist(datos, breaks = 0:10)

#Tabla
View(muestra)
media <- mean(datos)
varianza <- var(datos)

#Mostrar variables
print(muestra)
print(media)
print(varianza)
# se nota que has usado R, bien.
