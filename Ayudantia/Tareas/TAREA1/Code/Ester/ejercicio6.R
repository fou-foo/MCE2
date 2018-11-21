####################### #
# Tarea 1 - Ejercicio 6 #
####################### #
rm(list = ls()) # buena practica, bien

# a) 10 lanzamientos, 10^6 veces

#guardara las veces que sale águila
aguilas <- data.frame()

#guardara los primeros 3 resultados
results_3 <- data.frame(lanz=1:10)

#realizacion de experimiento
for( i in 1:100000){
  lanzamiento <- sample(0:1, 10, replace=T)
  aguilas <- rbind(aguilas,  data.frame(result=sum(lanzamiento)) ) #esta linea es muy costosa en tiempo y memoria
              # viendo tu nivel en el manejo del lenguaje te recomiendo 
              # comenzar a usar las funciones *apply
  
  #obtener los primeros 3 resultados
  if(i<=3){
    results_3 <- cbind(results_3, lanzamiento)
    colnames(results_3)[i+1] <- paste("exp_", i, sep="")
  }
  
}
#Tabla de frecuencias
frecuencia <- table(aguilas)
plot(frecuencia)
#hist(aguilas$result)

#Proporciones
proporciones <- frecuencia/100000
plot(proporciones, col="#973dd5")

# b) Grafica de funcion de masa utilizando dbinom
prob <- dbinom(0:10,10,.5)
#agregar linea de densidad
lines(x=0:10, y=prob, lty=3, color="#3dd597")

#En dado que se requieran las 2 graficas como histogramas
#plot(proporciones, col="#f3c8cf", lwd = 3)
#par(new=TRUE)
#plot (x=0:10, y=prob,type = "h", col="#3dd597") # falto una ','

#La probabilidad empirica es casi igual a la probabilidad teorica

# c) Realizar el experimento con probabilidad de 0.3

#guardara las veces que sale águila
aguilas <- data.frame()

#guardara los primeros 3 resultados
results_3 <- data.frame(lanz=1:10)

#realizacion de experimiento
for( i in 1:100000){
  lanzamiento <- sample(0:1, 10, replace=T, prob = c(.7, .3))
  aguilas <- rbind(aguilas,  data.frame(result=sum(lanzamiento)) )
  
  #obtener los primeros 3 resultados
  if(i<=3){
    results_3 <- cbind(results_3, lanzamiento)
    colnames(results_3)[i+1] <- paste("exp_", i, sep="")
  }
  
}
#Tabla de frecuencias
frecuencia <- table(aguilas)
plot(frecuencia)
#hist(aguilas$result)

#Proporciones
proporciones <- frecuencia/100000
plot(proporciones, col="#973dd5")

# b) Grafica de funcion de masa utilizando dbinom
prob <- dbinom(0:10,10,.5) # se te paso mover el valor de 'p'
prob <- dbinom(0:10, 10, .3) 
#agregar linea de densidad
lines(x=0:10, y=prob, lty=3, color="#3dd597")

#bien en general, salvo la probabilidad del segundo inciso te doy 
# .95 del ejercicio
