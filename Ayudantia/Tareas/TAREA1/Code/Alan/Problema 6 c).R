# La moneda contiene un 0 que representa el Sol y 1 que representa Aguila
moneda <- c(0, 1)
# En este caso es necesario un vector con las probabilidades, siendo 0.7 la del
# primer elemento (sol) y 0.3 la del segundo (aguila)
probabilidad <- c(0.7, 0.3)
# Con la funcion sample se obtienen con remplazo 10 valores, usando tambien el vector
# de probabilidad como uno de los parameros,como el aguila esta representado con un 1,
# la suma de dicha sample representa la cantidad de aguilas. 
# Lo anterior se replica 1000000 de veces y se guarda en el vector muestra.
muestra <- replicate(1000000, sum( sample( moneda, 10, replace=TRUE, prob=probabilidad) ), simplify="array")
# Impresion de los primeros 3 resultados
print("Los primeros 3 resultados son : ", quote=FALSE)
print(muestra[1])
print(muestra[2])
print(muestra[3])
frecuencia <- (table(muestra))
#La probabilidad empirica en base a la experimentacion
probabilidad.Empirica <- frecuencia/1000000
#Impresion de frecuencias
plot(frecuencia, type="h",xlab="x", ylab="Frecuencia", 
     main="Numero de apariciones en la muestra")
probabilidad.Analitica <- dbinom(c(0:10), 10, 0.3)
plot(c(0:10),probabilidad.Analitica, xlab="Cantidad de aguilas", ylab="Probabiliad",
     main="Comparacion entre experimentacion (rojo) y datos analiticos (verde)",
     type="h", col="green")
lines(c(0:10)+0.1,probabilidad.Empirica, type="h", col="red")
print("Dado que en este caso la probabilidad de obtener un aguila es menor", 
      quote=FALSE)
print("entonces se aprecia en la grafica se hace asimetrica mas 
      cargada hacia la izquierda", quote=FALSE)
#bien