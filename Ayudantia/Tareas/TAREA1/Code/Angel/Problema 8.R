# Urna simula ser la urna, representando con 0 las bolas blancas y con 1 las bolas grises
urna <- c(rep(0, times=49), rep(1, times=46) )
# Con la funcion sample, obtenemos 20 elementos sin remplazo de la urna
# como 1 representa las bolas grises, la suma de dicha sample es igual a 
# la cantidad de bolas grises obtenidas, luego esto lo replicamos 1000000 veces
# y los guardamos en un array llamado muestra
muestra <- replicate(1000000, sum( sample(urna, 20, replace=FALSE, prob=NULL) ), simplify="array")
# Para asegurar que contemos aunque sea como ceros las apariciones de todas las
# posibilidades para el resultado (del 0 al 20) hacemos factor a la muestra con los 
# niveles del 0 al 20
# Impresion de los primeros 3 resultados
print("Los primeros 3 resultados son : ", quote=FALSE)
print(muestra[1])
print(muestra[2])
print(muestra[3])
muestra <- factor(muestra, levels = c(0:20))
# Obtencion de las frecuencias y las probabilidades empiricas
frecuencia <- table(muestra)
probabilidad.Empirica <- frecuencia/1000000

# Grafica de las frecuencias
plot(frecuencia, type="h", xlab="x", ylab="Frecuencia", 
     main="Numero de apariciones en la muestra")

# Obtencion de la probabilidad analitica con la funcion dhyper
probabilidad.Analitica <- dhyper(c(0:20), 46, 49, 20)

plot(c(0:20), probabilidad.Analitica, xlab="Cantidad de bolas grises", ylab="Probabiliad",
     main="Comparacion entre experimentacion (rojo) y datos analiticos (verde)",
     type="h", col="green")
lines(c(0:20)+0.1, probabilidad.Empirica, type="h", col="red")

#Obtencion de la probabilidad de obtener 5 bolas grises al sacar 20 bolas de la urna
ans<-dhyper(5,46,49,20)
print(paste("La probabilidad de obtener 5 bolas grises al sacar 20 bolas de la urna 
            es : ", ans) ,quote=FALSE)
# bien Ángel