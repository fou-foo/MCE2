set.seed(13)
# n Limite de distribucion uniforme discreta U(1,2,..,n)
n <- 10 
# Los posibles valores que puede tomar x 
x <- c(1:n) 
# En muestra guardo los 10000 resultados, replace = TRUE ya que 
# el experimento se repite una y otra vez sobre c(1:n) completo cada vez
muestra <- sample(x, 10000, replace=TRUE, prob=NULL)
# Cálculo de las frecuencias, la media y la varianza de la muestra
frecuencia <- table(muestra)
media <- mean(muestra)
varianza <- var(muestra)
print(paste("La media es ", media))
print( paste("La varianza es ", varianza) )
print("Las frecuencias de las apariciones de x son  :")
print(frecuencia)
plot(frecuencia, type="h", xlab="x", ylab="Frecuencia", 
     main="Numero de apariciones en la muestra")
#bien

