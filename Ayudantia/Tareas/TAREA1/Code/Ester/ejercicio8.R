####################### #
# Tarea 1 - Ejercicio 8 #
####################### #
rm(list = ls())

#guardara las veces que sale bola gris
b_grises <- data.frame()

#guardara los primeros 3 resultados
results_3 <- data.frame(extr=1:10)

#realizacion de experimiento
for( i in 1:10000){ #te faltaron realizaciones 
  extr <- sample(c(rep(1, 46), rep( 0, 49)), 20, replace=F)
  b_grises <- rbind(b_grises,  data.frame(result=sum(extr)) )
  
  #obtener los primeros 3 resultados
  if(i<=3){
    results_3 <- cbind(results_3, extr)
    colnames(results_3)[i+1] <- paste("exp_", i, sep="")
  }
  
}
frecuencia <- table(b_grises)
plot(frecuencia)

#Probabilidad de tener 5 grises
prob_5 <- frecuencia[which(row.names(frecuencia)==5)]/10000
prob_5 #si hubieses hecho el numero de realizaciones que se pide este numero cambia

proporciones <- frecuencia/100000
plot(proporciones, col="#973dd5")

prob <- dhyper(0:20, 46, 49, 20)
#agregar linea de densidad
lines(x=0:20, y=prob, lty=3, color="#3dd597")
plot (x=0:20,y=prob,type = "h", col="#3dd597") #estas lineas estan permutadas

# en genral bien, los errores son descuidos te doy .85 del ejercicio
