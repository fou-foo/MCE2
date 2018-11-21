plotUnif <- function(N) {
  # Graficar PMF Y CDF de una distribucion uniforme discreta sobre (1,....,N)
  
  # Generar Datos
  x <- seq(1,N)
  y <- rep(1/N, N)
  y_acumulada <- seq(1/N, 1, 1/N)
  # Graficar PMF
  plot(x, y,pch=21, bg ='black',  ylim = c(0,1), xlim= c(1,N),
       xlab = "Espacio Muestral", ylab = "Probabilidad", 
       main = paste("PMF Uniforme Discreta, N=",N), 
       cex.axis = 0.8) #puedes dar saltos de linea para que el 
              #codigo sea más legible
  # Graficar CDF
  plot(stepfun(1:N, seq(0 , 1, 1/N), right = TRUE), 
       verticals = FALSE, ylim = c(0,1), xlim= c(1,N), 
       xlab = "Espacio Muestral", 
       ylab = "Probabilidad Acumulada", 
       main = paste("CDF Uniforme Discreta, N=", N),  
       cex.axis = 0.8)
}

# Graficar PMF y CDF para los casos N= 5, 10, 50
plotUnif(5)
plotUnif(10)
plotUnif(50)

###################################Segunda parte del problema########################

# Generar sample y obtener recuencias de muestra de tamaño 10,000 de datos provenientes 
#de U(1,..,10) 
set.seed(13)
x <- 1:10 #esta linea la gregue yo 
datos <- sample(x,10000, replace= TRUE) #te falto definir tu objeto 'x' supongo que es 1:10
freqs <- table(datos)

# Mostrar tabla de frecuencias
print("Frecuencias:")
print(freqs)
# Mostrar Media y Variancia:
cat("\n")
print(paste("Media:", mean(datos)))
print(paste("Varianza:", var(datos)))

# Graficar Histograma de Frecuencias
plot(freqs, xlab = "Num", ylab = "Frecuencia", 
     main = "Histograma de frecuencias, N = 10 000" )
# Funcional prueba esta linea 
barplot(freqs)
# te doy .9 de este ejercicio porque te falto declarar el objeto 'x'
# siempre que hagas test a tu codigo limpia tu actuel enviroment
rm(list=ls())
