# n es el limite superior de la distribucion uniforme discreta U(1,2,...,n)
# Para n=5
n <- 5
probabilidades <- rep(1/n, n) 
plot(probabilidades, type="b", xlab="Espacio Muestral", #Grafica PMF
     ylab="Probabilidad", main="PMF Uniforme Discreta",
     xlim=c(1, n), ylim=c(0, 1))
# Las sumas acumuladas dan las probabiliades acumuladas
probabilidades.Acumuladas <- cumsum(probabilidades) 
# Grafica CDF
plot(probabilidades.Acumuladas, type="s", xlab="Espacio Muestral", 
     ylab="Probabilidad Acumulada", main="CDF Uniforme Discreta",
     xlim=c(1, n), ylim=c(0 ,1))
# Para n=10
n <- 10
probabilidades <- rep(1/n, n) 
plot(probabilidades, type="b", xlab="Espacio Muestral", #Grafica PMF
     ylab="Probabilidad", main="PMF Uniforme Discreta",
     xlim=c(1, n), ylim=c(0, 1))
# Las sumas acumuladas dan las probabiliades acumuladas
probabilidades.Acumuladas <- cumsum(probabilidades) 
# Grafica CDF
plot(probabilidades.Acumuladas, type="s", xlab="Espacio Muestral", 
     ylab="Probabilidad Acumulada", main="CDF Uniforme Discreta",
     xlim=c(1, n), ylim=c(0, 1))
# Para n=50
n <- 50
probabilidades <- rep(1/n, n) 
plot(probabilidades, type="b", xlab="Espacio Muestral", #Grafica PMF
     ylab="Probabilidad", main="PMF Uniforme Discreta",
     xlim=c(1, n), ylim=c(0, 1))
# Las sumas acumuladas dan las probabiliades acumuladas
probabilidades.Acumuladas <- cumsum(probabilidades) 
# Grafica CDF
plot(probabilidades.Acumuladas, type="s", xlab="Espacio Muestral", 
     ylab="Probabilidad Acumulada", main="CDF Uniforme Discreta",
     xlim=c(1, n), ylim=c(0, 1))
#bien