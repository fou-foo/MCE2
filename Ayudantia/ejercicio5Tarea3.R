agave <- c(23.37,21.87,24.41,21.27,23.33,15.20,24.21,27.52,15.48,27.19,
  25.05,20.40,21.05,28.83,22.90,18.00,17.55,25.92,23.64,28.96,
  23.02,17.32,30.74,26.73,17.22,22.81,20.78,23.17,21.60,22.37)


####################
#
#a) Escriba una función en R que calcule la funcion de distribucion emprica para
# un conjunto de datos dado. La funcion debe tomar como paraametros al punto x
# donde se evalua y al conjunto de datos D.

#Utilizando esta funcion grafique la funcion de distribucion empirica
#asociada al conjunto de datos de lluvias. Ponga atencion a los puntos de discontinuidad.
#¿Que observa?
F.empirica <- function(datos)
{
  # correcion por continuidad
  D <- datos
  max.i <- (max(D))+1/2
  min.i <- (min(D))-1/2
  D <- c(D, max.i, min.i)
      # Entrada datos (numeric) : vector con los datos observados
  soporte <- sort(D)
  N <- length(D)
  imagen <- cumsum(rep(1/N, N))
  plot(soporte, imagen, col='purple', pch=20, type ='S', xlab='Soporte',
       ylab='Frecuencia acumulada'  )
  rug(soporte, col= 'blue')
  rug(imagen,  col= 'blue', side=2)
}
F.empirica(datos =agave)


# inciso b) QQplot
QQ.plot <- function(datos)
{
    D <- sort(datos)
        # Entrada datos (numeric) : vector con los datos observados
    # calculo de la recta
    cuantiles <- c(.25, .75)
    teoricas <- qnorm(cuantiles)
    muestrales <- quantile(D, cuantiles)
    pendiente <- diff(muestrales) / diff(teoricas)
    intercepto <- quantile(D, probs = 0.5)
    N <- length(D)
    # graficacion de los puntos
    soporte <- cumsum(rep(1/(N+1), N)) #ligera correcion
    soporte.normal <- qnorm(soporte)
    plot(soporte.normal, D, pch=20, col="purple",
         ylab = "Distribución muestral",
         xlab = "Distribución teorica")
    abline(a=intercepto, b= pendiente, col="orange", lty=2)
}
QQ.plot(datos = agave)
