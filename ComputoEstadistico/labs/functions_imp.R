# tarea modificar script para que jale con todas las columnas y cualquier distancia
# funciones para imputar datos
# dower -> funcion
#función que identifica y separa los casos completos del conjunto de datos de análisis
# los argumentos son
#muestray: todos los valores en la variable de interes (xi) que tiene datos faltantes
#muestrax: valores de las otras variable xj, distintas de xi,  en la muestra.
#Pi: probabilidad de que cada dato pertenezca a la muestra(la definiremos como 1 en todos los datos)

SEPARA.muestras <- function(muestray, muestrax, Pi)
{
  POS.faltantes <- is.na(muestray)
  POS.disponibles <- !POS.faltantes
  datosy.r <- muestray[POS.disponibles]
  datosx.r <- muestrax[POS.disponibles,]
  Pi.r <- Pi[POS.disponibles]
  datosx.m <- muestrax[POS.faltantes,]
  m <- nrow(datosx.m)
  list(datosy.r=datosy.r, datosx.r=datosx.r, Pi.r=Pi.r,
       m=m, datosx.m=datosx.m, POS.faltantes=POS.faltantes)
}

##############################################
#funcion que imputa los datos faltantes media la media de los datos observados

METODO.media <- function(datosy.r, Pi.r, m)
{
  Pesos <- 1/Pi.r
  N.est <- sum(Pesos)
  Media <- (1/N.est)*sum(Pesos*datosy.r)
  DONANTES.media <- rep(Media,m)
  DONANTES.media
}

###############################################

#funcion que imputa los datos mediante el metodo de Cohen

METODO.cohen <- function(datosy.r, Pi.r, m)
{
  r <- length(Pi.r)
  n <- r + m
  Pesos <- 1/Pi.r
  N.est <- sum(Pesos)
  Media <- (1/N.est)*sum(Pesos*datosy.r)
  Dr <- sqrt((1/N.est)*sum(Pesos*(datosy.r-Media)^2))
  m1 <- round(m/2)
  m2 <- m - m1
  Raiz <- sqrt(n+r+1)/sqrt(r-1)
  DONANTES.cohen <- c(rep(Media+Raiz*Dr,m1),rep(Media-Raiz*Dr,m2))
  DONANTES.cohen
}
######################################################################

#funcion que que imputa los datos faltantes mediante el metodo hot deck aleatorio
METODO.RHD <- function(datosy.r, Pi.r, m)
{
  Pesos <- 1/Pi.r
  Prob <- Pesos/sum(Pesos)
  DONANTES.RHD <- sample(datosy.r, m,replace=T, prob=Prob)
  DONANTES.RHD
}

###########################################################################

#funcion que imputa los datos faltantes mediante el metodo hot-deck basado en distancias

METODO.NNI <- function(datosy.r, datosx.r, datosx.m, m)
{
  DONANTES.NNI <- c()
  for (j in 1:m)
  {
    Diferencias <- abs(datosx.m[j,2] - datosx.r[,2])
    Dif.min <- min(Diferencias)
    POS.min <- Dif.min==Diferencias
    DONANTES <- datosy.r[POS.min]
    Num.T <- sum(POS.min)
    if (Num.T==1) DONANTES.NNI <- c(DONANTES.NNI, DONANTES)
    else DONANTES.NNI <- c(DONANTES.NNI, sample(DONANTES,1))
  }
  DONANTES.NNI
}


############################################################################3
#funcion que devuelve todos los valores de la variable imputada, es decir, asigna los valores
#imputados en las posiciones donde se ha producido la no respuesta

REALIZA.imputacion <- function(muestray, DONANTES, POS.faltantes)
{
  muestray[POS.faltantes] <- DONANTES
  muestray
}





