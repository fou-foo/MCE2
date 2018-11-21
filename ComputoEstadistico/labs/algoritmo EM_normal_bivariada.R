#=====================================================
# Estimación de los parámetros de la normal Bivariada,
# a partir de una muestra con observaciones faltantes, aplicando el algoritmo EM

library("MASS")

# Función genérica para estimar los parametros de una normal bivariada,
# considerando una muestra incompleta de datos,mediante el algoritmo EM

# Argumentos: Y - Datos
#             R - Matriz del mismo tamaño que Y, cuyos entradas indican si el dato
#                 es un dato faltante u observado
#             p - vector con los valores iniciales de los parámetros
#               p[1] media de Y[,1]
#               p[2] media de Y[,2]
#               p[3] desv.estandar de Y[,1]
#               p[4] desv.estandar de Y[,2]
#               p[5] correlación de Y[,1] y Y[,2]
#

emiteration <- function(Y, p, R, n=1000) #quitamos el peligroso parametro '...'
{
  p.old <-  emstep(Y, p, R)
  p.new <- emstep(Y, p.old, R)
  for (i in (3:n))
  {
      tolerancia <- sum((p.old -p.new)**2)**.5
      if( tolerancia > 0.001) #fijamos una tolerancia como lo sugieren las notas
      {
          p.old <- p.new
          p.new <- emstep(Y, p.old, R)
      }
  }
  return(list(estimacion=p.old, iteraciones=i)) #regresamos la estimacion y el numero de iteraciones que requirio
}

emstep <- function(Y,p,R)
{
  n <- nrow(Y) #tamanio de la muestra
  l <- length( R[R[,1] == 0,1] ) # numero de missings en la primer variable
  m <- length( R[R[,2] == 0,2] ) # numero de missings en la segunda variable
    # guardamos los vectores de observaciones completos
  Y1s <- Y[,1]
  Y2s <- Y[,2]
  #E-step
  Y1s[R[,1] == 0] <- p[1] + p[5]*p[3]/p[4]*(Y2s[R[,1] == 0] - p[2]) # esperanza de la primer variable
  Y2s[R[,2] == 0] <- p[2] + p[5]*p[4]/p[3]*(Y1s[R[,2] == 0] - p[1]) # esperanza de la segunda variable
  T[1] <- sum(Y1s)
  T[2] <- sum(Y2s)
  T[3] <- sum(Y1s**2)+l*(1-p[5]^2)*p[3]^2 # varianzas sin sesgo
  T[4] <- sum(Y2s**2)+m*(1-p[5]^2)*p[4]^2 # varianzas sin sesgo
  T[5] <- sum(Y1s*Y2s)
  #M-step
  p[1] <- T[1]/n
  p[2] <- T[2]/n
  p[3] <- sqrt(T[3]/n-p[1]**2) # varianza sin sesgo
  p[4] <- sqrt(T[4]/n-p[2]**2) # varianza sin sesgo
  p[5] <- (T[5]/n-p[1]*p[2])/(p[3]*p[4]) #varianza sin sesgo
  p
}

# una vez que se tiene la estimacion final de los parámetros,
# realizamos el paso E por ultima vez para obtener las
# estimaciones finales de los datos faltantes

impE <- function(Y,p,R)
{
    n <- nrow(Y) #numero de observaciones
    l <- length(R[R[,1] == 0,1]) # no nulos en la primer variable
    m <- length(R[R[,2] == 0,2]) # no nulos en la segunda variable
    Y1s <- Y[,1] # copia de vaalores
    Y2s <- Y[,2] #
  #E-step
  Y1s[R[,1] == 0] <- p[1] + p[5]*p[3]/p[4]*(Y2s[R[,1] == 0] - p[2])
  Y2s[R[,2] == 0] <- p[2] + p[5]*p[4]/p[3]*(Y1s[R[,2] == 0] - p[1])
  YIMP <- cbind(Y1s,Y2s)
}

#######################################################################################
#Datos del ejemplo

 Y <- matrix(0,10,2)
 Y[,1] <- c(NA,NA,NA,52,51,67,48,74,74,50)
 Y[,2] <- c(159,164,172,167,164,161,168,181,183,158)

 #numero de observaciones en la muestra
 nobs <- nrow(Y)

 # numero de observaciones completas (casos completos)
 POS.faltantes <- is.na(Y[,1]) #identifica la ubicación de los datos faltantes en Y1
 POS.disponibles <- !POS.faltantes # se identifica la ubicacion de los datos disponibles
 nobs.comp <- nrow(Y[POS.disponibles,]) #numero de casos completos

 #variable indicadora de los valores perdidos
 R <- ifelse(is.na(Y),0,1)

 #se da una estimacion inicial de los parametros, los cuales se colocan en el vector p
 media_ini <- colMeans(Y,na.rm = TRUE) #calculamos la media con los datos disponibles
 cov_ini <- ((nobs.comp-1)/nobs.comp)*var(Y,na.rm = TRUE) #calculamos la matriz de covarianza con los casos completos
 p <- c(media_ini[1], media_ini[2],
        sqrt(cov_ini[1,1]), sqrt(cov_ini[2,2]),
        (cov_ini[1,2]/(sqrt(cov_ini[1,1])*sqrt(cov_ini[2,2]))))
p #estimaciones iniciales de los parámetros
# #Starting values
# #Start 1

 #los parametros iniciales son los calculados originalmente en el ejemplo
 #p<-c(59.43,167.7,sqrt(118.24),sqrt(79.26),(70.06/(sqrt(118.24)*sqrt(79.26))))
#Y<-datos

##########################################################################################

# Se aplica el algoritmo EM
p <- emiteration(Y, p, n=1e8, R=R)
p$estimacion #estimaciones finales de los parametros
p$iteraciones
#Check for convergence
#p<-emstep(Y,p,R=R)

#para obtener la estimacion final los datos faltantes, realizamos el paso E por ultima vez con las estimaciones finales de los parametros
YIMP <- impE(Y,p$estimacion, R=R)
YIMP
#datos_impx1<-YIMP[indX1,1]
#datos_impx2<-YIMP[indX2,2]

########################################################################################
#Se estimarán los parámetros de una distribucion normal bivariada a partir de una muestra con observaciones perdidas
#en ambas variables

#primero se genera una muestra de 100 observaciones a partir de una poblacion normal bivariada
tam_muestra = 100
mu = c(0,0) #se define un vector de medias
sigma = matrix(c(1,0.8,0.8,1),2) #se define la matriz de covarianza
set.seed(0)
datos = mvrnorm(tam_muestra,mu,sigma) #generamos 100 datos de una distribucion normal bivariada de media mu y varianza sigma

#se eliminan m datos de cada columna,
m=2 # en esta caso se eliminan 2 datos de cada columna

#se eligen aleatoriamente los datos que se eliminaran de cada columna
ind1 <- 1:tam_muestra
indX1 <- sample(ind1, m,replace=FALSE, prob=NULL)
indX2 <- sample(ind1, m,replace=FALSE, prob=NULL)

#se identifican los datos que se eliminaran
dat_elimiX1 <- datos[indX1,1]
dat_elimiX2 <- datos[indX2,2]

#los datos eliminados se sustituyen por NA
datos[indX1,1] = NA
datos[indX2,2] = NA

#variable indicadora de los valores perdidos
R <- ifelse(is.na(datos),0,1)

#se da una estimacion inicial de los parametros,p
media_ini <- colMeans(datos,na.rm = TRUE) #se estima la media con los datos disponibles
cov_ini <- var(datos,na.rm = TRUE) #se estima la covarianza con los casos completos
#los cuales se colocan en el vector p
p <- c(media_ini[1],media_ini[2],sqrt(cov_ini[1,1]),sqrt(cov_ini[2,2]),(cov_ini[1,2]/(sqrt(cov_ini[1,1])*sqrt(cov_ini[2,2]))))

Y <- datos
##################################################################
# Se aplica el algoritmo EM
p <- emiteration(Y, p, n=1e8, R=R)
p$estimacion #estimaciones finales de los parametros
p$iteraciones
#Check for convergence
#p<-emstep(Y,p,R=R)
#estimacion final de los parámetros
#p
#para obtener la estimacion final los datos faltantes, realizamos el paso E por ultima vez con las estimaciones finales de los parametros
YIMP <- impE(Y,p$estimacion,R=R)
datos_impx1 <- YIMP[indX1,1] #estimacion final de los datos faltantes de la variable y1
datos_impx1
datos_impx2 <- YIMP[indX2,2] #estimacion final de los datos faltantes de la variable y2
datos_impx2


dat_elimiX1 #datos originales eliminados de la variable x1
dat_elimiX2 #datos originales eliminados de la variable x2

