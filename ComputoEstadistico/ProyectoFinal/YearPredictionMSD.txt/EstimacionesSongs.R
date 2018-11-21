###################### Packages necesarios
library(ggplot2) #graficos
library(easyGgplot2)
###################### Funciones
MSE <- function(y.hat, y)
{
    # Calculo de Mean Squared Error
    # Entradas
    # y.hat (numeric): vector con las estimaciones d eun modelo
    # y     (numeric): vector con los valores verdaderos
    return(mean((y-y.hat)**2))
}
###########################
setwd("C:/Users/fou-f/Desktop/Third/ComputoEstadistico/ProyectoFinal/YearPredictionMSD.txt") #fijamos el directorio de trabajo
dir()
t1 <- Sys.time()
songs <- read.csv('YearPredictionMSD.txt', stringsAsFactors = FALSE, header = FALSE)
t1 <- Sys.time() - t1
t1 # lectura 1.21231 mins
names(songs) <- c('y', paste0('X', 1:90))
################################
# Regresiones
###############################
# Casos con OLS
train <-  datos[1:463715, ]
test <- datos[463715:dim(datos)[1], ]
# OLS
