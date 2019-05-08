############################################
# Implementacion de la metodologia VAR-PLS #
# J. Antonio Garcia jose.ramirez@cimat.mx  #
############################################

remove(list=ls()) # removemos todos los objetos del enviroment

###########################################
# librerias                               #
{
library(vars) # tiene varias dependencias (implicitamente carga en el ambiente otras) util para modelos VAR
library(pls)  # para estimacion pls
library(psych) # solo ocupamos una funcion de aqui CHECAR CUAL ES
library(ggplot2) # libreria de graficos
library(lubridate)  # libreria para manejo de fechas
library(reshape2) #manipulacion de dataframes
  
}
###########################################
# Parametros                              #
{
path <- '/home/fou/Desktop/MCE2/4/Tesina/Code/' # ubicacion del archivo 'Funciones_VARPLSParallel.R' y los datos
h <- 6 # numero de steps a pronostricar
lag.max <- 15 # lag maximo para la determinacion inicial del AR(p)
runs <- 100  # numero de iteraciones bootstrap para los intervalos de confianza 
crit <- "FPE(n)" # criterio con cual elegir el orden inicial del VAR(p)
season <- NULL # PREGUNTAR A fRANCISCO
ec.det <- c("none", "const", "trend", "both") # posibles formas de tendencia
frecuencia <- 12 # frecuencia anual de las series
}
source(paste0(path, "Funciones_VARPLSParallel.R"))# cargar funciones auxiliares
##########################################
# Lectura de datos                       #
# Se espera un dataframe donde la primer columna sean las fechas de las series y la segunda la variable de interes a pronosticar
data <- read.csv(paste0(path, "CompendioObservatorio.csv"), row.names = 1)
##########################################
# imputacion de datos                    #
data <- na.omit(data)
data <- as.data.frame(apply(data, 2, log))
# data <- cbind(data, simulacion)
##########################################
# visualizacion 
data.s <- data
data.s$time <- row.names(data)
data.s$time <- dmy(data.s$time)
data.s <- melt(data.s, id='time')
ggplot(data.s, aes(x= time, y =log(value), color=variable)) + geom_line() +
  theme_minimal() + xlab('Tiempo') + ylab('log(x)') +
  theme(legend.position = "bottom", legend.title = element_text(color = "white")) +
  ggtitle('Variables econometrícas')
ggplot(data.s, aes(x= time, y =(value), color=variable)) + geom_line() +
  facet_wrap(variable~., scales = "free") +  theme_minimal() + xlab('') +
  ylab('') +  theme(legend.position = "bottom", legend.title = element_text(color = "white")) +
  ggtitle('Variables econometrícas') +guides( color=FALSE)
###########################################
data <- as.data.frame(sapply(data, log)) # estabilizacion de la varianza
# division de la muestra como sugiere Frances
n <- dim(data)[1] # tamaño total de la muestra
k <- dim(data)[2] # numero de componentes
Y <- tail(data, h)
X <- head(data, n-h)
p <- VARselect(y= X, lag.max = lag.max)$selection[crit]# determinacion del orden del VAR(p)
