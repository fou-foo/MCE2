#--------------------------------------
# Actualizacion 6 Marzo 2019.
#--------------------------------------

remove(list=ls())
require(gplots)
library(vars)
library(pls)
library(forecast)
library(mvtnorm)
library(ks)
library("tictoc")
library(chron)

dt.file <-"/home/andres/Dropbox/ECONOMETRIA/Proyecto_inflacion/codigos/tasa_cambio/"
source(paste(dt.file, "funciones.r", sep =""))
source(paste(dt.file, "modelo.r", sep =""))

#---------------------------------------
# DATOS
#---------------------------------------

costos <- read.csv(paste(dt.file, "/datos/Costos.csv", sep = ""), row.names = 1)
monetario <- read.csv(paste(dt.file, "/datos/Monetario.csv", sep = ""), row.names = 1)
demanda <- read.csv(paste(dt.file, "/datos/Demanda.csv", sep = ""), row.names = 1)
precios <- read.csv(paste(dt.file, "/datos/Tasa.csv", sep = ""), row.names = 1)

#---------------------------------------
# PARAMETROS
#---------------------------------------
args <- commandArgs(TRUE)
print(c(args))


length.fore <- strtoi(args[1])  # Num. de meses a pronosticar
anual <- 12             # Para tasa interanual
c.sig <- 0.10           # Nivel de significancia
show.data <- 48         # ventana de tiempo
length.test <- 6        # Meses a probar  intramuestra
n.try <- 6              # Rezagos a probar
restrict <- FALSE       # TRUE Si pronostico no puede superar (min,max)
objective <- 3          # Lo usa en el bias - Ahora el objetivo de BM es 3  
lag.max <- 12           # Para el numero de modelos 
seas.max <- 12          # Para el numero de modelos
ec.det <- c("none", "const", "trend")
variable <- "Precios"
mes.first = "01/01/2005"
mes.last <- args[2]
region <- "tipo.de.cambio"

#--------------------------------------
# MODELO
#--------------------------------------
pronostico <- matrix(NA, nrow=length.fore+1)
pronostico_figura <- matrix(NA, nrow=length.fore+1)
#--------------------------------------
# corriendo modelo
tic()
print(mes.last)
RESULT <- TASA_CAMBIO(precios, costos, demanda, monetario, region, variable, mes.first, mes.last,
                    length.fore, lag.max, c.sig, show.data, seas.max, length.test, n.try, restrict, objective,ec.det)
toc()
#--------------------------------------
# Variables de interes
last <- which(rownames(precios)==mes.last)
index <- 1
real <- precios[(last):(last+length.fore),index]
forecast <- c(precios[last,index],RESULT[[2]][1:length.fore,1])
linf <-  c(precios[last,index], RESULT[[2]][1:length.fore,2])
lsup <-  c(precios[last,index],RESULT[[2]][1:length.fore,3])
CI <- c(0,RESULT[[2]][1:length.fore,4])
precision <- (1 - abs(real-forecast)/real)
mes.format <- as.Date(mes.last, format = "%d/%m/%Y")
mes_pronostico <-as.character(seq(as.Date(mes.format),by = "month", length = length.fore+1))
pronostico_figura <- cbind(mes_pronostico, forecast, linf, lsup,CI,  real, precision)

#--------------------------------------
# RESULTADOS
#--------------------------------------
colnames(pronostico_figura) <- c("FECHA", "Pronostico","lim inf","lim sup", "CI", "Real", "Precision")
mes.auto <- sub("-01", "", mes.format)
write.csv(pronostico_figura, paste(dt.file, "resultados_boletin/",length.fore,"/",mes.auto,"/Pronostico_TasaCambio_",mes.auto,".csv", sep = ""),row.names = F)


