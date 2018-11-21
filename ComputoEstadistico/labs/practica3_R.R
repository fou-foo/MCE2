setwd("~/Desktop/Third/ComputoEstadistico/labs")

# Practica 3 . Estimacion de datos faltantes aplicando  diferentes métodos de imputación
library("MASS")
#library("snpMatrix")
library("alr3")
library("car")

#directorio o ruta donde están las funciones que imputan los datos
source("functions_imp.r")

######################################################################################

#se lee un conjunto de datos que tiene datos faltantes

#los datos se generaron de una encuesta aplicada por un  proveedor de telecomunicaciones,
# que desea comprender mejor los patrones de uso de servicios en su base de datos de clientes. Sin embargo  existen varios datos faltantes en el campo "ingresos"

telco_data <- read.csv("telco_missing.csv", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, na.strings = "NA")
# se llenan los valores faltantes con "NA"


#########################################################################################

# se seleccionan  los casos completos, es decir los casos donde existe informacion de ingresos y de las variables auxiliares

ing <- telco_data[,"income"]
n <- length(ing)

# se determina el numero de datos faltantes y observados en la variable "ingresos"
n_per <- sum(is.na(ing))
n_obs <- n-n_per

#se seleccionan los valores de las restantes columnas(variables auxiliares)
ind <- 1:30 # son 30 variables en total incluyendo ingresos
ind_var_aux <- ind !=5
var_aux <- telco_data[,ind_var_aux]

#se define la probabilidad de que cada dato pertenezca a la muestra,
# la definiremos como 1 en todos los casos, es decir, que
#todos los datos pertenece a la muestra
Pi <- rep(1,n)

# se separan los casos completos de datos mediante la funcion separa.muestras
muestray <-ing
muestrax <- var_aux
resultados <-SEPARA.muestras(muestray,muestrax,Pi)


#############################################################################################


#se imputan los datos faltantes del "ingreso" mediante la media de los datos observados (media incondicional)


# funcion que imputa los datos faltantes mediante la media
res_imp_media <-  METODO.media(resultados$datosy.r,resultados$Pi.r,resultados$m)

#se llenan los valores faltantes con los datos imputados
 ingreso_imp.media<- REALIZA.imputacion(ing,res_imp_media,resultados$POS.faltantes)



#########################################################################################
#se imputan los datos faltantes del "ingreso" mediante el metodo de Cohen

# funcion que imputa los datos mediante el metodo de Cohen

res_imp_cohen <- METODO.cohen(resultados$datosy.r,resultados$Pi.r,resultados$m)

#se llenan los valores faltantes con los datos imputados
ingreso_imp.cohen<- REALIZA.imputacion(ing,res_imp_cohen,resultados$POS.faltantes)
#OJO: a la mitad de los datos faltantes se les asigna un valor negativo

#############################################################################################
#se imputan los datos faltantes del "ingreso", mediante regresion (media condicional), para lo cual
#1. Se identifican las variables auxiliares para realizar el analisis de regresion
#2. entonces se eligen los casos donde el valor del ingreso y de las variables auxiliares consideradas son observados (casos completos)
#3. Se construye el modelos de regresion con estos casos


#sis <-  data.frame (cbind("age","employ","address","ed","gender","reside","marital"))

#se construye el modelo de regresion con los casos completos
lm.imp.1 <- lm (ing ~ age + employ + address + ed + gender + reside + marital, data=telco_data)
#nota: lm construye el modelo de regresion implicitamente con los casos completos

#se predicen todos los valores observados y faltantes del ingreso, de acuerdo al modelo de regresión construido
pred.1 <- predict (lm.imp.1, telco_data)

#se eligen los valores predichos para los datos faltantes
pred.1_perd<- pred.1[resultados$POS.faltantes]

# se llenan los datos faltantes con los valores predichos por la regresion (media condicional)
ingresos.imp.reg<- REALIZA.imputacion(ing,pred.1_perd,resultados$POS.faltantes)


#nf <- layout(rbind(c(0,1,1,0), c(0,2,2,0)))

#se construye el histograma para los datos completos para el "ingreso" despues de la imputacion por regresion
hist(ingresos.imp.reg, main ="Histograma de los ingresos completos")
#se construye el diagrama de dispersion entre los ingresos estimados por el modelo de regresion y los ingresos completos
#(con los datos faltantes imputados por regresion)
plot(pred.1,ingresos.imp.reg)

#OJO: existen algunos ingresos predichos con valor negativo

#vamos a transformar la variable ingreso, aplicando raiz cuadrada  a los datos observados

#se aplica raiz cuadrada a los valores observados de los ingresos y se vuelve a correr el modelo de regresion
lm.imp.2.sqrt <- lm (I(sqrt(ing)) ~age + employ + address + ed + gender + reside + marital, data=telco_data)


#se predicen todos los valores observados y faltantes del ingreso de acuerdo al modelo de regresión (considerando
#la tranformacion de los ingresos)
pred.2.sqrt <- predict (lm.imp.2.sqrt, telco_data)


#todos los valores predichos por el modelo son regresados a su escala original (elevandolos al cuadrado)
pred.2 <- pred.2.sqrt^2

#se eligen los valores predichos para los datos faltantes
pred.2_perd <-pred.2[resultados$POS.faltantes]

# se llenan los datos faltantes con los valores predichos por la regresion (regresados a su escala original)
ingresos.imp.reg2 <-REALIZA.imputacion(ing,pred.2_perd,resultados$POS.faltantes)

#se construye el histograma para los datos completos despues de la imputacion los datos por regresion

#nf <- layout(rbind(c(0,1,1,0), c(0,2,2,0)))
hist(ingresos.imp.reg2, main ="Histograma de los ingresos completos")

#se construye el diagrama de dispersion entre los ingresos estimados por el modelo de regresion y los ingresos completos
#(con los datos faltantes imputados por regresion)
plot(pred.2,ingresos.imp.reg2)

##########################################################################################
#se imputan los datos faltantes del ingreso, mediante regresion estocastica
# para esto debemos añadir un factor de incertidumbre (error aleatorio) de prediccion en la regresion.
#Para esto,construimos un vector aleatorio de valores predichos para los datos faltantes del ingreso utilizando la distribucion normal
#y despues elevamos al cuadrado los datos para regresarlos  a su escala original


pred.3.sqrt <- rnorm (n, predict (lm.imp.2.sqrt, telco_data),sigmaHat(lm.imp.2.sqrt)) #genera numeros aleatorios de una normal
#con  media los valores predichos por el modelo de regresion y varianza la varianza de los residuales

pred.3 <- pred.3.sqrt^2 # se elevan al cuadrado


#se eligen los valores predichos para los datos faltantes
pred.3_perd <-pred.3[resultados$POS.faltantes]

# se llenan los datos faltantes con los valores predichos por la regresion estocastica(despues agregar un error aleatorio)
ingresos.imp.reg3 <-REALIZA.imputacion(ing,pred.3_perd,resultados$POS.faltantes)

#nf <- layout(rbind(c(0,1,1,0), c(0,2,2,0)))

#se construye el histograma para los datos completos despues de la imputacion los datos por regresion estocastica
hist(ingresos.imp.reg3, main ="Histograma de los ingresos completos")


#####################################################################################################################

#se imputan los datos faltantes del ingreso, mediante el metodo Hot-deck aleatorio
# es decir los datos faltantes se imputan con valores observados de ingresos elegidos aleatoriamente

#nf <- layout(rbind(c(0,1,1,0), c(0,2,2,0)))


#ingreso.imp.RHD <- random.imp (ing)
#hist(ingresos.imp, main ="Histograma de los ingresos completos")

res_imp_RHD <-METODO.RHD(resultados$datosy.r,resultados$Pi.r,resultados$m)

#se llenan los valores faltantes con los datos imputados por el metodo RHD
ingreso.imp.RHD<- REALIZA.imputacion(ing,res_imp_RHD,resultados$POS.faltantes)

#se construye el histograma para los datos completos despues de la imputacion los datos por el metodo Hot-deck aleatorio
hist(ingreso.imp.RHD, main ="Histograma de los ingresos completos")

#####################################################################################################################

#se imputan los datos faltantes del ingreso, mediante el metodo Hot-deck en funcion de distancias (vecinos mas cercanos)
set.seed(0)
t1 <- Sys.time()
res_imp_DHD.foo <- METODO.NNI.foo(datosy.r =resultados$datosy.r,
                          datosx.r = resultados$datosx.r,
                          datosx.m = resultados$datosx.m,
                          m = resultados$m)
t2 <- Sys.time() - t1
t2
set.seed(0)
t <- Sys.time()
res_imp_DHD <- METODO.NNI(datosy.r =resultados$datosy.r,
                              datosx.r = resultados$datosx.r,
                              datosx.m = resultados$datosx.m,
                              m = resultados$m)
(t4 <- Sys.time() - t)
t4
res_imp_DHD.foo - res_imp_DHD
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



#se llenan los valores faltantes con los datos imputados por el metodo  Hot-deck en funcion de distancias
ingreso.imp.DHD<- REALIZA.imputacion(ing,res_imp_DHD,resultados$POS.faltantes)

#se construye el histograma para los datos completos despues de la imputacion los datos por el metodo Hot-deck del vecino mas cercano
hist(ingreso.imp.DHD, main ="Histograma de los ingresos completos")

