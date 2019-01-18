setwd("~/Desktop/SPI2019/Modelacion")
dir()
###############################################
# Datos #######################################
datos <- read.csv("dataframeVerdesPresentes.csv") # requerimos que el dataframe se llame data
################################################
################## Modelos ###################
arbol <- function(variable.predecir, data)
{
    # Closure para crear MUCHOS MODELOS
    # nombre.variable (string): Nombre de la variable que se va a sugerir
    # data (dataframe): Data frame con el historico
    # REGRESA UN MODELO ENTRENADO
    library(rpart)
    # generamos la formula para el modelo
    formula.foo <- as.formula(paste0(variable.predecir, "~ ."))
    rpart(formula.foo, data=data[1:40, ] ) # 5 arboles peque FALTA MAXIMO NUMERO DE OPCIONES
}
# CREACION DE N MODELOS ########################
MODELOS <- list()
f1 <- Sys.time()
for (i in 1:length(names(datos)))
{
    print(i)
    MODELOS[[i]] <-  arbol(variable.predecir = names(datos)[i], data=datos)
    print(names(datos)[i])
}
f2 <- Sys.time()-f1

