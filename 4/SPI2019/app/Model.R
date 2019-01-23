# En analisis de sensibilidad mostro que las 5 variables numericas mas importantes son
importantes.numericas <- c('Enfriamiento.i_TORElev4', 'Enfriamiento.i_HSRElev4',
                           'Enfriamiento.i_TORElev5', 'Enfriamiento.i_HSRElev5',
                           'dRuido2')
importantes.categoricas <- c('Garantias.dCapExcitacion', 'Garantias.dCapEficReg',
                             'Garantias.dCapPerd', 'Enfriamiento.i_ElevTempP3',
                             'iDestino')
####################################################################
setwd('/home/fou/Desktop/MCE2/4/SPI2019/app')
dir()
administrativas <- c(1,2,3,8,9)
categoricas <- c(4, 5, 6, 11, 12, 18, 19, 20, 21, 28, 29, 30, 31, 35, 55,
                 57, 63, 64, 65, 66, 67, 69, 70, 71, 72, 73, 74, 75, 76, 77,
                 88, 89, 90, 100, 101, 111, 112, 114, 117, 130, 132, 133, 134,
                 135, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 170)
embarque <- 29:31 # se eliminan 32 33 34 por estar en el conjunto de 'numericas'
numericas <- c(13, 15, 17, 22, 23, 24, 25,  27, 32, 33, 34, 36, 37, 38, 45,
               46, 47, 48, 49, 50, 58, 59, 60, 68, 78, 79, 80, 81, 82, 83, 84, 85,
               86, 87, 91, 93, 94, 97, 98, 102, 106, 107, 126, 127, 128, 129)

# definicion de funciones para rellenar el transformador base de la simulacion
calcula.moda <- function(x)
{
    # x (character): columna de tipo caracter
     # salida: mode de la columna
    frecuencias <- table(as.character(x))
    maxi <- which.max(frecuencias)
    moda <- frecuencias[maxi]
    return(names(moda))
}
calcula.mediana <- function(x)
{
    # x (numeric): columna de tipo numerica
    # salida: mediana de la columna
    mediana <- median(x)
    return( mediana)
}
###############################################
    # correcion de tipos de Datos y construccion del transformador base para la simulacion en la app ###########
datos <- read.csv("dataframeVerdesPresentes.csv", stringsAsFactors = FALSE) #
sapply(datos, class)
    # cast de numericas a factores 'character' para que las salidas de 'predict' sean caracteres y no niveles
datos[, categoricas] <- lapply(datos[, categoricas], function(x) factor(x))
datos[, embarque] <- lapply(datos[, embarque], function(x) factor(x))
    # construccion de transformacion base para la app
entrada <- datos[1, ]
entrada[1, categoricas] <- mapply(datos[, categoricas], FUN=calcula.moda)
entrada[1, embarque] <- mapply(datos[, embarque], FUN=calcula.moda)
entrada[1, numericas] <- mapply(datos[, numericas], FUN=calcula.mediana)

datos <- datos[, -c(administrativas, 26)]    # eliminamos las variables 'administrativas' por no ser importantes para la prediccion
entrada <- entrada[, -c(administrativas, 26)] # el indice 26 corresponde a dZsy que es constante en la muestra
entrada <- entrada[, c(importantes.categoricas, importantes.numericas)]
#########################
save(entrada, file ='entrada.rdata')
write.csv(datos, file='VariablesVerdesCorrectas.csv', row.names = FALSE)
################################################
################## Modelos ###################

arbol <- function(variable.predecir, data=datos[, c(importantes.categoricas, importantes.numericas)])
{
    # Closure para crear MUCHOS MODELOS
    # nombre.variable (string): Nombre de la variable que se va a sugerir
    # data (dataframe): Data frame con el historico
    # REGRESA UN MODELO ENTRENADO
    library(randomForestSRC)
    # generamos la formula para el modelo
    formula.foo <- as.formula(paste0(variable.predecir, "~ ."))
    rfsrcFast(formula.foo, data=data,
                   ntree = 1000) # modelo
}
# CREACION DE N MODELOS ########################
sapply(datos[, importantes.categoricas], class)
sapply(datos[, importantes.numericas], class)
inicio <- Sys.time()
#library(parallel)
#MODELOS <- mclapply(c(importantes.categoricas, importantes.numericas), function(x) arbol( variable.predecir=x), mc.cores = 8)



MODELOS <- list()
for (i in 1:length(c(importantes.categoricas, importantes.numericas)))
{
    variable <- c(importantes.categoricas, importantes.numericas)[i]
    print(variable)
    MODELOS[[i]] <- arbol(variable.predecir = variable)
}
fin <- Sys.time()
save(MODELOS, file = 'MODELOSRFRedox.rdata')
fin - inicio
