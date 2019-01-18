setwd("C:\\Users\\fou-f\\Desktop\\SPI2019\\app\\")
dir()





###############################################
# Datos #######################################
datos <- read.csv("ELCHIDO.csv") # requerimos que el dataframe se llame data
datos$tTipoAparato <- as.character(datos$tTipoAparato)
datos$iNumFases <- as.character(datos$iNumFases)
3#datos$iFrecuencia <- as.character(datos$iFrecuencia)
datos$tTipoArreglo <- as.character(datos$tTipoArreglo)
datos$tTipoOptimizador <- as.character(datos$tTipoOptimizador)
datos$dRelTransformacion <- as.character(datos$dRelTransformacion)
datos$bLlevaTerciario <- as.character(datos$bLlevaTerciario)
datos$IDevCercano <- as.character(datos$IDevCercano)
#datos$IVoltDefPor <- as.character(datos$IVoltDefPor)
#datos$DVoltSistGen <- as.character(datos$DVoltSistGen)
datos$dVoltajeSistAT
datos$dVoltajeSistATInterfaz
datos$dVoltajePrefallaRL <- as.character(datos$dVoltajePrefallaRL)
datos$bTerciarioExt <- as.character(datos$bTerciarioExt)
datos$bTerciarioCargado <- as.character(datos$bTerciarioCargado)
datos$tNormaGar <- as.character(datos$tNormaGar)
#datos$iLlevaSerie <- as.character(datos$iLlevaSerie)
datos$MSNM <- as.character(datos$MSNM)
#datos$dZry <- as.character(datos$dZry)
#datos$dZsh <- as.character(datos$dZsh)
datos2 <- datos[, c(1,2,4:8,11:16,18)]
#################
sort(unique(datos2$tTipoArreglo))
###############
sapply(datos2, class)
entrada <- datos2[1, ]
save(entrada, file ='entrada.rdata')
write.csv(datos2, file='ELCHIDOCORTO.csv', row.names = FALSE)
################################################
################## Modelos ###################
arbol <- function(variable.predecir, data=datos2)
{
    # Closure para crear MUCHOS MODELOS
    # nombre.variable (string): Nombre de la variable que se va a sugerir
    # data (dataframe): Data frame con el historico
    # REGRESA UN MODELO ENTRENADO
    library(rpart)
    # generamos la formula para el modelo
    formula.foo <- as.formula(paste0(variable.predecir, "~ ."))
    rpart(formula.foo, data=data ) # 5 arboles peque FALTA MAXIMO NUMERO DE OPCIONES
}
# CREACION DE N MODELOS ########################
inicio <- Sys.time()
library(parallel)
MODELOS <- mclapply(names(datos2), function(x) arbol( variable.predecir=x), mc.cores = 1)
fin <- Sys.time()
fin-inicio
names(MODELOS) <- names(datos2)
names(datos2)
save(MODELOS, file='MODELOS.rdata')
MODELOS[['iNumFases']]
