###################### Packages necesarios
library(ggplot2) #graficos
library(xtable)
library(leaps)
library(glmnet)
###################### Funciones
MSE <- function(y.hat, y)
{
    # Calculo de error cuadratico medio
    return(mean((y-y.hat)**2))
}

###########################
setwd("C:/Users/fou-f/Desktop/Third/ComputoEstadistico/ProyectoFinal/YearPredictionMSD.txt") #fijamos el directorio de trabajo
t1 <- Sys.time()
songs <- read.csv('YearPredictionMSD.txt', stringsAsFactors = FALSE, header = FALSE)
t1 <- Sys.time() - t1
t1 # lectura 1.21231 mins
names(songs) <- c('y', paste0('X', 1:90))
########################### estandarizacion de los datos
datos <- scale(songs[, 2:91])
dim(datos)
datos <- as.data.frame(datos)
names(datos)
datos$y <- songs$y
datos <- as.data.frame(datos)
train <- datos[1:463715,]
test <- datos[463716:dim(datos)[1], ]
remove(datos, songs)
setwd("C:/Users/fou-f/Desktop/Third/ComputoEstadistico/ProyectoFinal/YearPredictionMSD.txt") #fijamos el directorio de trabajo
save.image(file ='TrainTest.Rdata')
###################### Modelos OLS
setwd("C:/Users/fou-f/Desktop/Third/ComputoEstadistico/ProyectoFinal/YearPredictionMSD.txt") #fijamos el directorio de trabajo
load(file ='TrainTest.Rdata')
modelo.lm <- lm(y~ ., train) #dos segundos
(error.ols.train <- MSE(predict(modelo.lm, train), train$y)) # 91.2564
(error.ols.test <- MSE(predict(modelo.lm, test), test$y)) # 90.4431
summary(modelo.lm) # estimacion pobre R^2 ajustado de 0.2373
# seleccion del mejor subconjunto de variables
tiempo.step1 <- Sys.time()
modelo.lm.step <- step(modelo.lm)
tiempo.lm.step2 <- Sys.time()
(tiempo.lm.step2 - tiempo.step1) #1.11152 hrs
summary(modelo.lm.step)
# elimino 12, 51,54,55,67,79,80,81,86
# pero el modelo sigue siendo pobre R^2 ajustado de 0.2373
error.lm.step.train <- MSE(predict(modelo.lm.step, train), train$y) #91.2573
error.lm.step.test <- MSE(predict(modelo.lm.step, test), test$y) #90.4452
####################
#2 de nov 2018
###################
load(file ='TrainTest.Rdata')
t.subsets <- Sys.time()
set.seed(0)
modelos.subset <- regsubsets(y~ ., data =train, method = 'exhaustive',
                             nvmax = 30, really.big = TRUE)
# 5: 6.09sec, 10: 18.32 mins
modelos.subset
t.subsets2 <- Sys.time()
t.subsets2 - t.subsets
plot(modelos.subset, scale="bic", col=c('green', 'red'))
#####################################
library(glmnet)
tiempo.ridge1 <- Sys.time()
train2 <- model.matrix (y~., train )
y <- train$y
remove(train)
gc()
grid <- 10^seq(-5,100,length =100)
plot(grid)
set.seed(0)
modelo.Ridge <- cv.glmnet(train2, y, alpha =0,
                          lambda =grid, nfolds = 10)
#plot(modelo.Ridge)
(l <- modelo.Ridge$lambda.min)
test2 <- model.matrix(y~. , test)
y.hat.rige <- predict(modelo.Ridge, test2)
tiempo.ridge2 <- Sys.time()
(tiempo.ridge2 - tiempo.ridge1) #1.11152 hrs
summary(modelo.Ridge)
plot(modelo.Ridge) # se tardo con 10-folds
