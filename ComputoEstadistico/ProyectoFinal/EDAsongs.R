###################### Packages necesarios
library(ggplot2) #graficos
library(easyGgplot2)
library(xtable)
library(ggcorrplot)
###################### Funciones
###########################
setwd("C:/Users/fou-f/Desktop/Third/ComputoEstadistico/ProyectoFinal/YearPredictionMSD.txt") #fijamos el directorio de trabajo
dir()
t1 <- Sys.time()
songs <- read.csv('YearPredictionMSD.txt', stringsAsFactors = FALSE, header = FALSE)
t1 <- Sys.time() - t1
t1 # lectura 1.21231 mins
names(songs) <- c('y', paste0('X', 1:90))
########################### visualizaciones
# distribución de la variable de respuesta
anos <- data.frame(y=songs$y)
anos.tabla <- as.data.frame(table(anos))
anos.tabla$freq.por <- round(anos.tabla$Freq/ sum(anos.tabla$Freq), 3)
anos.tabla # porcentaje de canciones por año
a <- cbind(anos.tabla[1:30, ], anos.tabla[31:60, ], anos.tabla[61:90, ])
xtable(a)
ggplot(subset(anos, as.numeric(y) >= 1978), aes(x=factor(y), fill= I('purple')) ) + geom_bar() +
    theme_minimal() + xlab('Año')+ylab('Frecuencia') +
    ggtitle('No. de canciones por año en el conjunto de datos') #visualizacion de los años con más de 0.005% de ocurrencia
datos <- scale(songs[, 2:91])
dim(datos)
datos <- as.data.frame(datos)
names(datos)
datos$y <- songs$y
datos <- as.data.frame(datos)
# intento de visualizacion de grupos usando pca
set.seed(0)
muestra <- datos[sample(1:dim(songs)[1], round(dim(songs)[1]/1000)), ] #muestra aleatoria del 10%
names(muestra)
pca <- prcomp(x=muestra[, 1:90], retx = TRUE)
summary(pca) #las dos primeras componentes explican 19% de la varianza
pca <- as.data.frame(pca$x[, 1:2])
pca$Año <- factor(muestra$y)
p1 <- ggplot(pca, aes(PC1, PC2, color=Año)) + geom_point(alpha=1/2) +
    theme_minimal() + xlab('PC1')+ylab('PC2')+
    ggtitle('Proyección de las canciones en dos dimensiones, 0.1% del total') #visualizacion de los años con más de 0.005% de ocurrencia
set.seed(0)
muestra <- datos[sample(1:dim(songs)[1], round(dim(songs)[1]/100)), ] #muestra aleatoria del 10%
names(muestra)
pca <- prcomp(x=muestra[, 1:90], retx = TRUE)
summary(pca) #las dos primeras componentes explican 19% de la varianza
pca <- as.data.frame(pca$x[, 1:2])
pca$Año <- factor(muestra$y)
p2 <- ggplot(pca, aes(PC1, PC2, color=Año)) + geom_point(alpha=1/4) +
    theme_minimal() + xlab('PC1')+ylab('PC2')+
    ggtitle('Proyección de las canciones en dos dimensiones 1% del total') #visualizacion de los años con más de 0.005% de ocurrencia
rm(pca)
gc()
ggplot2.multiplot(p1, p2, cols=1)
################################
summary(datos$y)
save.image('Songs.Rdata')
setwd("C:/Users/fou-f/Desktop/Third/ComputoEstadistico/ProyectoFinal/YearPredictionMSD.txt") #fijamos el directorio de trabajo
load('Songs.Rdata')
##################
#datos$y <- scale(datos$y)
correlacion <- cor(datos)
colnames(correlacion) <- c(paste0('X', 1:90), 'Y')
############
a <- ggcorrplot(correlacion, title = "Correlaciones entre las variables",
           colors = c("#45974D", "#DAEADB", "#6D2A83"),  type = "upper",
           outline.color = rgb(1,1,1,0), method = 'square',
           show.diag =FALSE, tl.cex=c((1:9)/10,11))

a #correlograma
var(datos$y)
plot(correlacion[, 91], pch=20, col='#F72600', axis=NULL,  xaxt = "n",
     ylab='', main='Correlacion de la respuesta Y con las 90 variables')
axis(1, at=1:91, labels=names(correlacion[,91]) )
abline(h=0, col='purple3',lty=2, lwd = 2 )
##############################
##  Escalamiento multidimensional
    # convertimos a disimilaridades los datos estandarizados
gc()
mds <- songs[, 2:91]
mds <- 1/(abs(mds)+1)
# convertimos a disimilaridades
library(smacof)
color <- datos$y
datos$y <- NULL
dimensiones.mds <- 1:3
set.seed(0)
#muestra de tamaño 1%
index.1 <- sample(1:dim(datos)[1], round(dim(datos)[1]/100))
tiempo1.mds <- Sys.time()
mds <- unfolding( mds[index.1, ], ndim=90 )
tiempo2.mds <- Sys.time() #tres minutos
tiempo2.mds - tiempo1.mds
head(summary(mds))
plot(mds)
plot(mds, type = "p", label.conf = list(label = TRUE,
            col = "darkgray"), pch = 25, col = "red")
