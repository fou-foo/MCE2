setwd("C:/Users/fou-f/Desktop/Third/ComputoEstadistico/ProyectoFinal/YearPredictionMSD.txt")
dir()
t1 <- Sys.time()
songs <- read.csv(dir(), stringsAsFactors = FALSE, header = FALSE)
t1 <- Sys.time() - t1
t1 # lectura 1.61231 mins
names(songs)
#####################
# graficas de primer avance 
#####################
library(ggplot2)
muestra <- t(songs[1:9,2:91])
muestra <- as.data.frame(muestra)
names(muestra) <- paste0('Cancion', 1:9)
muestra$index <- 1:90
ggplot(muestra, aes(x=index, y=Cancion2)) + geom_line(aes(colour=I('#4B2E83'))) +
  theme_minimal() + xlab('')+ylab('') +
 geom_line(data=muestra, aes(y=Cancion1, colour=I('orange')))+
 geom_line(data=muestra, aes(y=Cancion3, colour=I('magenta')))+
  geom_line(data=muestra, aes(y=Cancion4, colour=I('blue')))+
  geom_line(data=muestra, aes(y=Cancion5, colour=I('green4')))+
  ggtitle('5 canciones de 2001')
m <- table(songs$V1)
m <- as.data.frame(m)
m$Var1 <- as.numeric(as.character(m$Var1))
range(m$Var1)

ggplot(m, aes(x=Var1, y=Freq)) + geom_line(aes(colour=I('#4B2E83'))) +
  theme_minimal() + xlab('')+ylab('') + 
  ggtitle('No. de canciones por año en el dataset')
n <- dim(songs)[1]
condicionamiento <- songs[,2:91 ]
condicionamiento <- scale(condicionamiento)
condicionamiento <- as.matrix(condicionamiento)
dim(condicionamiento)
condicionamiento <- t(condicionamiento)%*%condicionamiento
inversa <- solve(condicionamiento)
plot(m$Freq/sum(m$Freq))
t4 <- Sys.time()
lm <- lm(V1~. , data = songs)
t4 <- Sys.time() -t4
t4
t5 <- Sys.time()
lm <- step(lm)
t5 <- Sys.time() -t5
t5


y.hat <- predict(lm, songs)
correctos <- round(y.hat)==songs$V1
sum(correctos)/length(correctos)



train <- 1:463715
sapply(songs, class)
songs <- na.omit(songs)
515345
object.size(songs)
t2 <- Sys.time()
cor.m <- cor(songs[,2:91])
t2 <- Sys.time() - t2 
t2 # 3.937574 secs
t3 <- Sys.time()
pca <- princomp(songs[, 2:91])
t3 <- Sys.time() - t3
t3 #21.92173 secs
str(pca)
