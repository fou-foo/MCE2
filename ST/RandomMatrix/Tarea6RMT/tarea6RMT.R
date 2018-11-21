setwd("~/Desktop/Econometria")
################## librerias #############
library(BatchGetSymbols) # obtencion de series
library(reshape2)
library(lubridate)     #manejo de fechas
library(quantmod) # descarga de S&P500
library(dplyr)
library(tseries)
library(parallel)
library(readr) #lectura rapida
library(seasonal)
library(RMTstat)
library(pls)
################################
SMAPE <- function(y.hat, y)
{
  # Calculo de raiz cuadrada de error cuadratico medio
  return( sum( abs(y.hat-y) /(abs(y) + abs(y.hat))   )*(100/length(y)) )
}
################ obtencion de datos ############
diferencia <- today()- ymd('2008-01-01')
fecha <- as.numeric(diferencia)
today()-days(fecha) #checar fecha de inicio
first.date <- Sys.Date() - fecha #actualización en tiempo real 
last.date <- Sys.Date() 
freq.data <- 'weekly'  # frecuencia semanal 

# set tickers
#Componentes_Investing_com_United_States_500 <- read_csv("Componentes Investing.com United States 500.csv") # previanmente descargamos de https://mx.investing.com/indices/investing.com-us-500-components los nombres de las empresas
#tickers <- Componentes_Investing_com_United_States_500$Símbolo
#companias <- BatchGetSymbols(tickers = tickers, 
#                         first.date = first.date,
#                         last.date = last.date, 
#                         freq.data = freq.data,
#                         do.complete.data = FALSE, #sihay nulos los descartamos
#                         cache.folder = file.path(tempdir(),'BGS_Cache')) # cache in tempdir()
#dim(companias$df.tickers)
#a <- companias$df.tickers
#a <- subset(a, ticker=='A')
#sapply(a,class )
#a <- a[ a$price.open !=a$price.high,  ]
#a <- a[ a$price.low !=a$price.high,  ]
#a <- a[ a$price.low !=a$price.close ,  ]
#a <- a[ a$price.adjusted !=a$price.close ,  ]
#a <- unique(as.data.frame(a)) #identificamos la variable de interes
#serie <- companias$df.tickers
#names(serie)
#class(serie) <- 'data.frame'
#serie %>% select(ticker, ref.date, price.close ) -> serie# era open o close ?
SP500 <- BatchGetSymbols(tickers = "^GSPC", 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         do.complete.data = FALSE, #sihay nulos los descartamos
                         cache.folder = file.path(tempdir(),'BGS_Cache')) 
SP500 <- as.data.frame(SP500$df.tickers)[, c(2,7)]
SP500 <- na.omit(SP500)
#
#serie2 <- dcast(serie, ref.date ~ ticker, value.var = 'price.close' )
#write_csv(serie2, path='serie2.csv')
serie2 <- read_csv( file='serie2.csv')
#View(head(serie2))
serie3 <- apply(serie2, 2, function(x) sum(is.na(x))) # identificamos series problematicas
#plot(serie3)
#summary(serie3)
#table(serie3)
malas <- which(serie3 > 7 )
serie4 <- serie2[,  !(names(serie2) %in% names(malas)) ]
#class(serie4)
serie5 <- na.omit(serie4)
#sapply(serie4, class)
class(serie5) <- 'data.frame'
#View(head(serie5))
serie5$ref.date <- as.Date(serie5$ref.date)
#class(serie5$ref.date)
serie.cruda <- serie5
#View(head(serie5))
par(mfrow=c(3,3))
#inspeccion visual 
for(i in seq(2,446, by=1))
{
  s <- ts(serie5[, i], start = c(2008,1), frequency = 54)
  class(s)
  if(i %% 20==0) plot(s, main=as.character(names(serie5)[i]), xlab='series brutas')
  #s.non <- seas(s)
  #points(s.non$series$s11, col='red', type='l' )
  #if(i %% 9==0) a <- scan()
}
par(mfrow=c(1,1))
source(file='functions.r')
# quitamos tendencia 
quita.tendencia <- quita.tendencia.init(inicio= c(2008, 1),frecuencia = 12 )
series <- mclapply(FUN=quita.tendencia, serie5[,2:dim(serie5)[2]], mc.cores = 4)
series <- as.data.frame(series)
series$time <- serie5$ref.date
class(series)
head(series)
names(series) 
series.ex <- log(series[, -447]) # aplicamos logaritmo para estabilizar la varianza
series.ex$time <- series$time
par(mfrow=c(3,3))
#inspeccion visual sin seasonality
for(i in 1:(dim(series)[2]-1))
{
  s <- ts(series.ex[, i], start = c(2008,1), frequency = 54)
  if(i %% 20==0) plot(s, main=as.character(names(series.ex)[i]), xlab='log(serie)')
  #if(i %% 9==0) a <- scan()
}
par(mfrow=c(1,1))
# resagos
resagos <- matrix(rep(0, (dim(series.ex)[2]-1)*2), ncol=2)
dim(resagos)
colnames(resagos) <- c('adf', 'adf.propio')
for(i in 1:(dim(series.ex)[2]-1))
{
  resagos[i, 'adf'] <- adf.test(ts(series.ex[,i]))$parameter
  resagos[i, 'adf.propio'] <- adf.test.custom(ts(series.ex[,i]))
  print(i)
}
#resagos
apply(resagos, 2, mean)
#plot(resagos) # como casi todo es 8 le hacemos caso a adf.test
#plot(density(resagos[,2]))
resago <- 8
# diferenciar
serie.chida <- apply(series.ex[,-dim(series.ex)[2]], 2, function(x) diff(x, lag=resago))
serie.chida <- as.data.frame(serie.chida)
serie.chida$time <- serie5$ref.date[-(1:resago)]
# test de raices
p.value <- rep(5, dim(serie.chida)[2]-1)
for(i in 1:(dim(serie.chida)[2]-1))
{
  p.value[i] <- adf.test(serie.chida[,i ])$p.value
}
#hist(p.value) # segun ya son estacionales
#p.value # porque son menores a 0.05
# visualizar
par(mfrow=c(3,3))
#inspeccion visual sin tendencia
for(i in 1:(dim(serie.chida)[2]-1))
{
  s <- ts(serie.chida[, i], start = c(2008,2), frequency = 54) #manual
  if(i %% 9==0) plot(s, main=as.character(names(serie.chida)[i]), xlab='resago:8')
  #if(i %% 9==0) a <- scan()
}
par(mfrow=c(1, 1))
# visualizacion conjunta
library(ggplot2)
a <- melt(serie.chida, id='time' )
names(a)
ggplot(a, aes(x=time, y=value)) + geom_line(aes(colour=variable), alpha=0.2) +
  guides(colour=FALSE)+ theme_minimal()+ggtitle('Series estacionales y ergodicas')
SP500 <- BatchGetSymbols(tickers = "^GSPC", 
                first.date = first.date,
                last.date = last.date, 
                freq.data = freq.data,
                do.complete.data = FALSE, #sihay nulos los descartamos
                cache.folder = file.path(tempdir(),'BGS_Cache')) 
SP500 <- as.data.frame(SP500$df.tickers)[, c(2,7)]
sp.500 <- ts(SP500$price.close, start = c(2008, 1), frequency = 54)
plot.ts(sp.500)
##################################
# interseccion de series
#################################
names(SP500) <- c('time', 'price.close.SP')
m <- merge(serie.chida, SP500, by.x='time', by.y='time')
colnames(m) <- c(colnames(m)[1:(dim(m)[2]-1)], 'y')
tiempo <- m$time 
#tiempo
index <- which(tiempo>ymd('2018-01-01'))
m$time <- NULL
train <- m[-index, ]
test <- m[index,]
vals <- eigen(cor(scale(train[, -dim(train)[2]])))$values
# minisimulacion
set.seed(0)
#names(train)
r <- (dim(train)[2]-1)/dim(train)[1]
x <- c(0,seq((1-r**.5)**2, (1+r**.5)**2, by=0.1))
plot(x,dmp(x, ndf=dim(train)[1], pdim=dim(train)[2]-1 ), col='purple', type='l',
     ylab='', main='Distribución limite M-P')
rug(vals, col='red')
muestras <- dim(m)[1]*10
set.seed(0)
(limite <- mean(rmp(muestras, ndf=dim(train)[1], pdim=dim(train)[2]-1 )))
abline(v=limite)
vals <- vals[vals>limite]
(RMT.cota <- length(vals)) #60
integrate(function(x)dmp(x, ndf=dim(train)[1], pdim=dim(train)[2]-1 ), 0,limite )
################# resultado con RMT
modelo.pcr.rmt <- pcr(y~., data=train, ncomp=RMT.cota)
summary(modelo.pcr.rmt)
y.hat.test <- predict(modelo.pcr.rmt, ncomp=RMT.cota , newdata = test)
SMAPE(test$y, as.numeric(y.hat.test)) #100-2.654927
res1 <- data.frame(y=test$y, y.hat=as.numeric(y.hat.test),
                   time=tiempo[-(1:(dim(train)[1])) ])
ggplot(res1, aes(x=time, y=y))+geom_line(color=I('purple')) + theme_minimal()+
  geom_line(data=res1, aes(x=time, y=y.hat),color=I('orange'))
################# resultado con 80 vars
modelo.pcr.rmt <- pcr(y~., data=train, ncomp=dim(train)[2]-1)
summary(modelo.pcr.rmt)
y.hat.test <- predict(modelo.pcr.rmt, ncomp=66 , newdata = test)
SMAPE(test$y, as.numeric(y.hat.test)) #100- 5.178365
res1 <- data.frame(y=test$y, y.hat=as.numeric(y.hat.test),
                   time=tiempo[-(1:dim(train)[1])])
ggplot(res1, aes(x=time, y=y))+geom_line(color=I('purple')) + theme_minimal()+
  geom_line(data=res1, aes(x=time, y=y.hat),color=I('orange'))
################# resultado con PLS
modelo.pcr.rmt <- plsr(y~., data=train, ncomp=RMT.cota)
summary(modelo.pcr.rmt)
error <- rep(0, RMT.cota)
for (i in 1:RMT.cota){
y.hat.test <- predict(modelo.pcr.rmt, ncomp=i , newdata = test)
error[i] <- SMAPE(test$y, as.numeric(y.hat.test)) #100-19.15192
}
which.min(error)
y.hat.test <- predict(modelo.pcr.rmt, ncomp=which.min(error) , newdata = test)
SMAPE(test$y, as.numeric(y.hat.test)) #100- 0.7663043
res1 <- data.frame(y=test$y, y.hat=as.numeric(y.hat.test),
                   time=tiempo[-(1:dim(train)[1])])
ggplot(res1, aes(x=time, y=y))+geom_line(color=I('purple')) + theme_minimal()+
  geom_line(data=res1, aes(x=time, y=y.hat),color=I('orange'))
# estimar SIN HACER ESTACIONARIAS, ESTIMAR CON SP500 RECORTADO
serie.cruda <- na.omit(serie.cruda)
#View(head(serie.cruda))
colnames(serie.cruda) <- c('time', colnames(serie.cruda)[-1])
colnames(SP500) <- c('time', 'y')
m <- merge(serie.cruda, SP500)
colnames(m)
tiempo <- m$time 
tiempo
index <- which(tiempo>ymd('2018-01-01'))
m$time <- NULL
train <- m[-index, ]
test <- m[index,]
vals <- eigen(cor(scale(train[, -dim(train)[2]])))$values
# minisimulacion
set.seed(0)
r <- (dim(train)[2]-1)/dim(train)[1]
x <- c(0,seq((1-r**.5)**2, (1+r**.5)**2, by=0.1), 4)
plot(x,dmp(x, ndf=dim(train)[1]-1, pdim=dim(train)[2] ), col='purple', type='l')
rug(vals, col='red')
muestras <- dim(m)[1]*10
set.seed(0)
(limite <- mean(rmp(muestras, ndf=dim(train)[1], pdim=dim(train)[2]-1 )))
abline(v=limite)
vals <- vals[vals>limite]
(RMT.cota.2 <- length(vals))
integrate(function(x)dmp(x, ndf=dim(train)[1]-1, pdim=dim(train)[2] ), 0,limite )
################# resultado con RMT
modelo.pcr.rmt <- pcr(y~., data=train, ncomp=RMT.cota)
summary(modelo.pcr.rmt)
y.hat.test <- predict(modelo.pcr.rmt, ncomp=RMT.cota , newdata = test)
SMAPE(test$y, as.numeric(y.hat.test)) #100- 0.9553613
res1 <- data.frame(y=test$y, y.hat=as.numeric(y.hat.test),
                   time=tiempo[-(1:dim(train)[1])])
library(ggplot2)
ggplot(res1, aes(x=time, y=y))+geom_line(color=I('purple')) + theme_minimal()+
  geom_line(data=res1, aes(x=time, y=y.hat),color=I('orange'))
################# resultado con 80 vars
modelo.pcr.rmt <- pcr(y~., data=train, ncomp=dim(train)[2]-1)
summary(modelo.pcr.rmt)
y.hat.test <- predict(modelo.pcr.rmt, ncomp=1 , newdata = test)
SMAPE(test$y, as.numeric(y.hat.test)) #100-0.9330371
res1 <- data.frame(y=test$y, y.hat=as.numeric(y.hat.test),
                   time=tiempo[-(1:dim(train)[1])])
ggplot(res1, aes(x=time, y=y))+geom_line(color=I('purple')) + theme_minimal()+
  geom_line(data=res1, aes(x=time, y=y.hat),color=I('orange'))
################# resultado con PLS
modelo.pcr.rmt <- plsr(y~., data=train, ncomp=RMT.cota)
summary(modelo.pcr.rmt)
error <- rep(0, RMT.cota)
for (i in 1:RMT.cota){
  y.hat.test <- predict(modelo.pcr.rmt, ncomp=i , newdata = test)
  error[i] <- SMAPE(test$y, as.numeric(y.hat.test)) #100-19.15192
}
which.min(error)
y.hat.test <- predict(modelo.pcr.rmt, ncomp=which.min(error) , newdata = test)
SMAPE(test$y, as.numeric(y.hat.test)) #100-0.5049128

res1 <- data.frame(y=test$y, y.hat=as.numeric(y.hat.test),
                   time=tiempo[-(1:dim(train)[1])])
ggplot(res1, aes(x=time, y=y))+geom_line(color=I('purple')) + theme_minimal()+
  geom_line(data=res1, aes(x=time, y=y.hat),color=I('orange'))




















# test de cambios estructurales para sP500 y recortar series
adf.test(sp.500) #  pues que no hay raices unitarias
plot(as.numeric(sp.500), xlim=c(0,100))
Busetti.Harvey(sp.500, k=1, posicion = 60 )
plot(sp.500)
sp.500
sp.500.recortado <- window(sp.500, start=c(2009,10) )
plot(sp.500.recortado)
