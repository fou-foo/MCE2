setwd("C:/Users/fou-f/Desktop/Third/ST/Tarea2")
dir()
PIB <- read.csv('BIE_BIE20180916230447.csv', skip = 2)
PIB <- PIB[,1:2]
names(PIB) <- c('Periodo', 'PIB,a.precios.de.mercado') 
PIB <- ts(PIB$`PIB,a.precios.de.mercado`, start = 1993, frequency = 4)
PIB <- na.omit(PIB)
ts.plot(PIB, ylim=c(0,20000000), xlab='', ylab='Millones de pesos', main='PIB México base 1993, trimestral')
adf.test(unlist(PIB)) # raiz unitaria
adf.test(diff(unlist(PIB))) #no hay raiz
ts.plot(diff(PIB), xlab='', ylab='Millones de pesos', main='primeras diferencias PIB México')
adf.test.custom(unlist(PIB), option='c')
adf.test(diff(unlist(PIB), lag=4)) #segun el criterio BIC hay raiz 
adf.test(diff(unlist(PIB), lag=5)) # tal ves se sobreajusta
############### saltos estructurales
ts.plot(PIB[50:102], ylim=c(0,20000000), xlab='', ylab='Millones de pesos', main='PIB México base 1993, trimestral')
#posible cambio en 64
Busetti.Harvey(unlist(PIB), option='c', k = 1, posicion = 64, p=0.95)
ts.plot(PIB[1:20], ylim=c(0,20000000), xlab='', ylab='Millones de pesos', main='PIB México base 1993, trimestral')
#posible cambio en 7
Busetti.Harvey(unlist(PIB), option='c', k = 1, posicion = 7, p=0.95)
Busetti.Harvey(unlist(PIB), option='c', k = 2, posicion = c(7,64), p=0.95)
#
Busetti.Harvey(unlist(PIB), option='both', k = 1, posicion = 7, p=0.95)
Busetti.Harvey(unlist(PIB), option='both', k = 2, posicion = c(7,64), p=0.95)
PIB.log <- log(PIB)
acf(PIB.log)
pacf(PIB.log)
acf(diff(PIB.log, lag=1))
pacf(diff(PIB.log, lag=1))
#############
############
modelo <- arima(PIB, order = c(1, 0, 2), 
                seasonal=list(order=c(0, 1, 1), period=4))
summary(modelo)
res <- modelo$residuals
acf(res)
pacf(res)
hist(res)
mean(res)
ks.test(res, 'pnorm', mean(res), sd(res))
ks.test(rnorm(10**6),'pnorm')
shapiro.test(res)
shapiro.test(rnorm(5000))
qqnorm(res)
qqline(res)
BIC(modelo)
summary(lm(res[-102]~ diff(res)))
#########
