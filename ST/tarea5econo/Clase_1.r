
remove(list = ls())

#
## @ Clase 1: Algunos ejemplos de series de tiempo
#

# PaqueterIa seasonal: desestacionalizaciOn de series de tiempo
library(seasonal)
# PaquterIa forecast: selecciOn autoMatica de series de tiempo
library(forecast)
# PaquterIa portes: simulaCion de modelos VARMAS
library(portes)

# Seleccionamos alguna semilla
set.seed(12345)

# x = AirPassengers datos clAsicos de Box y Jenkins

# RegresiOn lineal entre x y una tendencia determinIstica
mcot <- lm(AirPassengers ~ c(1:length(AirPassengers)))

# A la regresIon anterior le agregamos dummies estacionales
mcos <- lm(AirPassengers ~ seasonaldummy(AirPassengers) +
          c(1:length(AirPassengers)))

# SelecciOn de modelo estacional
seas.model <- seas(AirPassengers)
class(AirPassengers)
str(AirPassengers)
str(s)
#
## @ Tendencia
#

# MCO
ts.plot(cbind(AirPassengers, fitted(mcot)), col = c(1,2), lwd = c(1,2),
  ylab = "Air Passengers", xlab = "")
  
# Serie de tendencia
ts.plot(cbind(AirPassengers, seas.model$series$s12), col = c(1,2), lwd = c(1,2),
  ylab = "Air Passengers", xlab = "")
  
# Serie extraIda como una camina aleatoria
ts.plot(cbind(AirPassengers, fitted(arima(AirPassengers, c(0,1,0)))),
  col = c(1,2), lwd = c(1,2), ylab = "Air Passengers", xlab = "")

# Diferencia
ts.plot(diff(AirPassengers), ylab = "Air Passengers", xlab = "")

#
## @ Estacionalidad
#     

# MCO
ts.plot(cbind(AirPassengers, fitted(mcos)),
  col = c(1,2), lwd = c(1,2), ylab = "Air Passengers", xlab = "")

# Serie desestacionalizada
opp <- par(mfrow = c(2,1))
ts.plot(cbind(AirPassengers, seas.model$series$s11 ), col = c(1,2), lwd = c(1,2),
  ylab = "Air Passengers", xlab = "")
ts.plot(cbind(AirPassengers - seas.model$series$s11), col = 4, lwd = 1,
  ylab = "Air Passengers", xlab = "")
par(opp)

# Diferencia estacional
ts.plot(diff(AirPassengers, 12), ylab = "Air Passengers", xlab = "")

# Diferencia estacional multiplicativa
ts.plot(diff(log(AirPassengers), 12), ylab = "Air Passengers", xlab = "")

# AIC vs Guerrero
aic <- udg(seas.model)$finmode
guerrero <- BoxCox.lambda(AirPassengers, method = c("guerrero"), lower = 0,
                upper = 1)


# X_t(w) 
ts.plot(AirPassengers, ylab = "", xlab = "", ylim = c(-200, 700))
  points(x = 1949:1960, y = AirPassengers[seq(1, length(AirPassengers), 12)],
    col = 4, pch = 14)
  for(i in 1 : length(1949:1960)){
    lines(x = rep(c(1949:1960)[i], 100),  
    seq(AirPassengers[seq(1, length(AirPassengers), 12)][i] - 
    2*sd(AirPassengers), AirPassengers[seq(1, length(AirPassengers), 12)][i] + 
    2*sd(AirPassengers), length.out = 100), col = 4)
    points(x = c(1949:1960)[i], AirPassengers[seq(1, 
      length(AirPassengers), 12)][i] - 
    2*sd(AirPassengers), col = "red", pch = "-", cex = 3) 
    points(x = c(1949:1960)[i], y = AirPassengers[seq(1, 
      length(AirPassengers), 12)][i] + 
    2*sd(AirPassengers), col = "red", pch = "-", cex = 3)
  }
  
# Suma acumulada
x <- rnorm(200)
opp <- par(mfrow = c(2,1))
  ts.plot(x, ylab = "", xlab = "")
  ts.plot(cumsum(x), ylab = "", xlab = "")
par(opp)
  
# AlgUn valor de phi de theta
phi <- 0
theta <- 0
Tt <- 200
phis <- 0
thetas <- 0
period <- NULL
if(phis != 0 | thetas != 0)
  period <- 12

# Serie de tiempo simulada
ts.x <- varima.sim(model=(list(ar = phi, ma = theta, sar = phis, sma = thetas, 
        period = period)), n = Tt, k = 1, inov.dist = "Gaussian")
        
ts.plot(ts.x, ylab = "", xlab = "")
abline(h = mean(ts.x), col = 2, lty = 1)
        
# ACF y PAC
opp <- par(mfrow = c(2,1))
  acf(ts.x, main = "")
  pacf(ts.x, main = "")
par(opp)

# PredicciOn
plot(forecast(auto.arima(AirPassengers)))




