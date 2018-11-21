
remove(list = ls())

set.seed(12345)

library(tseries)
library(urca)

# generamos series de tiempo cointegradas

# path 
path <- "C:/Users/FCO/Desktop/Clase_3_ST_Econometría/"

# some functions
source(paste(path, "functions.r", sep = ""))

# series de tiempo
N <- 2
# longitud temporal
T <- 200
# factores comunes
r <- 1

# factor comUn
Ft <- matrix(cumsum(rnorm(T*r)), ncol = r)
# matriz de cargas
P <-  matrix(rnorm(N*r), N, r)
# error estacionario
et <- matrix(rnorm(T*N), T, N)
# series simuladas
Yt <- t(P%*%t(Ft)) + et

opp <- par(mfrow = c(2, 1))
  ts.plot(scale(Yt), ylab = "", xlab = "", col = c(2, 4))
  title("simulated cointegrated time series")
  ts.plot(resid(lm(Yt[,1] ~ Yt[,2])), ylab = "", xlab = "")
  title("residual")
par(opp)

# no-cointegrated
P <- matrix(c(1,0))
Yt <- t(P%*%t(Ft)) + et

opp <- par(mfrow = c(2, 1))
  ts.plot(scale(Yt), ylab = "", xlab = "", col = c(2, 4))
  title("simulated no-cointegrated time series")
  ts.plot(resid(lm(Yt[,1] ~ Yt[,2])), ylab = "", xlab = "")
  title("residual")
par(opp)

# Cointegration
dat <- read.csv(paste(path, "IPI_MEX_USA.csv", sep = ""), row.names = 1)

# as ts
dat <- ts(dat, start = 1993, frequency = 12)

# log
dat <- log(dat)

ts.plot(dat, col = c(2, 4), ylab = "", xlab = "")
legend("topleft", c("IPIMEX", "IPIUSA"), col = c(2, 4), lty = 1)

# pruebas ADF
adf.test(dat[,"IPIMEX"])
adf.test(dat[,"IPIUSA"])

adf.test(diff(dat[,"IPIMEX"]))
adf.test(diff(dat[,"IPIUSA"]))  lag

# regression
regre <- lm(dat[,"IPIMEX"] ~ dat[,"IPIUSA"])

summary(regre)

# plot of resids
ts.plot(ts(resid(regre), start = 1993, frequency = 12), col = 1, ylab = "", 
  xlab = "")

adf(resid(regre), "none")

# model without lags
mce <- MCE(regre, lags = 0)
summary(mce)

# Phillips-Ouliaris procedure
summary(ca.po(dat, demean = "const", lag = "short", type = "Pu"))
summary(ca.po(dat, demean = "const", lag = "short", type = "Pz"))

eigen(cov(dat))

eigen(cov(dat))$values/sum(eigen(cov(dat))$values)


# test from 1 to 12 lags
test.lb <- c()
for(i in 1 : 12)
  test.lb[i] <- Box.test(resid(mce), lag = i,type = "Ljung-Box")$p.value

# test of cointegrated system  
R <- 500
mat_info <- matrix(0, R, 2)
colnames(mat_info) <- c("p.value", "ce")

for(i in 1 : R){
  # factor comUn
  Ft <- matrix(cumsum(rnorm(T*r)), ncol = r)
  # matriz de cargas
  P <-  matrix(rnorm(N*r), N, r)
  # error estacionario
  et <- matrix(rnorm(T*N), T, N)
  # series simuladas
  Yt <- t(P%*%t(Ft)) + et

  # long-run regression
  regre.i <- lm(Yt[,1] ~ Yt[,2])
  # residual
  vt <- resid(regre.i)
  # adf test
  mat_info[i, "p.value"] <- adf(vt, "none")$p.value
  # error correction model
  mat_info[i, "ce"]  <- coef(MCE(regre.i))[2]
}

hist(mat_info[,"ce"], col = "blue", main = "", xlab = "")
abline(v = -1, col = "red")
abline(v = 0, col = "red")

# stastics
nocoint <- mat_info[,"p.value"] > 0.05
sum(nocoint)/R
quantile(mat_info[nocoint, "p.value"])

# Ejercicio en clase
pe <- read.csv(paste(path, "pass-through.csv", sep = ""), row.names = 1)

# Graficar las series
colnames(pe)
ts.plot(scale(pe), col = c(2, 4))

# Logaritmo
pe <- log(pe)

# Plot de los precios
opp <- par(mfrow = c(2, 1))
acf(pe[,1])
acf(diff(pe[,1]))
par(opp)

# Plot del exchange
opp <- par(mfrow = c(2, 1))
acf(pe[,2])
acf(diff(pe[,2]))
par(opp)

# ADF para precios
adf.test(pe[,1])
adf.test(diff(pe[,1]))

# GrAfica en el tiempo
ts.plot(cbind(diff(pe[,1]), diff(pe[,2])), col = c(2, 4))

# ADF para el tipo de cambio
adf.test(pe[,2])
adf.test(diff(pe[,2]))

# RegresiOn
regre <- lm(pe[,1] ~ pe[,2])

# residuals
u <- resid(regre)
ts.plot(u)

# test
adf(u, "none")

# MCE 
mce <- MCE(regre, 2)   
summary(mce)

# plot en el tiempo del residual del MCE
ts.plot(resid(mce))

# Pruebas para validar si es ruido
library(lmtest)

# test from 1 to 12 lags
test.lb <- c()
for(i in 1 : 12)
  test.lb[i] <- Box.test(resid(mce), lag = i,type = "Ljung-Box")$p.value
  
acf(resid(mce))                                             
bgtest(mce)
