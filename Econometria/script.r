remove(list=ls())
##################### Packages
library(vars)
library(tseries)
library(forecast)
library(lmtest)
library(seasonal)
###########################  Parametros  ##############3 
# significance
sig <- "5pct" #significancia 
crit <- "AIC(n)" # information criteria
lagmax <- 12 # lag maxesto se supone que es proporcional al log(n) tamanio de muestra
testlag <- 16 # test lag no se que verga sea esto
seas <- NULL ## seas, porque la estacionalidad se quita explicitamente
exog <- NULL # exog, porque consideramos que ninguna variable es exogena 
names_sa <- c("remesas", "antad") #   variables que tienen estacionalidad 
type.var <- c("none", "const", "trend", "both") # type of specification on var: todos los tipos de VAR
vecm.type <- "transitory" # vec error correction model specification NI PUTA IDEA DE QUE SEA ESTO 
ec.det <- c("none", "const", "trend") # equation cointegration slide 43
nahead <- 12 # nahead, NI PUTA IDEA
################### cARGA DE DATOS
path <- "/home/fou/Desktop/Econometria/"
source(paste(path, "functions.r", sep = "")) # FUNCIONES AUXILIARES
data <- read.csv(paste(path, "datos.csv", sep = ""), row.names = 1) #DATA.FRAME COLUMNAS series, registros unidades de tiempo
dat <- ts(data, start = 2001, frequency = 12) #jala chido porque es mensual, MTS
plot(dat)
############ Quita estacionalidad yo diria que habria que quitarsela a todas
for(i in 1:length(names_sa)) #quita seasonality
  dat[,names_sa[i]] <- seas(dat[,names_sa[i]])$series$s11
series <- colnames(dat) #guarda nos nombres de las series
plot(dat)
dat[,series != "tiie28"] <- log(dat[,series != "tiie28"]) #igual de culeras que las de Andres
plot(dat) # el log disminuye la varianza
cor(dat) #para que se fija en la correlacion ?
dat <- dat[,-4] # quita la ifb por su alta correlacion negativa, si esta considerando variables cointegradas
                # y todas son positivas se muevene en la misma direccion (OJO los coeficientes se interpretan de signo contrario)

# test de resagos 
adf <- kpss <- bh <- matrix(0, ncol(dat), 2)
colnames(adf) <- colnames(kpss) <- colnames(bh)<- c("level", "diff")
rownames(adf) <- rownames(kpss) <- rownames(bh) <- colnames(dat)

for(i in 1 : ncol(dat)){
  adf[i, "level"] <- adf.test(dat[,i])$p.value #checa si hay raices unitarias nivel y diferencia
  adf[i, "diff"] <- adf.test(diff(dat[,i]))$p.value # checa el p-value del test 
  kpss[i, "level"] <- kpss.test(dat[,i])$p.value #con dos pinches test 
  kpss[i, "diff"] <- kpss.test(diff(dat[,i]))$p.value
}
adf #l ahipotesis nula es que hay raiz, en el ejemplo todas las putas series son raices unitarias
# LA TEORIA ECONOMISTA DICE QUE LA MAYORIA DE SERIES SON I(1)
list.var <- list() # lista para guardar todas las series estacionales
# VAR(1) es estable si los valores propios de Φ(matriz de coeficientes de VAR(1)) son menores a 1 en el módulo.
lambda <- eigen(t(dat)%*%dat/(nrow(dat)))$values #pues quien sabe porque no centra y saca valores propios
lambda2 <- eigen(t(dat)%*%dat/(nrow(dat)-ncol(dat)))$values #pues quien sabe porque no centra y saca valores propios
lambda-lambda2 # no pues si hay diferencia
plot(lambda[-ncol(dat)]/lambda[-1], type = "l",  ylab = "Ratio") #grafica valores de ratios de val.prop QUIEN SABE PARA QUE 
par(mfrow=c(1,1))
# @ test var CHINGO DE VAR
for(i in 1:length(type.var))
{
  type.i <- type.var[i]   # type var
  # var analysis
  k.i <- VARselect(dat, lag.max = lagmax, type = type.i,
          exogen = exog, season = seas)$selection[crit] #yo solo usare el de Akike
  # models
  list.var[[i]] <- VAR(dat, p = k.i, type = type.i, exogen = exog,
                    season = seas)
}
list.var #lista con todos los modelos
# apply criteria
crit.funct <- sapply(1:length(list.var), function(x) criteria(list.var[[x]]))
crit.funct #CHECARLO Y QUEDARSE CON EL MENOR SC: es el big
colnames(crit.funct) <- type.var
# specification VAR
spe <- which(crit.funct[crit,] == min(crit.funct[crit,]))

# VAR
var1 <- list.var[[spe]]
# lag
k <- var1$p
# @ table granger causality: Esto dijo que no importa
granger <- matrix(0, ncol(dat), 2)
colnames(granger) <- c("Granger", "Instant")
rownames(granger) <- colnames(dat)

for(i in 1 : nrow(granger)){
  test <- causality(var1, colnames(dat)[i])
  granger[i, "Granger"] <- c(test$Granger$p.value)
  granger[i, "Instant"] <- c(test$Instant$p.value) # ni la vimos en clase
}
round(granger, 2)
# johanse list
joha.list <- list()
# logLik anr r.i
LogLik <- r.i <- c()

# @ there is cointegration ?  TEST DE JOHANSEN
for(i in 1:length(ec.det))
{
  # johansen test (cointegration)
  joha <- ca.jo(dat, K = k, ecdet = ec.det[i], spec = vecm.type,
            type = "eigen", dumvar = exog, season = seas)

  # rank of cointegration
  h0 <- max(which(joha@cval[,sig] > joha@teststat)) # DETERMINACION DEL RANGO CONTRASTANDO CON LOS VALORES CRITICOS DEL ESTADISTICO Y EL MUESTRAL 
  # h0 hypothesis
  if(h0 == -Inf)
    h0 <- 0
  # H0 ES EL NUMERO DE NO FACTORES COMUNES
  # rank
  r <- ncol(dat) - h0
  # r es el numero de factores comunes (SI 0<r<ncol(dat)) hay cointegracionn 
  # rank in list
  r.i[i] <- r

  # @ logLik

  # not cointegration
  if(r == 0){
    LogLik[i] <- AIC(VAR(diff(dat), p = k - 1, type = var1$type,
                  exogen = exog[-1,], season = seas))
  }
  # cointegration
  if(r > 0 & r < ncol(dat))
    LogLik[i] <- AIC(vec2var(joha, r = r))[1]

  # stationary var
  if(r == ncol(dat)){
    LogLik[i] <- AIC(VAR(dat, p = k, type = var1$type,
                  exogen = exog, season = seas))
  }
}

# johansen
Which <- which(LogLik == min(LogLik)) #lo que dijo Liz
r <- r.i[Which] # numero de relaciones comunes, FACTORES, hechar choro de esto
ratio <- lambda[-ncol(dat)]/lambda[-1] # PREGUNTARLE A FRANCISCO 
r <- which.max(ratio)
# @ model
# VAR in first differences
if(r == 0){
  exog <- exog[-1, ]
  # model
  model.ols <- VAR(diff(dat), p = k - 1, type = var1$type, exogen = exog,
                season = seas)
  model <- model.ols
  tstat <- NULL
  type.model <- "VAR(first differences)"
}
# Vector Error Correction Model
if(r > 0 & r < ncol(dat)){
  # joha
  joha <- ca.jo(dat, K = k, ecdet = ec.det[Which], spec = vecm.type,
            type = "eigen", dumvar = exog, season = seas)
  # vecm ols
  vecm <- cajorls(joha, r = r)
  model.ols <- vecm$rlm
  # vec2var
  model <- vec2var(joha, r = r)
  # durbin watson
  dw.test <- dwtest(lm(vecm$rlm$model[,1] ~ as.matrix(vecm$rlm$model[,
             (ncol(dat)+1):ncol(vecm$rlm$model)])))
  dw <- round(c(dw.test$statistic, dw.test$p.value), 2)
  # aic criterion (modified)
  K <- (ncol(model$datamat) - ncol(dat))
  aic <- c(2*K -2*logLik(model))
  aicc <- round(aic + (2*K*(K+1))/(nrow(model$datamat) + K - 1), 2)
  # type model
  type.model <- "VEC"
  # R squared
  r2 <- round(summary(model.ols)[[1]]["r.squared"][[1]], 2)
  # summary
  mat.sum <- c(k, r, aicc, r2, dw)
  names(mat.sum) <- c("k","r","aic","r2","dw","dwp")
}
# stationary VAR
if(r == ncol(dat)){
  # both models
  model.ols <- var1
  model <- model.ols

  tstat <- NULL
  type.model <- "VAR"
}

# @ impulse response
irf.model <- irf(model, n.ahead = nahead, ci = 0.9)

# @ tables: estadisticos de las pruebas
if(type.model == "VEC"){
  # coef type.vecm
  Diff <- round(summary(model.ols)[[1]]$coefficients[,c("Estimate",
          "Pr(>|t|)")], 2)
  # equation cointegration
  ec <- round(vecm$beta, 2)  } else{
  # VAR
  Diff <- round(summary(model.ols)$varresult[[1]]$coefficients[,
            c("Estimate", "Pr(>|t|)")], 2)
  # equation cointegration
  ec <- NA
  # pvalues
  pval <- NA
  # durbin watson
  dw.test <- dwtest(summary(model.ols)$varresult[[1]])
  dw <- round(c(dw.test$statistic, dw.test$p.value), 2)
  # aic criterion (modified)
  K <- (ncol(model$datamat) - ncol(dat))
  aic <- c(2*K -2*logLik(model))
  aicc <- round(aic + (2*K*(K+1))/(nrow(model$datamat) + K - 1), 2)
  # R squared
  r2 <- round(summary(model.ols)$varresult[[1]]["r.squared"][[1]], 2)
  # summary
  mat.sum <- c(k, r, aicc, r2, dw)
  names(mat.sum) <- c("k","r","aic","r2","dw","dwp")
}
mat.sum # dwp : p-value del durbin watson Ho:no hay autocorrelacion 
# Prueba F
n <- ncol(dat)
K <- nrow(model.ols$coefficients)
gl <- nrow(dat) - K
la <- k
Fmat <- CoefP <- matrix(0, n, n)
colnames(Fmat) <- colnames(CoefP) <- colnames(dat)
rownames(Fmat) <- rownames(CoefP) <- colnames(dat)
Xreg <- model.ols$model[,-1]
for(j in 1 : n){
  sumnr <- sum(resid(model.ols)[,j]^2)
  for(i in 1 : n){

    names_r <- is.element(colnames(model.ols$model)[-1],
                  paste(colnames(dat)[i], ".dl",1:(la-1), sep = ""))

    model.ols.r <- lm(diff(dat[,j])[-(1:(la-1))] ~
                    as.matrix(Xreg[, -which(names_r)])-1)
                  
    sumr <- sum(resid(model.ols.r)^2)

    CoefP[i,j] <- round(mean(model.ols$coefficients[which(names_r),j]), 4)
    Fmat[i,j] <- round(df(((sumr-sumnr)/(la-1))/(sumnr/(la-1)), la-1, gl), 2)
  }
}
opp <- par(mfrow = c(4, 2))
for(i in 2 : ncol(dat)){
  ts.plot(scale(cbind(dat[,1], dat[,i])), col = c(1, 4))
  title(colnames(dat)[i])
}
par(opp) # interpretacion del factor en esta grafica


opp <- par(mfrow = c(4, 2))
for(i in 2 : ncol(dat)){
  ts.plot(scale(diff(cbind(dat[,1], dat[,i]))), col = c(1, 4))
  title(colnames(dat)[i])
}
par(opp)



