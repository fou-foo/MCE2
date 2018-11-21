
remove(list=ls())


require(gplots)
library(xlsReadWrite)
library(vars)
library(pls)
library(forecast)
library(mvtnorm)
library(ks)



    
regiones <- c("Frontera.norte", "Noroeste", "Noreste", "Centro.norte", "Centro.sur", "Sur", "Mexico", "Nacional")


region<-"Nacional"

variable <- "Precios"
mes<-"oct"    #es el nombre que se ya se empleo en carpetas para el mes actual
mes.pasado<-"sep"  #es el nombre empleado para el mes anterior


length.fore <- 12
conf.const <- TRUE
lag.max <- 12
c.sig <- 0.10
show.data <- 48
seas.max <- 12
length.test <- 6
n.try <- 6
restrict <- FALSE
forecast.average <- FALSE
type.model <- c("none", "const", "trend")
objective <- 4

#regiones <- c("Frontera.norte", "Noroeste", "Noreste", "Centro.norte", "Centro.sur", "Sur", "Mexico")
exog.mat <- NULL
exog.new <- matrix(rep(0, length.fore))
colnames(exog.new) <- c("Outliers")

#dt.file <- "C:/Documents and Settings/Fco/Desktop/inflacion/"
#dt.file <- "C:/Documents and Settings/Fco/Desktop/modelo_inflacion/"
#dt.file <- "C:/cimat_SHS_compu/CIMAT/inflacion/inflacion/modelo_inflacion/
dt.file <-"C:/Users/kiko/Desktop/Desktop/Desktop_4112012/modelo_inflacion/"


source(paste(dt.file, "model_functions.r", sep =""))

  
dt.econ <- paste(dt.file, "Econ.Data", sep = "")
dt.precios <- paste(dt.file, variable, sep = "")

costos <- read.csv(paste(dt.file, "Costos.csv", sep = ""), row.names = 1)
monetario <- read.csv(paste(dt.file, "Monetario.csv", sep = ""), row.names = 1)
demanda <- read.csv(paste(dt.file, "Demanda.csv", sep = ""), row.names = 1)
precios <- read.csv(paste(dt.file, "Precios.csv", sep = ""), row.names = 1)

regiones <- colnames(precios)

price <- precios[,region,drop = FALSE]

costos.pls <- factor.pls(price, costos)
monetario.pls <- factor.pls(price, monetario)
demanda.pls <- factor.pls(price, demanda)

endog.mat <- cbind(precios[,region], costos.pls$score.pls,
              monetario.pls$score.pls, demanda.pls$score.pls)
colnames(endog.mat) <- c(region, "Costos", "Monetario", "Demanda")

load.mat.costos <- costos.pls$load.mat.w
load.mat.monetario <- monetario.pls$load.mat.w
load.mat.demanda <- demanda.pls$load.mat.w

#png(paste(dt.file,"graficos_mar/Relacion indicadores1/Relacion indicadores1-",region,".png",sep=""),
#width=650)
  ts.plot(ts(scale(endog.mat), start = 2000, frequency = 12),
    col = c(1 : ncol(endog.mat)), lwd = c(3, rep(1, ncol(endog.mat) - 1)))
  legend("topleft", colnames(endog.mat), col = c(1 : ncol(endog.mat)),
    lwd = c(3, rep(1, ncol(endog.mat) - 1)))
  title(paste("Relaciones entre indicadores de mercado y el INPC region-",region, sep=""))
#dev.off()
      
## Formando especificacion de todas las combinaciones posibles de modelos
c.num.cols <- ncol(endog.mat)
n <- nrow(endog.mat)
ec.det <- c("none", "const", "trend")
idx.matrix  <- as.data.frame(matrix(0, (lag.max - 1)*(seas.max - 1)*length(ec.det), 3))
colnames(idx.matrix) <- c("seas", "lag", "ec.det")
idx.matrix[,"seas"] <- rep(2:seas.max, each = (length(ec.det))*(lag.max - 1))
idx.matrix[,"lag"] <- rep(c(2:lag.max), nrow(idx.matrix)/(lag.max - 1))
idx.matrix[,"ec.det"] <- rep(rep(ec.det, each = lag.max - 1), seas.max - 1)

## Variables que cargan los elmentos basicos del modelo de pronostico
crit.stat <- c("MAPE", "MSE",
              "THEIL", "BIAS", "LOG.LIK",
              "CONF", "R2.ADJ", "COMMON.TREND")

idx.endog <- matrix(NA, nrow(idx.matrix), length(crit.stat))
colnames(idx.endog) <- crit.stat

#which(idx.endog[,"MAPE"] == min(idx.endog[,"MAPE"], na.rm = TRUE))

values <-  c("forecast", "vec", "ce", "w", "common.trends", "common.trend", "statistic", "try.forecast")
mape <- mse <- theil <- bias <- log.lik <- c.i <- r.2 <- trend <- rep(list(NA), length(values))
names(mape) <- names(mse) <- names(theil) <- names(bias) <- names(log.lik) <-
  names(c.i) <- names(r.2) <- names(trend) <- values

list.forecast.test <- list()
  
for(i in  1 : nrow(idx.endog)) {
  print(i)
              
  seas <- idx.matrix[i, "seas"]
  i.lag <- idx.matrix[i, "lag"]
  ec.det <- idx.matrix[i, "ec.det"]

  if(seas == 2)
    seas <- NULL

  # Primera estimacion de Johansen
  johansen <- ca.jo(endog.mat, K = i.lag, season = seas,
                ecdet = ec.det, spec=c("longrun"), dumvar = exog.mat)

  # Obtenemos el rango de la matriz de cointegracion
  test.ca <- johansen@teststat < johansen@cval[,"10pct"]
  names(test.ca) <- c(c(ncol(endog.mat) - 1) : 0)

  r <- as.numeric(names(test.ca)[which(test.ca)])

  if(any(r == 0) || length(r) == 0) next

  vec <- vec2var(johansen, r = min(r))

  # Identificamos Outliers Multivariados
  if(sum(mult.outliers(vec)$out.var) == 0){
    outliers <- rep(0, nrow(vec$y))
    outliers[(which(rowSums(abs(scale(resid(vec)))) == max(rowSums(abs(scale(resid(vec)))))))+ vec$p] <- 1
  }

  outliers <- c(rep(0, vec$p), mult.outliers(vec)$out.var)

  # Nuevas Exogenas
  exog.test <- cbind(exog.mat, outliers)
  colnames(exog.test) <- c(colnames(exog.mat), "Outliers")

  # Reestimamos Johansen
  johansen <- ca.jo(endog.mat, K = i.lag, season = seas,
                ecdet = ec.det, spec=c("longrun"), dumvar = exog.test)


  # Obtenemos el rango de la matriz de cointegracion nuevamente
  test.ca <- johansen@teststat < johansen@cval[,"10pct"]
  names(test.ca) <- c(c(ncol(endog.mat) - 1) : 0)

  r <- as.numeric(names(test.ca)[which(test.ca)])

  # Reestimamos el rango de la matriz de cointegracion
  if(any(r == 0) || length(r) == 0) next

  vec <- vec2var(johansen, r = min(r))

  # Guardamos los resultados en lista
  forecast.mult <- matrix(NA, length.fore, ncol(vec$y) + 3)
  colnames(forecast.mult) <- c(colnames(vec$y), "CI", "lower", "upper")

  # Ciclo para recursion de pronostico de modelo cointegrado
  for(j in 1 : length.fore){
    ## Pronostico del modelo
    forecast <- sapply(1 : ncol(vec$y), function(x) predict.vec2var(vec, n.ahead = 1,
                  dumvar = as.matrix(exog.new)[j,,drop = FALSE],
                  mat.res = NULL)$fcst[[x]][,c("fcst", "CI", "lower", "upper")])
    colnames(forecast) <- colnames(vec$y)

    # Reestimamos Johansen
    y <- rbind(vec$y, forecast["fcst",])
    x <- rbind(eval(vec$vecm@dumvar), exog.new[j, ])
    rownames(y) <- rownames(x) <- NULL

    johansen <- ca.jo(y, K = i.lag, season = seas,
                  ecdet = ec.det, spec=c("longrun"), dumvar = x)

    # Obtenemos el rango de la matriz de cointegracion nuevamente
    test.ca <- johansen@teststat < johansen@cval[,"10pct"]
    names(test.ca) <- c(c(ncol(endog.mat) - 1) : 0)

    r <- as.numeric(names(test.ca)[which(test.ca)])

    if(any(r == 0) || length(r) == 0) next

    vec <- vec2var(johansen, r = min(r))

    forecast.mult[j,1:ncol(vec$y)] <- forecast["fcst",]
    forecast.mult[j,"CI"] <- forecast["CI", region]
    forecast.mult[j,"lower"] <- forecast["lower", region]
    forecast.mult[j,"upper"] <- forecast["upper", region]
  }

  # Hubo algun modelo con r = n o r = 0
  if(any(is.na(forecast.mult))) next

  # Restriccion pronostico atipico
  if(restrict){
    if(any(forecast.mult[,region] > max(price)) ||
      any(forecast.mult[,region] < min(price))) next
  }

  # Matriz de cointegracion normalizada
  coint.mat <- sapply(1 : min(r),
                function(x) scale(-(cointegration(vec$vecm)[,x] - vec$y[,region])))

  # Ecuaciones de cointegracion
  coint.equa <- sapply(1 : min(r) , function(x) -round(vec$vecm@V[,x][-1], 4))

  # Pesos de acuerdo a Lambdas
  w.fore <- johansen@lambda[1 : min(r)]/sum(johansen@lambda[1 : min(r)])

  # Tendencia comun unica
  common.trend <- rowSums(sapply(1 : min(r), function(x) w.fore[x] *
                    coint.mat[,x])) * sd(price) + mean(price)

  # Pronostico de la tendencia comun
  fore.common.trend <- common.trend[c(n + 1): c(n + length.fore)]

  # Restriccion de tendencia comun atipica
  if(restrict){
    if(any(fore.common.trend > max(price)) ||
      any(fore.common.trend < min(price))) next
  }

  # Estadistico de relacion
  beta <- -ca.jo(cbind(vec$y[,region], common.trend), K = i.lag, season = seas,
                ecdet = ec.det, spec=c("longrun"),
                dumvar = rbind(exog.test, exog.new[,colnames(exog.test), drop = FALSE]))@V[2,1]

  ## Seleccion recursiva de modelos de pronosticos. Pronostica 'n.try' periodos de muestra tres pasos adelante
  mat.predict <- matrix(NA, n.try, 3)
  colnames(mat.predict) <- crit.stat[1:3]

  # Empieza el ciclo para seleccionar el modelo
  for(k in 1 : n.try){

    i.test.1 <- c(1:(n-k-length.test+1))
    i.test.2 <- c(n-k-length.test + 2):
                  c(nrow(vec$y[c(1:(n-k-length.test+1)),]) + length.test)

    endog.try <- vec$y[1:n, ,drop = FALSE]
    exog.try <- x[1:n, ,drop = FALSE]
    endog.try <- vec$y[i.test.1,,drop = FALSE]
    exog.try <- x[i.test.1,,drop = FALSE]

    exog.real <- x[i.test.2,, drop = FALSE]
    endog.real <- vec$y[i.test.2, region, drop = FALSE]

    idx.x <- apply(exog.try, 2, sum) != 0

    johansen.predict <- ca.jo(endog.try, K = vec$p,
                          seas = seas, dumvar = exog.try[,idx.x, drop = FALSE],
                          ecdet = ec.det, spec=c("longrun"))


      # Obtenemos el rango de la matriz de cointegracion nuevamente
      test.ca <- johansen.predict@teststat < johansen.predict@cval[,"10pct"]
      names(test.ca) <- c(c(ncol(endog.try) - 1) : 0)

      r <- as.numeric(names(test.ca)[which(test.ca)])

      # Reestimamos el rango de la matriz de cointegracion
      if(any(r == 0) || length(r) == 0) next

      vec.test <- vec2var(johansen.predict, r = min(r))

      fore.mult.test <- matrix(NA, length.test, ncol(vec$y))
      colnames(fore.mult.test) <- colnames(vec$y)

      for(j in 1 : length.test){
        ## Pronostico del modelo
        forecast.test <- sapply(1 : ncol(vec.test$y), function(x) predict.vec2var(vec.test, n.ahead = 1,
                      dumvar = exog.real[j,idx.x,drop = FALSE],
                      mat.res = NULL)$fcst[[x]][,"fcst"])
                      #mat.res = mat.res[j,,drop = FALSE])$fcst[[x]][,"fcst"])

        # Reestimamos Johansen
        endog.try <- rbind(vec.test$y, forecast.test)
        exog.try <- rbind(eval(vec.test$vecm@dumvar), exog.real[j,idx.x,drop = FALSE])
        rownames(endog.try) <- rownames(exog.try) <- NULL

        johansen.test <- ca.jo(endog.try, K = i.lag, season = seas,
                    ecdet = ec.det, spec=c("longrun"), dumvar = exog.try)

        # Obtenemos el rango de la matriz de cointegracion nuevamente
        test.ca <- johansen.test@teststat < johansen.test@cval[,"10pct"]
        names(test.ca) <- c(c(ncol(endog.try) - 1) : 0)

        r <- as.numeric(names(test.ca)[which(test.ca)])

        if(any(r == 0) || length(r) == 0) next

        vec.test <- vec2var(johansen.test, r = min(r))

        fore.mult.test[j,] <- forecast.test
      }

      list.forecast.test[[k]] <- fore.mult.test
                                        
      mat.predict[k, "MAPE"] <- mean(abs(c(endog.real - fore.mult.test[,region])/endog.real))

      mat.predict[k, "MSE"] <- sqrt(mean((fore.mult.test[,region] - endog.real)^2))

      mat.predict[k, "THEIL"] <- sqrt(mean((fore.mult.test[,region] - endog.real)^2))/
                            (sqrt(sum((fore.mult.test[,region])^2)/length.test)
                          + sqrt(sum((endog.real)^2)/length.test))

#      mat.predict[k, "BIAS"] <- (mean(fore.mult.test[,region]) -
#                          mean(endog.real))^2/mean((fore.mult.test[,region] - endog.real)^2)
  }

  mean.mat.predict <- round(colMeans(mat.predict), 3)

  ## Estadisticos del Modelo
  # Porcentaje relativo de error                          
  idx.endog[i, "MAPE"]  <-  mean.mat.predict["MAPE"]

  # Raiz de Error Cuadratic Medio
  idx.endog[i, "MSE"]  <- mean.mat.predict["MSE"]

  # Estadistico de Theil
  idx.endog[i, "THEIL"] <- mean.mat.predict["THEIL"]

  # Proporcion de sesgo
  growth <- inf(c(endog.mat[,region], forecast.mult[,region]))
  idx.endog[i, "BIAS"] <- mean(abs(c(growth[c(length(growth) - length.fore + 1):length(growth)] -
                          objective)/objective))
                          
  #idx.endog[i, "BIAS"] <- mean.mat.predict["BIAS"]

  # Modelo que maximiza la funcion de verosimilitud
  idx.endog[i, "LOG.LIK"] <- -logLik(vec)

  # MSE Teorico
	idx.endog[i, "CONF"] <- mean(forecast.mult[,"CI"])

  # R2
  idx.endog[i, "R2.ADJ"] <- 1 - summary(cajools(johansen))[[1]]$adj.r.squared

  # Common Trend
  idx.endog[i, "COMMON.TREND"] <- abs(1 - beta)

  # Condicionamos valores
  if(!is.na(idx.endog[i, "MAPE"])){
    if(all(idx.endog[1:c(i - 1), "MAPE"] >= idx.endog[i, "MAPE"], na.rm = TRUE)){
      mape[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
      mape[["vec"]] <- vec
      mape[["ce"]] <- coint.equa
      mape[["w"]] <- w.fore
      mape[["common.trends"]] <- coint.mat
      mape[["common.trend"]] <- common.trend
      mape[["statistic"]] <- mean.mat.predict["MAPE"]
      mape[["try.forecast"]] <- list.forecast.test
    }
  }
  if(!is.na(idx.endog[i, "MSE"])){
    if(all(idx.endog[1:c(i - 1), "MSE"] >= idx.endog[i, "MSE"], na.rm = TRUE)){
      mse[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
      mse[["vec"]] <- vec
      mse[["ce"]] <- coint.equa
      mse[["w"]] <- w.fore
      mse[["common.trends"]] <- coint.mat
      mse[["common.trend"]] <- common.trend
      mse[["statistic"]] <- mean.mat.predict["MSE"]
      mse[["try.forecast"]] <- list.forecast.test
    }
  }
  if(!is.na(idx.endog[i, "THEIL"])){
    if(all(idx.endog[1:c(i - 1), "THEIL"] >= idx.endog[i, "THEIL"], na.rm = TRUE)){
      theil[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
      theil[["vec"]] <- vec
      theil[["ce"]] <- coint.equa
      theil[["w"]] <- w.fore
      theil[["common.trends"]] <- coint.mat
      theil[["common.trend"]] <- common.trend
      theil[["statistic"]] <- mean.mat.predict["THEIL"]
      theil[["try.forecast"]] <- list.forecast.test

    }
  }
  if(!is.na(idx.endog[i, "BIAS"])){
    if(all(idx.endog[1:c(i - 1), "BIAS"] >= idx.endog[i, "BIAS"], na.rm = TRUE)){
      bias[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
      bias[["vec"]] <- vec
      bias[["ce"]] <- coint.equa
      bias[["w"]] <- w.fore
      bias[["common.trends"]] <- coint.mat
      bias[["common.trend"]] <- common.trend
      bias[["statistic"]] <- mean.mat.predict["BIAS"]
      bias[["try.forecast"]] <- list.forecast.test
    }
  }
  if(!is.na(idx.endog[i, "LOG.LIK"])){
    if(all(idx.endog[1:c(i - 1), "LOG.LIK"] <= idx.endog[i, "LOG.LIK"], na.rm = TRUE)){
      log.lik[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
      log.lik[["vec"]] <- vec
      log.lik[["ce"]] <- coint.equa
      log.lik[["w"]] <- w.fore
      log.lik[["common.trends"]] <- coint.mat
      log.lik[["common.trend"]] <- common.trend
      log.lik[["statistic"]] <- idx.endog[i, "LOG.LIK"]
      log.lik[["try.forecast"]] <- list.forecast.test

    }
  }
  if(!is.na(idx.endog[i, "CONF"])){
    if(all(idx.endog[1:c(i - 1), "CONF"] >= idx.endog[i, "CONF"], na.rm = TRUE)){
      c.i[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
      c.i[["vec"]] <- vec
      c.i[["ce"]] <- coint.equa
      c.i[["w"]] <- w.fore
      c.i[["common.trends"]] <- coint.mat
      c.i[["common.trend"]] <- common.trend
      c.i[["statistic"]] <- idx.endog[i, "CONF"]
      c.i[["try.forecast"]] <- list.forecast.test
    }
  }
  if(!is.na(idx.endog[i, "R2.ADJ"])){
    if(all(idx.endog[1:c(i - 1), "R2.ADJ"] >= idx.endog[i, "R2.ADJ"], na.rm = TRUE)){
      r.2[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
      r.2[["vec"]] <- vec
      r.2[["ce"]] <- coint.equa
      r.2[["w"]] <- w.fore
      r.2[["common.trends"]] <- coint.mat
      r.2[["common.trend"]] <- common.trend
      r.2[["statistic"]] <- 1 - idx.endog[i, "R2.ADJ"]
      r.2[["try.forecast"]] <- list.forecast.test
    }
  }
  if(!is.na(idx.endog[i, "COMMON.TREND"])){
    if(all(idx.endog[1:c(i - 1), "COMMON.TREND"] >= idx.endog[i, "COMMON.TREND"], na.rm = TRUE)){
      trend[["forecast"]] <- forecast.mult[,c(colnames(vec$y), "lower", "upper")]
      trend[["vec"]] <- vec
      trend[["ce"]] <- coint.equa
      trend[["w"]] <- w.fore
      trend[["common.trends"]] <- coint.mat
      trend[["common.trend"]] <- common.trend
      trend[["statistic"]] <- idx.endog[i, "COMMON.TREND"]
      trend[["try.forecast"]] <- list.forecast.test
    }
  }
}

# Resultados
forecast.price <- cbind(mape[["forecast"]][,region], mse[["forecast"]][,region],
                  theil[["forecast"]][,region], bias[["forecast"]][,region],
                  log.lik[["forecast"]][,region], c.i[["forecast"]][,region],
                  r.2[["forecast"]][,region], trend[["forecast"]][, region])
colnames(forecast.price) <- crit.stat

forecast.costos <- cbind(mape[["forecast"]][,"Costos"], mse[["forecast"]][,"Costos"],
                  theil[["forecast"]][,"Costos"], bias[["forecast"]][,"Costos"],
                  log.lik[["forecast"]][,"Costos"], c.i[["forecast"]][,"Costos"],
                  r.2[["forecast"]][,"Costos"], trend[["forecast"]][, "Costos"])
colnames(forecast.costos) <- crit.stat

forecast.monetario <- cbind(mape[["forecast"]][,"Monetario"], mse[["forecast"]][,"Monetario"],
                  theil[["forecast"]][,"Monetario"], bias[["forecast"]][,"Monetario"],
                  log.lik[["forecast"]][,"Monetario"], c.i[["forecast"]][,"Monetario"],
                  r.2[["forecast"]][,"Monetario"], trend[["forecast"]][, "Monetario"])
colnames(forecast.monetario) <- crit.stat

forecast.demanda <- cbind(mape[["forecast"]][,"Demanda"], mse[["forecast"]][,"Demanda"],
                  theil[["forecast"]][,"Demanda"], bias[["forecast"]][,"Demanda"],
                  log.lik[["forecast"]][,"Demanda"], c.i[["forecast"]][,"Demanda"],
                  r.2[["forecast"]][,"Demanda"], trend[["forecast"]][, "Demanda"])
colnames(forecast.demanda) <- crit.stat




fore.costos <- rowMeans(forecast.costos)
fore.monetario <- rowMeans(forecast.monetario)
fore.demanda <- rowMeans(forecast.demanda)
#forecast.mean <- forecast.price[,"CONF"]


linf <- rowMeans(cbind(mape[["forecast"]][,"lower"], mse[["forecast"]][,"lower"],
                  theil[["forecast"]][,"lower"], bias[["forecast"]][,"lower"],
                  log.lik[["forecast"]][,"lower"], c.i[["forecast"]][,"lower"],
                  r.2[["forecast"]][,"lower"], trend[["forecast"]][, "lower"]))
                  
lsup <- rowMeans(cbind(mape[["forecast"]][,"upper"], mse[["forecast"]][,"upper"],
                  theil[["forecast"]][,"upper"], bias[["forecast"]][,"upper"],
                  log.lik[["forecast"]][,"upper"], c.i[["forecast"]][,"upper"],
                  r.2[["forecast"]][,"upper"], trend[["forecast"]][, "upper"]))

forecast.mean <- cbind(rowMeans(forecast.price), linf, lsup, lsup -
                  rowMeans(forecast.price))
colnames(forecast.mean) <- c("Forecast", "linf", "lsup", "CI")

#forecast.mean <- forecast.price[, "COMMON.TREND"]

vec <- trend[["vec"]]
forecast <- trend[["forecast"]]
ce <- trend[["ce"]]
w <- trend[["w"]]
common.trends <- trend[["common.trends"]]
common.trend <- trend[["common.trend"]]


mat.graph <- vec$y
name.mes <- substring(as.character(seq(as.Date("2000/1/1"),
              by = "month", length = nrow(mat.graph))), 3, 7)

seq.graph <- c(nrow(mat.graph) - show.data + 1): nrow(mat.graph)

mat.graph <- mat.graph[seq.graph,]
name.mes <- name.mes[seq.graph]
num.mes <- 1 : length(name.mes)

# Nombrando ecuaciones de cointegracion
col.names.ce <- c()
for(i in 1 : ncol(ce))
  col.names.ce[i] <- paste("Tendencia - ", i, sep = "")

colnames(ce) <- col.names.ce
colnames(common.trends) <- col.names.ce

# Nombre de variables
graph.names <- colnames(endog.mat)[-which(colnames(endog.mat) == region)]


# Vector de colores
v.colors <- c("blue", "red", "green", "pink", "cyan", "yellow2", "purple", "gray63",
              "firebrick1", "chocolate4","khaki3","seagreen1","orange", "deepskyblue4",
              "orange3", "red4", "deeppink2", "lightgreen","lightpink4", "lightskyblue4")

#
## Resultados a graficar
#
graph.ce <- 1
graph.trend <- 1:ncol(ce)

# Tendencias comunes a graficar
trends.norm <- scale(common.trends[seq.graph, , drop = FALSE])

# Ecuaciones de cointegracion
png(paste(dt.file,"graficos_",mes,"/Grafica de Cointegracion/Grafica de Cointegracion-",region,".png",sep=""),
width=650)
  opp <- par(mar = c(10, 4, 6, 4))
    barplot(ce[, graph.ce], las = 3, cex.names = 0.75, col = v.colors[1:nrow(ce)])
    title(paste("Grafica de cointegracion para relacion: ", colnames(ce)[graph.ce], sep = ""),
      col.main = "red4")
  par(opp)
dev.off()

# Graficas de tendencias
png(paste(dt.file,"graficos_",mes,"/Grafica de tendencias/Grafica de tendencias-",region,".png",sep=""),
width=650)
  matplot(num.mes, mat.graph[,region], type = "l",  col = v.colors[1], lwd = 5, xaxt = "n", xlab = "Mes",
    ylab = "INPC", yaxt = "n", ylim = c(min(c(mat.graph[,region],common.trend[seq.graph])),
    max(c(mat.graph[,region],common.trend[seq.graph]))))
  lines(common.trend[seq.graph], col = 1, lwd = 2)
  axis(1, num.mes, name.mes, las = 3, cex.axis = 0.75)
  axis(2, cex.axis = 0.75, col = "blue", lwd = 4)
  par(new=T)
  matplot(num.mes, trends.norm, type = "l",
    col = v.colors[2:c(ncol(trends.norm) + 1)], lwd = 1,
    xaxt = "n", yaxt = "n", xlab = "Mes",  ylab = "INPC", lty = 1,
    ylim = c(min(trends.norm), max(trends.norm)))
  axis(4, cex.axis = 0.75, col = "gold4", lwd = 2)
  abline(v = nrow(mat.graph) - length.fore, col = "gray", lwd = 2, lty = 2)
  legend("topleft", c(region, "Tendencia Comun", colnames(trends.norm)), col = c(v.colors[c(1)], 1,
  v.colors[2:c(ncol(trends.norm) + 1)]), lwd = c(5, 3, rep(2, ncol(trends.norm))), bg = "white",
  title = "Variables del modelo", cex = 0.85, title.col = "blue4")
  title(paste("Grafica de tendencias comunes normalizadas respecto al INPC-", region, sep = ""), col.main = "brown",
  font.main = 4)
dev.off()

# Costos
png(paste(dt.file,"graficos_",mes,"/Grafico de cargas_Costos/Grafico de cargas_Costos-",region,".png",sep=""),
width=650)
  load.mat.costos. <- as.numeric(load.mat.costos)
  names(load.mat.costos.) <- rownames(load.mat.costos)
  opp <- par(mar = c(10, 4, 6, 4))
     barplot(load.mat.costos., col = v.colors[1:length(load.mat.costos.)],
      las = 3, cex.names = 0.75)
    title(paste("Grafica de pesos de indicador Costos respecto al INPC-region ", region, sep = ""), col.main = "brown",
    font.main = 4)
  par(opp)
dev.off()

# Monetario
png(paste(dt.file,"graficos_",mes,"/Grafico de cargas_Monetario/Grafico de cargas_Monetario-",region,".png",sep=""),
width=650)
  load.mat.monetario. <- as.numeric(load.mat.monetario)
  names(load.mat.monetario.) <- rownames(load.mat.monetario)
  opp <- par(mar = c(10, 4, 6, 4))
    barplot(load.mat.monetario., col = v.colors[1:length(load.mat.monetario.)],
      las = 3, cex.names = 0.75)
    title(paste("Grafica de pesos de indicador Monetario respecto al INPC-region ", region, sep = ""), col.main = "brown",
      font.main = 4)
  par(opp)
dev.off()

# Demanda
png(paste(dt.file,"graficos_",mes,"/Grafico de cargas_Demanda/Grafico de cargas_Demanda-",region,".png",sep=""),
width=650)
  load.mat.demanda. <- as.numeric(load.mat.demanda)
  names(load.mat.demanda.) <- rownames(load.mat.demanda)
  opp <- par(mar = c(10, 4, 6, 4))
    barplot(load.mat.demanda., col = v.colors[1:length(load.mat.demanda.)],
      las = 3, cex.names = 0.75)
    title(paste("Grafica de pesos de indicador demanda respecto al INPC-region ", region, sep = ""), col.main = "brown",
      font.main = 4)
  par(opp)
dev.off()


mat.plot <- cbind(c(vec$y[1:n,region], rep(NA, length.fore)), c(rep(NA, n - 1), vec$y[n], forecast.mean[,"Forecast"]),
              c(rep(NA, n), linf), c(rep(NA, n), lsup))
colnames(mat.plot) <- c(region, "Pronóstico", "Límite Inferior", "Límite Superior")

mat.plot.exog <- scale(cbind(c(endog.mat[,"Costos"], fore.costos), c(endog.mat[,"Monetario"], fore.monetario),
                  c(endog.mat[,"Demanda"], fore.demanda)))
colnames(mat.plot.exog) <- c("Costos", "Monetario", "Demanda")

  # compute the limits of the graph
png(paste(dt.file,"graficos_",mes,"/Grafico de pron. multivariado/Grafico de pron. multivariado-",region,".png",sep=""),
width = 650)
  require(gplots)
  ylim <- c(min(mat.plot[seq.graph, ], na.rm = TRUE),
            max(mat.plot[seq.graph, ], na.rm = TRUE)+1)

  opar <- par(mar = c(4, 4, 2, 2), las = 1)
    mat.plot <- mat.plot[seq.graph,]
    seq.forecast <- c(nrow(mat.plot) - length.fore + 1) : nrow(mat.plot)
    mat.plot.exog <- mat.plot.exog[seq.graph,]

    plot(mat.plot, ylim = ylim, type = "n", xlim = c(1,nrow(mat.plot)),
      ylab = "INPC", xlab = "Periodo", xaxt = "n")
    axis(1, 1:show.data, name.mes, las = 3)
      
    usr <- par("usr")
    # split the figure in two parts - the part used to fit the model
    rect(usr[1], usr[3], seq.forecast[1], usr[4], border = NA, col = "lightcyan")
    # split the figure in two parts - the part used to make the forecast
    rect(seq.forecast[1], usr[3], usr[2], usr[4], border = NA, col = "cornsilk")

    abline(h = seq(round(min(ylim)), round(max(ylim)), 0.5), col = "gray", lty = 2, lwd = 1)
    
    # draw a 95% confidence band
    polygon(c(seq.forecast, sort(seq.forecast, TRUE)),
      c(mat.plot[seq.forecast, "Límite Inferior"],
      rev(mat.plot[seq.forecast, "Límite Superior"])),
      col = "orange",
      lty = 2,border = NA)

    lines(seq.forecast , mat.plot[seq.forecast, "Límite Inferior"], lty = 2)
    lines(seq.forecast , mat.plot[seq.forecast, "Límite Superior"], lty = 2)

    mat.plot[seq.forecast[1], region] <- mat.plot[seq.forecast[1], "Pronóstico"]
    lines(c(mat.plot[, region]), lwd = 2)
    lines(seq.forecast, mat.plot[seq.forecast, "Pronóstico"],lwd = 2, col = "white")

    media <- mean(c(mat.plot[,region],mat.plot[seq.forecast, "Pronóstico"]), na.rm = TRUE)
    sdd <-  sd(c(mat.plot[,region],mat.plot[seq.forecast, "Pronóstico"]), na.rm = TRUE)

    parr <- 2.5
    lines(mat.plot.exog[, "Costos"]*sdd + media, col = "darkblue")
    lines(mat.plot.exog[, "Monetario"]*sdd + media , col = "deeppink2")
    lines(mat.plot.exog[, "Demanda"]*sdd + media, col = "gold4")

    smartlegend(x="left", y= "top", inset=0,
                legend = c("INPC","Pronóstico Integral","Intervalo de confianza 95%"),
                fill=c("black","white","orange"),
                bg = "cornsilk")
    smartlegend(x="left", y= "bottom", inset=0,
            legend = c("Estructura de Costos", "Mercado Monetario", "Demanda"),
            fill=c("darkblue","deeppink2","gold4"),
            bg = "cornsilk")
            
#    title(paste("Modelo de Pronóstico Multivariado para el INPC Región - ", region, sep = ""),
#      col.main = "red4", font.main= 33, cex.main = 1)
      mtext(paste("Modelo de Pronóstico Multivariado para el INPC Región - ", region, sep = ""),
        cex = 1.2, line = 0.9, col = "red4")
      mtext(paste("CIMAT-MTY", sep = ""), cex = 1, line = 0, col = "royalblue4")
  par(opar)
dev.off()

# Gonzalo - Granger
vec.mape <- r.2[["vec"]]$vecm
gonzalo <- gon.gra(vec.mape)

A1 <- gonzalo$A1
A2 <- gonzalo$A2

alpha <- gonzalo$alpha
gamma.c <- gonzalo$gamma.c

mat.inpc <- round(rbind(matrix(rep(NA, 24), ncol = 2), cbind(inf(
              c(endog.mat[,region], forecast.mean[, "Forecast"])),
              inf(gonzalo$P[,1]))), 1)
              
colnames(mat.inpc) <- c("Inflación Anual", "Inflación Anual Permanente")

inpc.t <- matrix(mat.inpc[,1] - mat.inpc[,2])
colnames(inpc.t) <- c("Inflación Anual Transitoria") 

png(paste(dt.file,"graficos_",mes,"/Inflacion en componentes temporales/Inflacion en componentes temporales-",region,".png",sep=""),
width=650)
  matplot(1:show.data, mat.inpc[seq.graph, ], type = "l", col = c("blue", "red"),
    lwd = c(3,2), lty = 1, xaxt = "n", ylab = "% INPC", xlab = "")
  axis(1, 1:show.data, name.mes, las = 3)
  abline(v = show.data - length.fore, col = "gray", lty = 2, lwd = 2)
  par(new=T)
  matplot(1:show.data, inpc.t[seq.graph, ], type = "l", col = c("gold"),
    lwd = 2, lty = 1, xaxt = "n", yaxt = "n", ylab = "", xlab = "")
  axis(4, las = 3)
  abline(v = show.data - length.fore, col = "gray", lty = 2, lwd = 2)
  legend("topleft", c(colnames(mat.inpc), colnames(inpc.t)) ,col = c("blue", "red", "gold"),
    lwd = 2, lty = 1, bg = "white")
  title(paste("Inflación en componentes temporales-",region, sep = ""), col.main = "red4")
dev.off()

png(paste(dt.file,"graficos_",mes,"/Inflacion desagregada en componentes/Inflacion desagregada en componentes-",region,".png",sep=""),
width=650)
  op <- par(mfrow = c(3, 1))
    ts.plot(ts(vec.mape@x[,1], start = 2000, frequency = 12), col = "black", lwd = 2, ylab = "INPC")
    title("INPC en desagregación según componentes temporales", col.main = "blue4")
    ts.plot(ts(gonzalo$P[,1], start = 2000, frequency = 12), col = "red", lwd = 2, ylab = "INPC-P")
    ts.plot(ts(gonzalo$T[,1], start = 2000, frequency = 12), col = "blue", lwd = 2, ylab = "INPC-T")
  par(op)
dev.off()

mat.inpc <- cbind(mat.inpc, inpc.t)
write.csv(forecast.mean, paste(dt.file, "resultados_",mes,"/inpc/inpc-",region,".csv", sep = ""))
write.csv(mat.inpc[c(nrow(mat.inpc) - length.fore + 1):nrow(mat.inpc),,drop=FALSE],
  paste(dt.file, "resultados_",mes,"/inflacion/inflacion-",region,".csv", sep = "")) 

write.csv(ce, paste(dt.file, "resultados_",mes,"/ce/ce-",region,".csv", sep = ""))
write.csv(load.mat.costos, paste(dt.file, "resultados_",mes,"/costos/costos-",region,".csv", sep = ""))
write.csv(load.mat.monetario, paste(dt.file, "resultados_",mes,"/monetario/monetario-",region,".csv", sep = ""))
write.csv(load.mat.demanda, paste(dt.file, "resultados_",mes,"/demanda/demanda-",region,".csv", sep = ""))
write.csv(mape$statistic, paste(dt.file, "resultados_",mes,"/mape/mape-",region,".csv", sep = ""))
write.csv(mat.plot.exog, paste(dt.file, "resultados_",mes,"/exog/exog-",region,".csv", sep = ""))
write.csv(A1,paste(dt.file, "resultados_",mes,"/permanente/permanente-",region,".csv", sep = ""))
write.csv(A2,paste(dt.file, "resultados_",mes,"/transitoria/transitoria-",region,".csv", sep = ""))
write.csv(alpha,paste(dt.file, "resultados_",mes,"/largo.plazo/largo.plaz-",region,".csv", sep = ""))
write.csv(gamma.c,paste(dt.file, "resultados_",mes,"/gamma.c/gamma.c-",region,".csv", sep = ""))
write.csv(w, paste(dt.file, "resultados_",mes,"/w/w-",region,".csv", sep = ""))

#
##
#
show.data.2 <- 18
trend.model <- FALSE

if(trend.model){
  try.fore <- sapply(n.try: 1, function(x) trend[["try.forecast"]][[x]][,region])
}else{
  try.fore <- sapply(n.try: 1, function(x) mape[["try.forecast"]][[x]][,region])
}

fore <- matrix(NA, n.try * 2 - 1, n.try)

for(i in 1 : n.try)
fore[i:c(n.try + i - 1), i] <- try.fore[,i]


na.mat <- matrix(NA, nrow = length(price) - nrow(fore), ncol = n.try)
colnames(na.mat) <- colnames(fore)

fore <- rbind(na.mat, fore)

name.mes.2 <- substring(as.character(seq(as.Date("2000/1/1"),
            by = "month", length = nrow(fore))), 3, 7)
            
# Vector de colores
v.colors <- c("blue", "red", "green", "pink", "cyan", "yellow2", "purple", "gray63", "firebrick1", "chocolate4","khaki3",
           "seagreen1","orange", "deepskyblue4", "orange3", "red4", "deeppink2", "lightgreen","lightpink4", "lightskyblue4")
                  
names.fore <- c()
for(i in 1 : n.try)
names.fore[i] <- paste("Pronostico-",i, sep ="")

accuracy.mat <- cbind(price, fore)[-c(1 : c(length(price) - show.data.2)),]
colnames(accuracy.mat) <- c("INPC (Real)", names.fore)

name.mes.2 <- name.mes.2[-c(1 : c(length(price) - show.data.2))]

png(paste(dt.file,"graficos_",mes,"/Modelo multivariado de prediccion/Modelo multivariado de prediccion-",region,".png",sep=""),
width=650)
  matplot(1:show.data.2, accuracy.mat, type = "l", col = v.colors[1:ncol(accuracy.mat)],
  lwd = c(3, rep(2, ncol(accuracy.mat) - 1)), lty = 1, xaxt = "n", ylab = "INPC", xlab = "Mes")
  axis(1, 1:show.data.2, name.mes.2, las = 3)
  abline(v = 8, col = "gray", lty = 2)
  legend("topleft", colnames(accuracy.mat), col = v.colors[1:ncol(accuracy.mat)],
  lwd = c(3, rep(2, ncol(accuracy.mat) - 1)), lty = 1, title = "Predicción fuera de muestra", title.col = "red4")
  if(trend.model){
    title("Modelo Multivariado de Tendencia Común Económica (COMMON-TREND-MODEL)", col.main = "red4")
  }else{
    title("Modelo Multivariado de Predicción fuera de muestra (MIN-MAPE)", col.main = "red4")
  }
dev.off()

write.csv(accuracy.mat, paste(dt.file, "resultados_",mes,"/try.forecast/try.forecast-",region,".csv", sep = ""))

png(paste(dt.file,"graficos_",mes,"/Relacion indicadores2/Relacion indicadores2-",region,".png",sep=""),
width=650)
  ts.plot(ts(endog.mat[, region, drop = FALSE], start = 2000, frequency = 12), col = "blue",
    lwd = 3, ylab = "INPC")
  axis(2, col = "blue", lwd = 2)
  par(new = T)
  matplot(endog.mat[, -1, drop = FALSE], type = "l", col = c("red", "green4", "brown"),
    lwd = 1, yaxt = "n", lty = 1, xlab = "", xaxt = "n", ylab = "")
  axis(4, col = "red", lwd = 1)
  legend("topleft", c(paste("INPC-",region), colnames(endog.mat)[-1]),
    col =  c("blue", "red", "green4", "brown"), lwd = c(2, 1, 1 ,1),
    lty = 1)
  title(paste("Relaciones entre indicadores de mercado y el INPC región-",region, sep=""))
dev.off()

png(paste(dt.file,"graficos_",mes,"/INPC/INPC-",region,".png",sep=""),
width=650)
  ts.plot(ts(endog.mat[, region, drop = FALSE], start = 2000, frequency = 12), col = "blue",
    lwd = c(3, 1), lty = c(1,2),  ylab = "INPC")
  axis(2, col = "blue", lwd = 2)
  par(new = T)
  matplot(mat.inpc[-c(c(nrow(mat.inpc) - 11): nrow(mat.inpc)), 1], type = "l",
    col = c("blue"), lwd = 1, yaxt = "n", lty = 2, xlab = "", xaxt = "n", ylab = "")
  axis(4, col = "blue", lwd = 1, lty = 2)
  legend("topleft", c(paste("INPC-",region), "Inflación Anual"),
    col =  c("blue"), lwd = c(2, 1, 1 ,1),
    lty = c(1, 2))
  title(paste("Índice Nacional de Precios al Consumidor (INPC)"))
dev.off()

vec.r2 <- r.2[["vec"]]
ca.jo.r2 <- vec.r2$vecm

fitt <- c(rep(NA, ca.jo.r2@lag), fitted(vec.r2)[,1])

mat.fitt <- ts(cbind(ca.jo.r2@x[,1], fitt), start = 2000, frequency = 12)
colnames(mat.fitt) <- c(paste("INPC-",region), "Ajuste")

png(paste(dt.file,"graficos_",mes,"/Explicacion/Explicacion-",region,".png",sep=""),
width=650)
  ts.plot(mat.fitt, col = c(1,2), lwd = 2, lty = c(1,2))
  abline(v = 2011, col = "gray", lty = 2)
  legend("topleft", colnames(mat.fitt), col = c(1,2), lty = c(1,2), lwd = 2)
  title("Explicación histórica del INPC Cerveza dado indicadores económicos", col.main = "red4")
dev.off()

rank.m <- length(r.2[["w"]])

test.coef <- test.coint(vec.r2$vecm, rank.m)

mat.p.val <- matrix(NA, nrow(test.coef[[1]]), rank.m)
rownames(mat.p.val) <- rownames(test.coef[[1]])

for(i in 1 : rank.m)
  mat.p.val[,i] <- test.coef[[i]][,"p.value"]

p.value.beta <- round(rowSums(sapply(1 : rank.m,
                  function(x) mat.p.val[,x] * r.2[["w"]][x])), 3)

coef.beta <- round(rowSums(sapply(1 : rank.m,
                function(x) r.2[["ce"]][,x] * r.2[["w"]][x])), 3)
              
irf.vec.full <- irf(vec.r2)
irf.vec <- irf.vec.full[[1]][[region]]
irf.li <- irf.vec.full[[2]][[region]]
irf.ls <- irf.vec.full[[3]][[region]]

png(paste(dt.file,"graficos_",mes,"/Impulso/Impulso-",region,".png",sep=""),
width = 650)
  opp <- par(mfrow = c(3,1))
    ts.plot(cbind(irf.vec[,2], irf.li[,2], irf.ls[,2]), col = c(1, 2, 2), lwd = 2, lty = c(1,2,2))
    abline(h = 0, col = "gray", lty = 2)
    title("Costos -->> INPC")
    ts.plot(cbind(irf.vec[,3], irf.li[,3], irf.ls[,3]), col = c(1, 2, 2), lwd = 2, lty = c(1,2,2))
    abline(h = 0, col = "gray", lty = 2)
    title("Mercado Monetario -->> INPC")
    ts.plot(cbind(irf.vec[,4], irf.li[,4], irf.ls[,4]), col = c(1, 2, 2), lwd = 2, lty = c(1,2,2))
    abline(h = 0, col = "gray", lty = 2)
    title("Demanda -->> INPC")
  par(opp)
dev.off()


tabla.coef <- rbind(coef.beta, p.value.beta)
rownames(tabla.coef) <- c("long_run_expl", "p.value")

write.csv(tabla.coef, paste(dt.file, "resultados_", mes, "/coef.test/coef.test-",region,".csv", sep = ""))


### Gráfico New Forecast vs Old Forecast (Nacional)
###
###region
#####
###mes.pasado
#####
###mes
#
#old.fore.inpc<- read.csv((paste(dt.file,"resultados_",mes.pasado,
#       "/inpc/inpc-",region,".csv",sep="")))
###            
#mat.plot.2<-cbind(mat.plot[,1:2],c(rep(NA, nrow(mat.plot)-13), old.fore.inpc[,2], NA ))
###
#png(paste(dt.file,"graficos_",mes,"/Grafico de cambios en pronosticos/Grafico de cambios en pronosticos-",region,".png",sep=""),
#width = 650)
###
#seq.graph <- c(nrow(mat.plot.2) - show.data + 1): nrow(mat.plot.2)
#ylim <- c(min(mat.plot.2[seq.graph, ], na.rm = TRUE),
#         max(mat.plot.2[seq.graph, ], na.rm = TRUE))
###
#matplot(mat.plot.2, ylim = ylim, type = "l" ,
#    ylab = "INPC", xlab = "Periodo", col=c(1,"green","red"), lwd=2)
#     axis(1, 1:show.data, name.mes, las = 3)
#     legend("topleft", c("Pronóstico Actual", "Pronóstico del Reporte Anterior"), col=c("green","red"),
#      lwd=c(2,2), lty=1)
#title(paste("Pronóstico del reporte anterior a nivel",region, sep=" "), col.main = "brown",
# font.main = 4)
##
#  dev.off()
#

