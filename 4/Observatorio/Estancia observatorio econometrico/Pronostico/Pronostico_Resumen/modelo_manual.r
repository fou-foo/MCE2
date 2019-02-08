# Ultima actualizacion: 12-08-2018 (LIZETH)
#____________________________________________________________________________________

remove(list=ls())

require(gplots)
library(vars) 
library(pls)
library(forecast)
library(mvtnorm)
library(ks)

dt.file <-"/home/andres/Dropbox/ECONOMETRIA/Proyecto_inflacion/codigos/"
source(paste(dt.file, "model_functions.r", sep =""))

#___________________________________________________________________________________
###           VARIABLES Y PARAMETROS                                           #####
#___________________________________________________________________________________

# Determinacion de variables:

regiones <- c("Frontera.norte", "Noroeste", "Noreste", "Centro.norte", 
              "Centro.sur", "Sur", "Mexico", "Nacional")

region   <- "Nacional" # Seleccionar la region que se quiere realizar 
variable <- "Precios"  # La variable a modelar
mes      <- "dic"      # el mes actual
mes.pasado <- "ene"    # el nombre empleado para el mes anterior

# Parametros a seleccionar:

titular <- 1          # 1 si quiero que aparezcan los titulos
length.fore <- 6       # Num. de meses a pronosticar (?con 3?)
lag.max  <- 12         # Para el numero de modelos 
seas.max <- 12         # Para el numero de modelos 
ec.det <- c("none", "const", "trend")
c.sig <- 0.10          # Nivel de significancia
show.data  <- 48       # Se utilizan los ?ltimos 4 a?os como ventana de tiempo
show.data2 <- 24       # Se utilizan los ?ltimos 2 a?os como ventana de tiempo
length.test <- 6       # El # de meses a probar el pron intramuestra (asociado a n.try)
n.try <- 6             # Rezagos a probar atras
restrict <- FALSE      # TRUE Si pronostico no puede superar min/max
#forecast.average <- FALSE # NO SE USA
objective <- 3         # Lo usa en el bias - Ahora el objetivo de BM es 3  

# Rutas: 
dt.econ <- paste(dt.file, "Econ.Data", sep = "")
dt.precios <- paste(dt.file, variable, sep = "")

### NOTA: ecdet: 'none' for no intercept in cointegration, 
###              'const' for constant term in cointegration 
###              'trend' for trend variable in cointegration.

#___________________________________________________________________________________
###                            CARGAMOS LOS DATOS                              #####
#___________________________________________________________________________________

costos    <- read.csv(paste(dt.file, "Costos.csv", sep = ""), row.names = 1)
monetario <- read.csv(paste(dt.file, "Monetario.csv", sep = ""), row.names = 1)
demanda   <- read.csv(paste(dt.file, "Demanda.csv", sep = ""), row.names = 1)
precios   <- read.csv(paste(dt.file, "Precios.csv", sep = ""), row.names = 1)
out.precios <- precios[217:222,7]
inicia <- which(rownames(demanda)=="01/01/2005")    # el primer dato a considerar

costos    <- costos[inicia:216,]
monetario <- monetario[inicia:216,]
demanda   <- demanda[inicia:216,]
precios   <- precios[inicia:216,]

price <- precios[,region,drop = FALSE]
#___________________________________________________________________________FINdatos


####################################################################################
####################################################################################
####################################################################################
####################################################################################


#___________________________________________________________________________________
### PARTE 1:PLS                                                                #####
#___________________________________________________________________________________

# PLS para reducir la dimensionalidad 

costos.pls    <- factor.pls(price, costos )
monetario.pls <- factor.pls(price, monetario)
demanda.pls   <- factor.pls(price, demanda)

### NOTA: El warning es porque no hay regresores externos.
### Warning message: In forecast.Arima(auto.arima(score.pls, xreg = xreg[1:nrow(score.pls),  
### :The non-existent newxreg arguments will be ignored.

endog.mat <- cbind(precios[,region], costos.pls$score.pls,
              monetario.pls$score.pls, demanda.pls$score.pls)
colnames(endog.mat) <- c(region, "Costos", "Monetario", "Demanda")

load.mat.costos    <- costos.pls$load.mat.w
load.mat.monetario <- monetario.pls$load.mat.w
load.mat.demanda   <- demanda.pls$load.mat.w

#___________________________________________________________________________  FIN P1




#___________________________________________________________________________________
### PARTE 2: PRONOSTICO DE INFLACION                                           #####
#___________________________________________________________________________________

numcols <- ncol(endog.mat)   # Variables
n <- nrow(endog.mat)         # Observaciones

exog.mat <- NULL      
exog.new <- matrix(rep(0, length.fore)) # Outliers en pronostico
colnames(exog.new) <- c("Outliers")

# Posibles combinaciones de modelos:
idx.matrix  <- as.data.frame(matrix(0, (lag.max - 1)*(seas.max - 1)*length(ec.det), 3))
colnames(idx.matrix) <- c("seas", "lag", "ec.det")
idx.matrix[,"seas"]  <- rep(2:seas.max, each = (length(ec.det))*(lag.max - 1))
idx.matrix[,"lag"]   <- rep(c(2:lag.max), nrow(idx.matrix)/(lag.max - 1))
idx.matrix[,"ec.det"]<- rep(rep(ec.det, each = lag.max - 1), seas.max - 1)

### NOTA: Numero de modelos = seasoanal(11)*lag(11) x deterministic(3) = 363

# Guardar el desempe?o de cada uno de los 363 modelos:
crit.stat <- c("MAPE", "MSE", "THEIL", "BIAS", 
              "LOG.LIK","CONF", "R2.ADJ", "COMMON.TREND")

idx.endog <- matrix(NA, nrow(idx.matrix), length(crit.stat))
colnames(idx.endog) <- crit.stat

# Valores a considerar para cada criterio:
values <-  c("forecast", "vec", "ce", "w", "common.trends", "common.trend", 
             "statistic", "try.forecast")

mape <- mse <- theil <- bias <- log.lik <- c.i <- r.2 <- trend <- rep(list(NA), length(values))
names(mape) <- names(mse) <- names(theil) <- names(bias) <- names(log.lik) <-
  names(c.i) <- names(r.2) <- names(trend) <- values

list.forecast.test <- list()

### NOTA: Core del codigo. Loop que corre todos los modelos siguiendo:
### (a) Cointegracion de variables originales
### (b) Pronostico
### (c) Evaluacion desempe?o


for(i in  1:nrow(idx.endog) ) {
  seas   <- idx.matrix[i, "seas"]
  i.lag  <- idx.matrix[i, "lag"]
  ec.det <- idx.matrix[i, "ec.det"]

  if(seas == 2)
    seas <- NULL
  #________________________________________________________________________________
  ## (a) COINTEGRANCION VARIABLES ORIGINALES
  #________________________________________________________________________________
  
  # Primera estimacion de Johansen
  johansen <- ca.jo(endog.mat, K = i.lag, season = seas,
                ecdet = ec.det, spec=c("longrun"), dumvar = exog.mat)

  ### NOTA: cval es el valor critico al 10% 
  test.ca <- johansen@teststat < johansen@cval[,"10pct"]
  names(test.ca) <- c(c(numcols - 1) : 0)

  # Obtenemos el rango de la matriz de cointegracion
  r <- as.numeric(names(test.ca)[which(test.ca)])
  r
  ((any(r == 0) || length(r) == 0))
  
  # 1ra excepcion: no existen relaciones a largo plazo
  if(any(r == 0) || length(r) == 0) next

  vec <- vec2var(johansen, r = min(r))

  #######################################################
  ### REVISAR:
  
  # Identificamos Outliers Multivariados (Funcion creada)
  if(sum(mult.outliers(vec)$out.var) == 0){
    outliers <- rep(0, nrow(vec$y))
    outliers[(which(rowSums(abs(scale(resid(vec)))) == max(rowSums(abs(scale(resid(vec)))))))+ vec$p] <- 1
  } # END if
  
  ### DUDA: Si aqui asigna un valor a outliers, lo que pasa arriba no desaparece? sin importar si
  ###       no hubo outliers.
  ### Buscar cuando se cumpla que no hay outlierts.
  
  outliers <- c(rep(0, vec$p), mult.outliers(vec)$out.var)
  ########################################################
  
  #_______________________________________
  ## a.1 se consideran ahora los outlieres
  #_______________________________________
  
  # Nuevas Exogenas
  exog.test <- cbind(exog.mat, outliers)
  colnames(exog.test) <- c(colnames(exog.mat), "Outliers")

  # Reestimamos Johansen
  johansen <- ca.jo(endog.mat, K = i.lag, season = seas,
                ecdet = ec.det, spec=c("longrun"), dumvar = exog.test)

  # Obtenemos el rango de la matriz de cointegracion nuevamente
  test.ca <- johansen@teststat < johansen@cval[,"10pct"]
  names(test.ca) <- c(c(numcols - 1) : 0)
  
  # Reestimamos el rango de la matriz de cointegracion
  r <- as.numeric(names(test.ca)[which(test.ca)])

  # 2da excepcion: no existen relaciones a largo plazo al incluir outliers
  if(any(r == 0) || length(r) == 0) next

  vec <- vec2var(johansen, r = min(r))
  

  #______________________________________________________________________________
  ## (b) COINTEGRANCION CON PRONOSTICO PARA SIGUIENTES length.fore MESES
  #______________________________________________________________________________
  
  # Creamos la matriz para guardar los resultados
  forecast.mult <- matrix(NA, length.fore, ncol(vec$y) + 3)
  colnames(forecast.mult) <- c(colnames(vec$y), "CI", "lower", "upper")  
  
  for(j in 1 : length.fore){
    
    ### NOTA: Se aplica funcion -> 'predict.vec2var' de 'model_functions.r'
    ### Se predice utilizando el objeto VEC

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
    names(test.ca) <- c(c(numcols - 1) : 0)

    r <- as.numeric(names(test.ca)[which(test.ca)])

    # 3ra excepcion: condicion para considerar el siguiente mes
    #debido a que el valor de pronostico no preservo las relaciones a largo plazo,
    #es decir, no hay ecuaciones de cointegracion.
    if(any(r == 0) || length(r) == 0) next 

    vec <- vec2var(johansen, r = min(r))

    # Aqui va llenando los datos del pronostico
    forecast.mult[j,1:ncol(vec$y)] <- forecast["fcst",]
    forecast.mult[j,"CI"] <- forecast["CI", region]
    forecast.mult[j,"lower"] <- forecast["lower", region]
    forecast.mult[j,"upper"] <- forecast["upper", region]
  } # end for j

  # 4ta excepcion: condición para considerar el siguiente modelo i(donde i=1:363),
  #cuando no se pudo pronosticar algun mes debido a la 3ra excepcion. 
  if(any(is.na(forecast.mult))) next 

  # 5ta excepcion: Pronostico fuera del rango max/min
  if(restrict){
    if(any(forecast.mult[,region] > max(price)) ||
       any(forecast.mult[,region] < min(price)) ) next
  }

  # Matriz de cointegracion normalizada
  coint.mat <- sapply( 1:min(r),
                function(x) scale( -(cointegration(vec$vecm)[,x] - vec$y[,region]) ) )

  # Ecuaciones de cointegracion
  coint.equa <- sapply(1 : min(r) , function(x) -round(vec$vecm@V[,x][-1], 4))

  # Pesos de acuerdo a Lambdas
  w.fore <- johansen@lambda[1 : min(r)]/sum(johansen@lambda[1 : min(r)])
  summary(johansen)
  johansen@lambda
  # Tendencia comun unica
  common.trend <- rowSums(sapply(1 : min(r), function(x) w.fore[x] *
                    coint.mat[,x])) * sapply(price,sd) + sapply(price,mean)
  
  # Pronostico de la tendencia comun
  fore.common.trend <- common.trend[(n + 1):(n + length.fore)]

  # 6ta excepcion: Restriccion de tendencia comun atipica
  if(restrict){
    if(any(fore.common.trend > max(price)) ||
      any(fore.common.trend < min(price))) next
  }

  # Estadistico de relacion
  beta <- ca.jo(cbind(vec$y[,region], common.trend), K = i.lag, season = seas,
                ecdet = ec.det, spec=c("longrun"),
                dumvar = rbind(exog.test, exog.new[,colnames(exog.test), drop = FALSE]))@V[2,1]
  ### Revisar estadistico de relacion, porque v[2,1]?
  
  #______________________________________________________________________________
  ## (c) DESEMPE?O DEL MODELO
  #______________________________________________________________________________
  
  mat.predict <- matrix(NA, n.try, 4)
  colnames(mat.predict) <- crit.stat[1:4]

  # Empieza el ciclo para evaluar el desempe?o de los modelos (EDM) - Dentro muestra
  for(k in 1 : n.try){
    i.test.1 <- c(1:(n-k-length.test+1))
    i.test.2 <- c(n-k-length.test + 2):
                  c(nrow(vec$y[c(1:(n-k-length.test+1)),]) + length.test)
    
    endog.try <- vec$y[i.test.1,,drop = FALSE]
    exog.try  <- x[i.test.1,,drop = FALSE]

    endog.real <- vec$y[i.test.2, region, drop = FALSE]
    exog.real  <- x[i.test.2,, drop = FALSE]
    
    idx.x <- apply(exog.try, 2, sum) != 0

    johansen.predict <- ca.jo(endog.try, K = vec$p,
                          seas = seas, dumvar = exog.try[,idx.x, drop = FALSE],
                          ecdet = ec.det, spec=c("longrun"))

    # Obtenemos el rango de la matriz de cointegracion nuevamente
    test.ca <- johansen.predict@teststat < johansen.predict@cval[,"10pct"]
    names(test.ca) <- c(c(ncol(endog.try) - 1) : 0)
    
    # Reestimamos el rango de la matriz de cointegracion
    r <- as.numeric(names(test.ca)[which(test.ca)])


    # 7ma excepcion: no existen relaciones a largo plazo dentro de muestra
    if(any(r == 0) || length(r) == 0) next

    vec.test <- vec2var(johansen.predict, r = min(r))

    fore.mult.test <- matrix(NA, length.test, ncol(vec$y))
    colnames(fore.mult.test) <- colnames(vec$y)
    
    for(j in 1:length.test){
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

      # 8va excepcion: condicion para considerar el siguiente mes
      #debido a que el valor de pronostico no preservo las relaciones a largo plazo,
      #es decir, no hay ecuaciones de cointegracion dentro de muestra.
      if(any(r == 0) || length(r) == 0) next

      vec.test <- vec2var(johansen.test, r = min(r))

      fore.mult.test[j,] <- forecast.test
      } #End for j

    list.forecast.test[[k]] <- fore.mult.test

    # Aqui llena los errores 
    mat.predict[k, "MAPE" ] <- mean( abs((endog.real - fore.mult.test[,region])/endog.real) )
    mat.predict[k, "MSE"]   <- sqrt(mean( (fore.mult.test[,region] - endog.real)^2 ))

    mat.predict[k, "THEIL"] <- sqrt(mean((fore.mult.test[,region] - endog.real)^2)) /   
                            (sqrt(sum( (fore.mult.test[,region])^2)/length.test)
                           + sqrt(sum((endog.real)^2)/length.test))

    mat.predict[k, "BIAS"] <- ( mean(fore.mult.test[,region]) - mean(endog.real) )^2 / 
                                mean( (fore.mult.test[,region] - endog.real)^2 )
  } # END K FOR EDM

  mean.mat.predict <- round(colMeans(mat.predict), 4)

  ## ESTADISTICOS DEL MODELO
  
  # Porcentaje relativo de error
  idx.endog[i, "MAPE"]  <-  mean.mat.predict["MAPE"]

  # Raiz de Error Cuadratic Medio
  idx.endog[i, "MSE"]  <- mean.mat.predict["MSE"]

  # Estadistico de Theil
  idx.endog[i, "THEIL"] <- mean.mat.predict["THEIL"]
  
  # Proporcion de sesgo
  idx.endog[i, "BIAS"] <- mean.mat.predict["BIAS"]
  
  # Modelo que maximiza la funcion de verosimilitud
  idx.endog[i, "LOG.LIK"] <- -logLik(vec)

  # MSE Teorico
	idx.endog[i, "CONF"] <- mean(forecast.mult[,"CI"])

  # R2
  idx.endog[i, "R2.ADJ"] <- 1 - summary(cajools(johansen))[[1]]$adj.r.squared

  # Common Trend
  idx.endog[i, "COMMON.TREND"] <- abs(1 - beta)

  # Vamos acumulando el que tenga el mejor resultado (dentro de muestra), pero guardamos el valor fuera de muestra.
  # 9na excepcion: se evita que los valorese sean na, es decir,
  # que algun mes no se haya pronosticado dentro de muestra
  if(!is.na(idx.endog[i, "MAPE"])){
    if( all(idx.endog[1:(i - 1), "MAPE"] >= idx.endog[i, "MAPE"], na.rm = TRUE) ){
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
} # end for sobre i


#___________________________________________________________________________________
### PARTE 3: RESULTADOS                                                          #####
#___________________________________________________________________________________


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


fore.costos    <- rowMeans(forecast.costos)
fore.monetario <- rowMeans(forecast.monetario)
fore.demanda   <- rowMeans(forecast.demanda)



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

#######################################
### SETTINGS
#######################################
vec      <- trend[["vec"]]
forecast <- trend[["forecast"]]
ce       <- trend[["ce"]]
w        <- trend[["w"]]
common.trends <- trend[["common.trends"]]
common.trend <- trend[["common.trend"]]
# Nombrando ecuaciones de cointegracion
col.names.ce <- c()
for(i in 1 : ncol(ce))
  col.names.ce[i] <- paste("Tendencia - ", i, sep = "")

colnames(ce) <- col.names.ce
colnames(common.trends) <- col.names.ce

#___________________________________________________________________________________
### PARTE 4: GRAFICAS                                                          #####
#___________________________________________________________________________________

# PARAMETROS PARA GRAFICOS
mat.graph <- vec$y
name.mes <- substring(as.character(seq(as.Date("2005/01/01"),
                                       by = "month", length = nrow(mat.graph))), 3, 7)

# Para la parte de la submuestra
seq.graph <- c(nrow(mat.graph) - show.data - length.fore  + 1): nrow(mat.graph)

mat.graph <- mat.graph[seq.graph,]
name.mes  <- name.mes[seq.graph]
num.mes   <- 1 : length(name.mes)

# Tendencias comunes a graficar
trends.norm <- scale(common.trends[seq.graph, , drop = FALSE])

# Paletas de colores
col.indic <- c("deeppink3","gold3","dodgerblue3")
col.tend  <- c("red3","orange","gold")
titulos   <- "red4"

# 1. Grafica de cargas costos
#_________________________________________________________________________________g1

load.mat.costos. <- as.numeric(load.mat.costos)
names(load.mat.costos.) <- rownames(load.mat.costos)
png(paste(dt.file,"graficas/Grafico de cargas-costos-",region,".png",sep=""),
    width=650)
opp <- par(mar = c(3, 3, 3, 1))
barplot(load.mat.costos., col = palette()[(1:length(load.mat.costos.))],
        las = 1, cex.names = 0.9)
title(paste("Pesos de indicador costos respecto al INPC ", region, sep = ""),
      col.main = "red4", font.main = 4)
par(opp)
dev.off()



# 2. Grafica de cargas monetario
#_________________________________________________________________________________g2

load.mat.monetario. <- as.numeric(load.mat.monetario)
names(load.mat.monetario.) <- rownames(load.mat.monetario)

png(paste(dt.file,"graficas/Grafico de cargas-monetario-",region,".png",sep=""),
    width=650)
opp <- par(mar = c(3, 3, 3, 1))
barplot(load.mat.monetario., col = palette()[(1:length(load.mat.monetario.))],
        las = 1, cex.names = 0.9)
title(paste("Pesos de indicador monetario respecto al INPC ", region, sep = ""),
      col.main = "red4", font.main = 4)
par(opp)
dev.off()

# 3. Grafica de cargas Demanda
#_________________________________________________________________________________g3

load.mat.demanda. <- as.numeric(load.mat.demanda)
names(load.mat.demanda.) <- rownames(load.mat.demanda)

png(paste(dt.file,"graficas/Grafico de cargas-demanda-",region,".png",sep=""),
    width=650)
opp <- par(mar = c(3, 3, 3, 1))
barplot(load.mat.demanda., col = palette()[(1:length(load.mat.demanda.))],
        las = 1, cex.names = 0.9)
title(paste("Pesos de indicador demanda respecto al INPC ", region, sep = ""),
      col.main = "red4", font.main = 4)
par(opp)
dev.off()

# 4. Grafica INPC y relacion con indicadores
#_________________________________________________________________________________g4

png(paste(dt.file,"graficas/Relacion indicadores1-",region,".png",sep=""),width=650)
opp <- par(mar = c(3, 3, 1, 1))
ts.plot(ts(scale(endog.mat), start = 2005, frequency = 12),col = c(1,col.indic),
        lwd = c(3, rep(1, numcols - 1)), xlab= "Periodo")
legend("topleft", c("INPC", colnames(endog.mat)[2:4]), col = c(1,col.indic),
       lwd = c(3, rep(1, numcols - 1)),
       inset = c(0.01, 0.01))
title(paste("Relaciones entre indicadores y el INPC ",region, sep=""),
      col.main = titulos)
par(opp)
dev.off()


# 5. Grafica de cointegracion
#_________________________________________________________________________________g5
# Ecuaciones de cointegracion
for ( i in 1:ncol(ce)){
  png(paste(dt.file,"graficas/Grafica de Cointegracion-",region,i,".png",sep=""),
      width=650)
  opp <- par(mar = c(10, 4, 6, 4))
  barplot(ce[, i], las = 1, cex.names = 0.75, col = palette())
  title(paste("Cointegracion para relacion: ", colnames(ce)[i], sep = ""),
        col.main = titulos)
  par(opp)
  dev.off()
}

# 6. Grafica de tendencias comunes
#_________________________________________________________________________________g6

val.trends <- max(abs(min(trends.norm)), abs(max(trends.norm)))+2
png(paste(dt.file,"graficas/Grafica de tendencias-",region,".png",sep=""),
    width=650)
matplot(num.mes, mat.graph[,region], type = "l",  col = "deepskyblue3", lwd = 4, xaxt = "n", 
        xlab = "Periodo", ylab = "INPC", yaxt = "n", 
        ylim = c(min(c(mat.graph[,region],common.trend[seq.graph]))-30,
                 max(c(mat.graph[,region],common.trend[seq.graph])))+10)
lines(common.trend[seq.graph], col = 1, lwd = 2)
axis(1, num.mes, name.mes, las = 3, cex.axis = 0.65)
axis(2, cex.axis = 0.75, col = "dodgerblue4", lwd = 3)
par(new=T)
matplot(num.mes, trends.norm, type = "l", lwd = 1.5, xaxt = "n", yaxt = "n", lty = 1,
        col = col.tend,  xlab = "Periodo",  ylab = "INPC",
        ylim = c(-val.trends, val.trends))
axis(4, cex.axis = 0.75, col = "gold4", lwd = 2)
abline(v = nrow(mat.graph) - length.fore, col = "gray67", lwd = 2, lty = 2)
legend("topleft", c(region, "Tendencia comun", colnames(trends.norm)), bg = "white",
       col = c("deepskyblue3",1,col.tend), inset = c(0.01, 0.01),
       lwd = c(4, 3, rep(2, ncol(trends.norm))), 
       title = "Variables del modelo", cex = 0.85, title.col = "midnightblue")
title(paste("Tendencias comunes normalizadas respecto a INPC Nacional", sep = ""), 
      col.main = titulos, font.main = 4)
dev.off()



# 7. Grafica de pronostico multivariado
#_________________________________________________________________________________
## NOTA: Esta es la imagen que resume TODO el modelo desarrollado.

mat.plot <- cbind(c(vec$y[1:n,region], rep(NA, length.fore)), c(rep(NA, n - 1),
                  vec$y[n], forecast.mean[,"Forecast"]),
              c(rep(NA, n), linf), c(rep(NA, n), lsup))
colnames(mat.plot) <- c(region, "Pronostico", "Limite Inferior", "Limite Superior")


mat.plot.exog <- scale(cbind(c(endog.mat[,"Costos"], fore.costos), c(endog.mat[,"Monetario"], fore.monetario),
                  c(endog.mat[,"Demanda"], fore.demanda)))
colnames(mat.plot.exog) <- c("Costos", "Monetario", "Demanda")

mat.plot <- mat.plot[seq.graph,]
seq.forecast <- c(nrow(mat.plot) - length.fore + 1) : nrow(mat.plot)
mat.plot.exog <- mat.plot.exog[seq.graph,]

ylim <- c(min(mat.plot, na.rm = TRUE),  max(mat.plot, na.rm = TRUE)+3)

# compute the limits of the graph
png(paste(dt.file,"graficas/Grafico de pron. multivariado-",region,".png",sep=""),
    width = 650)
opar <- par(mar = c(4, 4, 1, 1), las = 1)

plot(mat.plot, ylim = ylim, type = "n", xlim = c(1,nrow(mat.plot)),
         ylab = "INPC", xlab = "Periodo", xaxt = "n")
axis(1, 1:(show.data+length.fore), name.mes, las = 3)

# para conocer los valores numericos de los margenes
usr <- par("usr")

#split the figure in two parts - the part used to fit the model
rect(usr[1], usr[3], seq.forecast[1], usr[4], border = NA, col = "lightcyan")

# split the figure in two parts - the part used to make the forecast
rect(seq.forecast[1], usr[3], usr[2], usr[4], border = NA, col = "gray94")

#lineas horizontales cada 2 unidades
abline(h = seq(round(min(ylim)), round(max(ylim)), 2), col = "gray84",lty = 2, lwd = 1)

# draw a 95% confidence band
polygon(c(seq.forecast, sort(seq.forecast, TRUE)),
            c(mat.plot[seq.forecast, "Limite Inferior"],
              rev(mat.plot[seq.forecast, "Limite Superior"])),
            col = "khaki1", lty = 2, border = NA)

# se añaden lineas punteadas en los bordes de la banda de confidencia
lines(seq.forecast , mat.plot[seq.forecast, "Limite Inferior"], lty = 2, col ="khaki4")
lines(seq.forecast , mat.plot[seq.forecast, "Limite Superior"], lty = 2, col ="khaki4")

# para graficar INPC (reales y pronostico)
mat.plot[seq.forecast[1], region] <- mat.plot[seq.forecast[1], "Pronostico"]
lines(c(mat.plot[, region]), lwd = 2)
lines(seq.forecast, mat.plot[seq.forecast, "Pronostico"],lwd = 2, col = "indianred4")

# para graficar los factores (costos, monetario y demanda), escalados al INPC
media <- mean(c(mat.plot[,region],mat.plot[seq.forecast, "Pronostico"]), na.rm = TRUE)
sdd <-  sd(c(mat.plot[,region],mat.plot[seq.forecast, "Pronostico"]), na.rm = TRUE)
lines(mat.plot.exog[, "Costos"]*sdd + media, col = col.indic[1])
lines(mat.plot.exog[, "Monetario"]*sdd + media , col = col.indic[2])
lines(mat.plot.exog[, "Demanda"]*sdd + media, col = col.indic[3])

legend("topleft", inset = c(0.01, 0.01),
           legend = c("INPC","Pronostico Integral","IC 95%"),
           fill=c("black","indianred4","khaki1"),   bg = "gray98")
legend("bottomright", inset = c(0.01, 0.01),
           legend = c("Costos", "Monetario", "Demanda"),
           fill=col.indic,
           bg = "gray98")
mtext(paste("Modelo de Pronostico Multivariado para el INPC Region ",
                region, sep = ""), cex = 1.2, line = 0.9, col = titulos)
par(opar)
dev.off()


# 8. Grafica de INPC por componente permanente y temporal
#_____________________________________________________________________________
### NOTA: Se usa la metodologia: Gonzalo-Granger(1995)

# Gonzalo - Granger
vec.mape <- r.2[["vec"]]$vecm
gonzalo <- gon.gra(vec.mape)

A1 <- gonzalo$A1
A2 <- gonzalo$A2

alpha <- gonzalo$alpha
gamma.c <- gonzalo$gamma.c

mat.inpc <- round(rbind(matrix(rep(NA, 24), ncol = 2), cbind(
              c(endog.mat[,region], forecast.mean[, "Forecast"]),
              gonzalo$P[,1])), 1)
colnames(mat.inpc) <- c("INPC", "Componente Permanente")

inpc.t <- matrix(mat.inpc[,1] - mat.inpc[,2])
colnames(inpc.t) <- c("Componente Transitorio")


png(paste(dt.file,"graficas/Componentes temporales-",region,".png",sep=""),
    width=650)
matplot(1:(show.data+length.fore), mat.inpc[seq.graph, ], type = "l", xaxt = "n", 
        col = c("black", palette()[2]), lwd = c(3,2), lty = 1, 
        ylab = "INPC", xlab = "")
axis(1, 1:(show.data+length.fore), name.mes, las = 3)
par(new=T)
matplot(1:(show.data+length.fore), inpc.t[seq.graph, ], type = "l", col = palette()[3],
        lwd = 2, lty = 1, xaxt = "n", yaxt = "n", ylab = "", xlab = "")
axis(4, las = 3)
abline(v = show.data, col = "gray", lty = 2, lwd = 2)
legend("topleft", c(colnames(mat.inpc), colnames(inpc.t)),bg = "white",
       col = c("black", palette()[2], palette()[3]),lwd = 2, lty = 1 )
title(paste("INPC en componentes temporales", sep = ""), col.main = titulos)
dev.off()



# 9. TS INPC por componente permanente y temporal
#_________________________________________________________________________________g9

png(paste(dt.file,"graficas/INPC desagregado en componentes-",region,".png",sep=""),
width=650)
  op <- par(mfrow = c(3, 1))
    ts.plot(ts(vec.mape@x[,1], start = 2005, frequency = 12), col = "black", lwd = 2,
            ylab = "INPC", xlab = "Periodo")
    title("INPC en desagregacion por componentes", col.main = titulos)
    ts.plot(ts(gonzalo$P[,1], start = 2005, frequency = 12), col = palette()[2], lwd = 2,
            ylab = "INPC-P", xlab = "Periodo")
    ts.plot(ts(gonzalo$T[,1], start = 2005, frequency = 12), col = palette()[3], lwd = 2,
            ylab = "INPC-T", xlab = "Periodo")
  par(op)
dev.off()


# #___________________________________________________________________________________
# ### PARTE 5: EVALUANDO DESEMPEÑO FUERA MUESTRA                                 #####
# #___________________________________________________________________________________
# png(paste(dt.file,"graficas/Desempeño fuera de muestra-",region,".png",sep=""),
#     width=650)
# out.fore <- as.data.frame(forecast)
# out.fore <- out.fore[,-which(names(out.fore) %in% c(colnames(endog.mat)[2:4]))]
# out.fore <- cbind(out.precios, out.fore)
# out.fore$Precision <- (1- abs((out.fore$out.precios-out.fore$Nacional)/out.fore$out.precios) )*100
# 
# 
# out.fore <- cbind(name.mes[(length(name.mes)-length.fore+1):length(name.mes)], out.fore)
# row.names(out.fore) <- NULL
# colnames(out.fore) <- c("Periodo","Real", "Pronostico", "LI","LS","Precisi?n")
# 
# 
# 
# plot(out.fore$Pronostico, type ="l")
# lines(out.fore$Real, col ="blue")
# lines(out.fore$LI, col ="red")
# lines(out.fore$LS, col ="red")
# dev.off()
# #___________________________________________________________________________________
### PARTE 6: GUARDANDO ARCHIVOS CON RESULTADOS                                 #####
#___________________________________________________________________________________


mat.inpc <- cbind(mat.inpc, inpc.t)
write.csv(forecast.mean, paste(dt.file, "resultados/inpc-",region,".csv", sep = ""))
write.csv(mat.inpc[c(nrow(mat.inpc) - length.fore + 1):nrow(mat.inpc),,drop=FALSE],
  paste(dt.file, "resultados/inflacion-",region,".csv", sep = ""))
write.csv(ce, paste(dt.file, "resultados/ce-",region,".csv", sep = ""))
write.csv(load.mat.costos, paste(dt.file, "resultados/costos-",region,".csv", sep = ""))
write.csv(load.mat.monetario, paste(dt.file, "resultados/monetario-",region,".csv", sep = ""))
write.csv(load.mat.demanda, paste(dt.file, "resultados/demanda-",region,".csv", sep = ""))
write.csv(mape$statistic, paste(dt.file, "resultados/mape-",region,".csv", sep = ""))
write.csv(mat.plot.exog, paste(dt.file, "resultados/exog-",region,".csv", sep = ""))
write.csv(A1,paste(dt.file, "resultados/permanente-",region,".csv", sep = ""))
write.csv(A2,paste(dt.file, "resultados/transitoria-",region,".csv", sep = ""))
write.csv(alpha,paste(dt.file, "resultados/largo.plaz-",region,".csv", sep = ""))
write.csv(gamma.c,paste(dt.file, "resultados/gamma.c-",region,".csv", sep = ""))
write.csv(w, paste(dt.file, "resultados/w-",region,".csv", sep = ""))








# ##################################################################################
# ##################################################################################
# ##################################################################################
# ##################################################################################
# ##################################################################################
# ##################################################################################
# ##################################################################################

