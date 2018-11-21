quita.tendencia.init <- function(data, inicio , frecuencia)
{
  # CLOSURE para quitar seasonality, regresa una funcion
  #  data (vector): valores de la serie de tiempo 
  # inicio (vector.longitud2): inicio de la serie de tiempo c(2008,1)
  # frecuencia (numeric): frecuencia de la serie (semanal:54)
  inicio <- inicio 
  frecuencia <- frecuencia
  function(data)
  {
    s <- ts(data, start = inicio, frequency = frecuencia) # habra que harcodear estos numeros
    x <- tryCatch(seas(s)$series$s11,
                         error = function(e) data, finally = data )
    x <- as.numeric(x)
    return(x)
  }
}
adf.test.custom <- function(y, option='both')
{
  # funcion para elegir el resago optimo
  # y (numeric): vector con los datos de la serie de tiempo univariada
  # option (chacaracter) : eleccion de la tendencia e intercepto 'none','c','t','both'
  y <- ts(y) 
  lag <- floor(log(length(y))) + 1 #acotamos el numero de lags por el que siguiere el 
  #texto de Chan Ngai
  datos <- data.frame(y1 = diff(y))
  for (i in 2:lag) #aumentamos las columnas de lags 
  {
    datos[, as.character(paste0('y',i))] <- c(diff(y, lag=i), rep(NA, i-1))
  }
  names(datos) <- c('y1', names(datos)[2:lag])
  if (option == 'none')
  {
    #aplicamos el test para cada lag
    resultado <- mapply(function(x)
    {
      formula <- paste(names(datos)[x], collapse = '+')
      formula <- as.formula(paste0('y1 ~ ', formula, '-1'))
      modelo <- lm(formula , data = datos )
      resumen <- summary(modelo)
      # nos fijamos si todos los coeficientes de la regresion
      # son significativos individualmente
      coeficientes.significativos <- resumen$coefficients[, 'Pr(>|t|)']
      coeficientes.significativos <- coeficientes.significativos <= 0.05
      if(sum(coeficientes.significativos) == 1)
      {
        big <- BIC(modelo)
        # en caso de que todos los coeficientes sean significativos regresamos 
        # el BIC de la regresion
        return(big)
      } else {return(Inf)} #si un coeficiente al menos es no significativo
      #regresamos un BIC infinito
    }, 2:lag)
  }
  
  if (option == 'c')
  {
    datos[, 'c'] <- rep(1, dim(datos)[1] )
    #aplicamos el test para cada lag
    resultado <- mapply(function(x)
    {
      formula <- paste(names(datos)[x], collapse = '+')
      formula <- as.formula(paste0('y1 ~ ', formula))
      modelo <- lm(formula , data = datos )
      resumen <- summary(modelo)
      # nos fijamos si todos los coeficientes de la regresion
      # son significativos individualmente
      coeficientes.significativos <- resumen$coefficients[, 'Pr(>|t|)']
      coeficientes.significativos <- coeficientes.significativos <= 0.05
      if(sum(coeficientes.significativos) == 2)
      {
        big <- BIC(modelo)
        # en caso de que todos los coeficientes sean significativos regresamos 
        #el BIC de la regresion
        return(big)
      }else {return(Inf)} #si un coeficiente al menos es no significativo 
      #regresamos un BIC infinito
    }, 2:lag)
  }
  if (option == 't')
  {
    datos[, 't'] <- cumsum(1:dim(datos)[1])
    #aplicamos el test para cada lag
    resultado <- mapply(function(x)
    {
      formula <- paste(c(names(datos)[x], 't'), collapse = '+')
      formula <- as.formula(paste0('y1 ~ ', formula, '-1'))
      modelo <- lm(formula , data = datos )
      resumen <- summary(modelo)
      # nos fijamos si todos los coeficientes de la regresion
      # son significativos individualmente
      coeficientes.significativos <- resumen$coefficients[, 'Pr(>|t|)']
      coeficientes.significativos <- coeficientes.significativos <= 0.05
      if(sum(coeficientes.significativos) == 2)
      {
        big <- BIC(modelo)
        # en caso de que todos los coeficientes sean significativos regresamos 
        #el BIC de la regresion
        return(big)
      }else { return(Inf)}  #si un coeficiente al menos es no significativo
      #regresamos un BIC infinito
    }, 2:lag)
  }
  if (option == 'both')
  {
    datos[, 't'] <- cumsum(1:dim(datos)[1])
    #aplicamos el test para cada lag
    resultado <- mapply(function(x)
    {
      formula <- paste(c(names(datos)[2:(x)], 't'), collapse = '+')
      formula <- as.formula(paste0('y1 ~ ', formula))
      modelo <- lm(formula , data = datos )
      resumen <- summary(modelo)
      # nos fijamos si todos los coeficientes de la regresion
      # son significativos individualmente
      coeficientes.significativos <- resumen$coefficients[, 'Pr(>|t|)']
      coeficientes.significativos <- coeficientes.significativos <= 0.05
      if(sum(coeficientes.significativos) == 3)
      {
        big <- BIC(modelo)
        # en caso de que todos los coeficientes sean significativos regresamos
        #el BIC de la regresion
        return(big)
      } else { return(Inf)}  #si un coeficiente al menos es no significativo
      #regresamos un BIC infinito
    }, 2:lag)
  }
  parsimonia <- which.min(resultado) 
  names(parsimonia) <- 'Lag optimo'
  return(parsimonia)
}

Busetti.Harvey <- function(y, option='both', k = k, 
                           posicion=posicion, 
                           p=.95)
{
  # y (numeric): vector con la serie de tiempo
  # K (int): numero de posibles saltos estructurales 1<=k<=4
  # posiciones (int): vector con los indices en donde la serie se sospecha que
  #presenta cambios estructurales
  # p (double): confianza a la que se requiere el test
  if(k >4) stop()
  serie <- y
  posicion <- posicion
  k <- k
  #creamos un dataframe con las posiciones para particionar la serie
  particion <- data.frame(start = c(1, posicion+1), 
                          stop = c(posicion, length(serie)))
  e <- serie
  # a continuacion particionamos la serie con el data.frame 'particion'
  muestras <- lapply(X=1:dim(particion)[1], 
                     function(x)
                     {
                       return(e[particion$start[x]:particion$stop[x]])
                       
                     })
  estadistico.particion <- function(parte)
  {
    #calculo del numerados del estadistico dado por la ecuacion (4.5) del paper
    media.parte <- mean(unlist(parte), na.rm=TRUE ) 
    e.s <- sum((cumsum( parte- media.parte))**2)
    return(e.s/(length(parte)**2))
  }
  errores <- lapply(muestras, estadistico.particion)
  sigma <- var(serie) #para ambos casos la varianza se calcula igual
  # se termina calculo del estadistico de la ecuacion (4.5)
  estadistico <- sum(unlist(errores))/sigma
  #tabla de valores de los valores criticos para el modelo de la forma 1
  tabla1 <- data.frame(k = 1:4, 
                       p0.9 =c(0.347, 0.607, 0.841, 1.063),
                       p0.95= c(0.461, 0.748, 1.000, 1.237  ),
                       p0.99 = c(0.743, 1.074, 1.359, 1.623 ))
  
  #tabla de valores de los valores criticos para el modelo de la forma 2
  tabla2 <- data.frame(k=1:4, 
                       p0.9= c(0.119, 0.211, 0.296, 0.377),
                       p0.95= c(0.149, 0.247, 0.332, 0.423 ),
                       p0.99 = c(0.218, 0.329, 0.428, 0.521  ) )
  if(option == 'c') #determinacion del valor critico
  {
    valor.critico <- tabla1[ k , paste0('p',p)]
  } else {
    valor.critico <- tabla2[ k , paste0('p',p)]
  }
  a <- ifelse(estadistico >= valor.critico, 'Se rechaza H0, ie s? hay cambio estructural',
              'No se rechaza H0, ie no hay cambio estructural' )
  a <- paste0(a, ' en las posiciones: ', posicion, ' con confianza de: ', p)
  return(a)}  #regresamos un mensaje imformativo
# test johansen
test.joha <- function(x, lags.max = 8, crit = "AIC(n)", ec.det = "const",
              dum.var = NULL, seas = NULL, typ = "trace"){

  # select type var and k optim
  type.var <- c("none", "const", "trend", "both")
  mat.k <- matrix(0, length(type.var), lags.max)
  rownames(mat.k) <- type.var

  for(i in 1:length(type.var)){
    mat.k[i,] <- VARselect(x, lag.max = lags.max,
                    type = type.var[i], exogen = dum.var)$criteria[crit, ]
  }

  # var type
  type.opt <- which(apply(mat.k, 1, min)==min(apply(mat.k, 1, min)))
  k.opt <- which(mat.k[type.opt,] == min(mat.k[type.opt,]))

  x.econ <- ca.jo(x, type = typ, K = k.opt, ecdet = ec.det, 
                dumvar = dum.var)

  return(x.econ)
}

# @ criteria information
criteria <- function(object)
{
  y <- object$y
  K <- ncol(y)
  p <- object$p
  datamat <- object$datamat
  ylagged <- datamat[,(K+1):ncol(datamat)]
  sample <- nrow(ylagged)
  sampletot <- nrow(y)
  detint <- ncol(ylagged) - K^2 + 2
  type <- object$type
  rhs <- NULL
  if(type == "none"){
    rhs <- matrix(0, nrow(ylagged), 2)
    rhs[,1] <- 1
    rhs[,2] <- (p + 1):sampletot
  }
  if(type == "const"){
    rhs <- matrix(0, nrow(ylagged), 1)
    rhs[,1] <- (p + 1):sampletot
  }
  if(type == "trend"){
    rhs <- matrix(0, nrow(ylagged), 1)
    rhs[,1] <- 1
  }
  if(!is.null(rhs)){
    ys.lagged <- as.matrix(cbind(ylagged, rhs))
  }else{
    ys.lagged <- as.matrix(ylagged)
  }
  yendog <- y[(p + 1):sampletot,]
  resids <- lm.fit(x = ys.lagged, y = yendog)$residuals
  sigma.det <- det(crossprod(resid(object))/sample)
  aic <- log(sigma.det) + (2/sample)*(p * K^2 + K * detint)
  hq <- log(sigma.det) + (2*log(log(sample))/sample)*detint
  bic <- log(sigma.det) + (log(sample)/sample)
  result <- c(aic, hq, bic)
  names(result) <- c("AIC(n)", "HQ(n)", "SC(n)")
  return(result)
}

# @ impulse response function plot
irf.plot <- function(irf, variable){

  # k variable
  k <- which(colnames(irf$irf[[1]]) == variable)
  N <- ncol(irf$irf[[1]])

  # list
  list.irf <- list()

  # names.f
  names.f <- c("lower", "mean", "upper")

  for(i in 1 : N){

    mat.temp <- matrix(0, nrow(irf$irf[[1]]), 3)
    colnames(mat.temp) <- names.f

    mat.temp[,"mean"] <- irf$irf[[k]][,i]
    mat.temp[,"lower"] <- irf$Lower[[k]][,i]
    mat.temp[,"upper"] <- irf$Upper[[k]][,i]


    list.irf[[i]] <- mat.temp
  }
  names(list.irf) <- colnames(irf$irf[[1]])

  return(list.irf)
}
