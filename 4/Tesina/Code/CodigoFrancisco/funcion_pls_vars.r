#
## @ VAR-PLS para pronsoticar Y con X como funcion de Y en forma autorregresiva
#

# Funcion para matriz en forma de p lags X = (y[t-1],...,y[t-p])
lag.mat <- function(x, p){
  x <- as.matrix(x)
  mat.temp <- matrix(rep(NA, p), p, ncol(x))
  colnames(mat.temp) <- colnames(x)
  mat <- rbind(mat.temp,
          x[-c(c(nrow(x)-p + 1):nrow(x)),,drop=FALSE])
  colnames(mat) <- paste("l",p,".", colnames(x), sep = "")
  return(mat)
}

# Estimacion VAR-PLS
var.pls <- function(x, p, ncomp = NULL, season = NULL, exog = NULL){

  # Matriz X
  x <- as.matrix(x)
  class(x) <- "matrix"
  obs <- nrow(x)

  # Matriz de exogenas
  if(!is.null(exog)){
    exog <- as.matrix(exog)
    class(exog) <- "matrix"
  }

  # Definimos la estacionalidad
  if (!(is.null(season))) {
    season <- abs(as.integer(season))
    dum <- (diag(season) - 1/season)[, -season]
    dums <- dum
    while (nrow(dums) < obs) {
      dums <- rbind(dums, dum)
    }
    dums <- dums[1:obs, ]
    colnames(dums) <- paste("sd", 1:ncol(dums), sep = "")
  }else{
    dums <- NULL
  }

  # Matriz de variables exogenas
  exogen <- cbind(dums, exog)

  # Generamos la matriz X
  mat.x.lags <- matrix(NA, nrow(x), ncol(x)*p)
  colnames.x <- c()
  i.1 <- seq(1, ncol(mat.x.lags), ncol(x))
  i.2 <- seq(ncol(x), ncol(mat.x.lags), ncol(x))

  for(i in 1 : p){
    mat.x.lags[,i.1[i]:i.2[i]] <- lag.mat(x, i)
    colnames.x[i.1[i]:i.2[i]] <- colnames(lag.mat(x, i))
  }

  # Agregamos bloque de exogenas
  mat.x.lags <- cbind(mat.x.lags, exogen)
  colnames(mat.x.lags) <- c(colnames.x, colnames(exogen))

  # Modelo PLS Y = BX + E (todas las componentes) SIMPLS
  pls.model <- plsr(x ~ mat.x.lags, method = "simpls")

  # ncomp predict
  if(is.null(ncomp))
    ncomp <- ncol(mat.x.lags)

  # Resultados
  result <- list(pls.model, x, mat.x.lags, p, season, exog, ncomp)
  names(result) <- c("pls.model", "x", "mat.x.lags", "p", "season",
                    "exog", "ncomp")
  class(result) <- "var.pls"
  return(result)
}

# Prediccion con modelo VAR-PLS
predict.var.pls <- function(object, n.ahead, exog.new = NULL, ncomp = NULL){

  if(class(object) != "var.pls")
    stop("Please, provide an object of class 'var.pls'")

  if(!is.null(exog.new)){
    if(nrow(exog.new) != n.ahead)
      stop("nrow of exogen variables is different to 'n.ahead'")
  }

  # Recuperamos objetos de interes
  mat.x.lags <- object$mat.x.lags
  colnames.x <- colnames(mat.x.lags)
  pls.model <- object$pls.model
  x <- object$x
  p <- object$p
  season <- object$season
  ncomp <- object$ncomp
  obs <- nrow(x)

  # Definimos la estacionalidad
  if (!(is.null(season))) {
    season <- abs(as.integer(season))
    dum <- (diag(season) - 1/season)[, -season]
    dums <- dum
    while (nrow(dums) < c(obs + n.ahead)) {
      dums <- rbind(dums, dum)
    }
    dums <- dums[1:c(obs + n.ahead), ]
    colnames(dums) <- paste("sd", 1:ncol(dums), sep = "")
    # Matriz fija de X*+h
    dums.fore <- dums[c(obs+1):c(obs+n.ahead),]
  }else{
    dums.fore <- NULL
  }

  # Exogenas a futuro
  exog.fore <- cbind(dums.fore, exog.new)

  # Matriz X que alimentara al pronostico
  mat.x.fore <- matrix(NA, n.ahead, ncol(mat.x.lags))
  colnames(mat.x.fore) <- colnames.x

  # Matriz de pronosticos
  mat.y.fore <- matrix(NA, n.ahead, ncol(x))

  # Primer pronostico es con X conocida
  mat.x.fore[1,] <- c(x[nrow(x),], mat.x.lags[nrow(mat.x.lags),
                      c(1:c(ncol(mat.x.lags)-ncol(x)))])

  # Redefinimos con X con X*+h
  M <- 0
  if(!is.null(exog.fore)){
    mat.x.fore[,c(ncol(x)*p + 1): c(ncol(x)*p + ncol(exog.fore))] <-
      exog.fore
    M <- ncol(exog.fore)
  }

  # Realizamos el pronostico para horizonte seleccionado
  for(i in 1 : n.ahead){
    if(i == 1){ # Pronostico 1
      mat.y.fore[i, ] <- predict(pls.model, newdata =
                          t(as.matrix(mat.x.fore[i,])))[,,ncomp]
    }else{ # Pronostico 2,...,n.ahead
      if(!is.null(exog.fore)){
        mat.x.fore[i,] <- c(mat.y.fore[i-1,], mat.x.fore[i-1,1:
                            c(ncol(mat.x.fore)-ncol(x)-M)],mat.x.fore[i,
                            c(ncol(mat.x.fore)-M+1):ncol(mat.x.fore)])
      }else{
        mat.x.fore[i,] <- c(mat.y.fore[i-1,], mat.x.fore[i-1,1:
                            c(ncol(mat.x.fore)-ncol(x)-M)])
      }
        mat.y.fore[i,] <- predict(pls.model, newdata =
                            t(as.matrix(mat.x.fore[i,])))[,,ncomp]
    }
  }
  colnames(mat.y.fore) <- colnames(x)

  # Regresamos el pronostico obtenido
  return(mat.y.fore)
}

# Funcion que calcula intervalos de prediccion por Bootstrap
ci.var.pls.boot <- function(object, n.ahead, exog.new = NULL, runs = 1000,
  seed = 12345, ci = 0.10){

  # Objeto VAR-PLS
  if(class(object) != "var.pls")
    stop("Please, provide an object of class 'var.pls'")

  if (!(is.null(seed)))
    set.seed(abs(as.integer(seed)))

  # Objetos de interes de funcion VAR-PLS
  x <- object$x
  mat.x.lags <- object$mat.x.lags
  model <- object$pls.model
  p <- object$p
  season <- object$season
  exog <- object$exog

  # Matrices y elementos
  K <- ncol(x)
  obs <- nrow(x) - p
  total <- nrow(x)
  datamat <- cbind(x, mat.x.lags, 1)
  colnames(datamat) <- c(colnames(x), colnames(mat.x.lags), "const")
  datamat <- datamat[-c(1:p),]

  # Coeficientes de modelo PLS
  Btemp <- coef(object$pls.model, intercept = TRUE)[,,1]
  B <- matrix(0, K, ncol(mat.x.lags) + 1)
  colnames(B) <- c(colnames(mat.x.lags), "const")
  rownames(B) <- colnames(x)
  B[,"const"] <- Btemp["(Intercept)",]
  B[,-ncol(B)] <- t(Btemp[-1, ])

  # Realizamos Bootstrap
  BOOT <- vector("list", runs)
  ysampled <- matrix(0, nrow = total, ncol = K)
  colnames(ysampled) <- colnames(object$x)
  Zdet <- NULL
  if (ncol(datamat) > (K * (p + 1))) {
      Zdet <- as.matrix(datamat[, (K * (p + 1) + 1):ncol(datamat)])
  }
  resorig <- scale(model$resid[,,model$ncomp], scale = FALSE)*
              c(1/sqrt(c(total-p)/c(total-2*p)))
  for (i in 1:runs) {
      booted <- sample(c(1:obs), replace = TRUE)
      resid <- resorig[booted, ]
      lasty <- c(t(x[p:1, ]))
      ysampled[c(1:p), ] <- x[c(1:p), ]
      for (j in 1:obs) {
          lasty <- lasty[1:(K * p)]
          Z <- c(lasty, Zdet[j, ])
          ysampled[j + p, ] <- B %*% Z + resid[j, ]
          lasty <- c(ysampled[j + p, ], lasty)
      }
      var.pls.boot <- var.pls(ysampled, p, season = season, exog = exog)
      BOOT[[i]] <- predict.var.pls(var.pls.boot, n.ahead, exog.new)
  }

  # Guardamos resultados y obtenemos percentiles
  lower <- ci/2
  upper <- 1 - ci/2
  mat.l <- matrix(NA, nrow = n.ahead, ncol = K)
  mat.u <- matrix(NA, nrow = n.ahead, ncol = K)

  temp <- rep(NA,runs)
  for(j in 1 : ncol(x)){
    for(l in 1 : n.ahead){
      for(i in 1 : runs){
        temp[i] <- BOOT[[i]][l,j]
      }
      mat.l[l,j] <- quantile(temp, lower, na.rm = TRUE)
      mat.u[l,j] <- quantile(temp, upper, na.rm = TRUE)
    }
  }
  colnames(mat.l) <- colnames(mat.u) <- colnames(x)

  # Pronostico de modelo
  fore <- predict.var.pls(object, n.ahead)

  # Resultados de modelo
  result <- list(fore, mat.l, mat.u)
  names(result) <- c("Forecast","Lower", "Upper")

  return(result)
}
