# CIMAT
# Curso: Cómputo Estadístico
# Tarea Regresión, Validación Cruzada y Reducción de la Dimensión
# Agosto-Diciembre 2018
# Por Eduardo Uresti
# -----------------------------
# Bibliotecas requeridas
{
  # install.packages("ISLR")
  # install.packages("glmnet")
  # install.packages("pls")
  # install.packages("leaps")
  library(ISLR)
  library(glmnet)
  library(pls)
  library(leaps)  
}
# -----------------------------
# Problema 1
{
# Datos
{
  data("College")
  summary(College)
  College <- na.omit(College)
  dim(College)
  predictoras <- scale(College[,3:18])
  Apps        <- College$Apps
  datos       <- data.frame(cbind(Apps,predictoras))
  
  remove(predictoras, Apps)
}  
# a) Dividir los datos (%75 %25) y calcular el error de prueba (MSE).
{
  set.seed(1) # Para repetir simulacion, si vale la pena
  # Total de datos
  n <- nrow(datos)
  # 25% del conjunto de datos para entrenamiento
  seleccion <- sample(n,round(0.75*n))
  train <- datos[seleccion,]
  test  <- datos[-seleccion,]
  
  # Modelo de regresión usando los datos de entrenamiento
  modelo <- lm(Apps~.,data=train)
  print(modelo)
  
  # MSE: Error de entrenamiento
  residuales <- train$Apps - predict(modelo,train)
  errorEntrenamiento <- mean(residuales^2); errorEntrenamiento
  
  # MSE de prueba
  residuales <- test$Apps - predict(modelo,test)
  errorPrueba <- mean(residuales^2); errorPrueba

  remove(modelo,residuales)
}
# b) Regresión Ridge por validación cruzada sobre el mismo conjunto de prueba
{
  x      <- model.matrix (Apps~.,train)
  y      <- train$Apps
  grid <- 10^seq(10,-10,length = 100) # Generación de grid para lambda

  # Regresión Ridge por validación cruzada
  ridge.out<-cv.glmnet(x,y,alpha =0,lambda=grid)
  plot(ridge.out)
  
  # Lambda con el menor error cuadrático de prueba
  bestlam<-ridge.out$lambda.min; bestlam
  
  # MSE de prueba
  residuales <- test$Apps - predict(ridge.out,model.matrix(Apps~. , test) )
  errorRidge <- mean(residuales^2); errorRidge

  remove(ridge.out, x, y, grid,bestlam, residuales)
}
#c) Regresión Lasso por validación cruzada  sobre el mismo conjunto de prueba
{
  x      <- model.matrix (Apps~.,train)
  y      <- train$Apps
  grid <- 10^seq(10,-10,length = 100) # Generación de grid para lambda

  lasso.out<-cv.glmnet(x,y,alpha=1,lambda =grid) #funcion que realiza la validacion cruzada k-fold, por default usa k=10
  plot(lasso.out,main="Lasso") #grafica los MSE para cada valor de lamda
  bestlam<-lasso.out$lambda.min  #elige el valor de lamda que tiene el MSE mas pequeño
  bestlam

  # Número de coeficientes prácticamente nulos
  lasso.coef<-predict(lasso.out,type="coefficients",s=bestlam)
  lasso.coef
  lasso.coef[abs(lasso.coef)< 0.001]
  
  # MSE de prueba
  residuales <- test$Apps - predict(lasso.out,model.matrix(Apps~. , test) )
  errorLasso <- mean(residuales^2); errorLasso
    
  remove(x,y,lasso.coef,lasso.out, bestlam,grid,residuales)
}
# d) Modelo PCR por validación cruzada  sobre el mismo conjunto de prueba
{
  pcr.out<-pcr(Apps~.,data=train,scale=TRUE,validation="CV") 
  summary (pcr.out) #
  validationplot(pcr.out,val.type="MSEP")

  # MSE de prueba
  residuales <- test$Apps - predict(pcr.out, test, ncomp = 3)
  errorPCR <- mean(residuales^2); errorPCR
  
  residuales <- test$Apps - predict(pcr.out, test, ncomp = 5)
  errorPCR <- mean(residuales^2); errorPCR
  
  residuales <- test$Apps - predict(pcr.out, test, ncomp = 6)
  errorPCR <- mean(residuales^2); errorPCR
  

  remove(pcr.out, residuales)
}
# e) Ajuste por PLS por validación cruzada  sobre el mismo conjunto de prueba
{
  pls.out<-plsr(Apps~.,data=test,scale=TRUE,validation ="CV")
  summary(pls.out)
  validationplot(pls.out,val.type="MSEP")
  
  # MSE de prueba
  residuales <- test$Apps - predict(pls.out, test, ncomp = 3)
  errorPLS <- mean(residuales^2); errorPLS
  
  residuales <- test$Apps - predict(pls.out, test, ncomp = 5)
  errorPLS <- mean(residuales^2); errorPLS
  
  residuales <- test$Apps - predict(pls.out, test, ncomp = 6)
  errorPLS <- mean(residuales^2); errorPLS
  
  remove(pls.out, residuales)
}
# f) Comentar sobre errores y limpiar variables
{
  remove(errorEntrenamiento, errorPrueba, errorRidge, errorLasso, errorPCR, errorPLS)
  remove(datos, train, test, n, seleccion)
}
}
# Problema No 2
{
  # a) datos
  {
    p <- 20
    n <- 1000
    error <- rnorm(n,0,0.1)
    x <- matrix(runif(n*p,-1,2),ncol=p)
    betas <- runif(p+1,-2,2)
     # Numero de ceros
    nCeros <- 10 #
    
    indicesDeCeros <- sample(p+1,nCeros)
    betas[indicesDeCeros] <- 0
    matrizDiseno <- cbind(matrix(rep(1,n),ncol=1),x)
    nombresColumnas <- c("(Intercept)",paste0("X",1:p))
    colnames(matrizDiseno) <- nombresColumnas
    y <- matrizDiseno %*% betas + error
    datos <- cbind(y,x)
    colnames(datos)<- c("Y",paste0("X",1:p))
    head(datos)
    datos <- data.frame(datos)
    
    remove(error, y,x)
  }
  # b) División del conjunto de datos
  {
    k <- 100
    seleccion <- sample(n,k)
    train <- datos[seleccion,]
    test  <- datos[-seleccion,]
  }
  # c) y d)
  {
    k <- 20
    models   <- regsubsets(Y~., data = train, nvmax = k,method = c("forward"))
    summary(models)
    errorTrain <- c()
    errorTest  <- c()
    for (i in 1:k) {
      modelo <- coef(models, i)
      residual1 <- train$Y - matrizDiseno[ seleccion,names(modelo)] %*% modelo
      residual2 <-  test$Y - matrizDiseno[-seleccion,names(modelo)] %*% modelo
      errorTrain <- c(errorTrain,mean(residual1^2))
      errorTest  <- c(errorTest, mean(residual2^2))
    }
    plot(errorTrain,xlab="n best-subset",ylab="MSE",main="Error sobre conjunto de entrenamiento")
    plot(errorTest ,xlab="n best-subset",ylab="MSE",main="Error sobre conjunto de prueba")
    
    remove(i, residual1, residual2, errorTrain, modelo)
  }
  # e) Valor de n para el cual el error de prueba es mínimo 
  {
    nBestSubset <- which.min(errorTest); nBestSubset
    
    remove(errorTest)
  }
  # f) Comparación entre el beta original y el del nBest
  {
    modelo <- coef(models,  nBestSubset)
    nbetas <- rep(0,p+1)
    names(nbetas) <- nombresColumnas
    nbetas[names(modelo)] <- modelo
    diferencia <- abs(betas - nbetas)
    # Prácticamente coinciden en
    nombresColumnas[diferencia < 0.001]
    
    remove(nbetas, diferencia, modelo,nBestSubset)
  }
  # g) Diferencia entre beta y el beta del nbest=r
  {
    errores <- c()
    for(i in 1:k) {
      nbetas <- rep(0,p+1)
      names(nbetas) <- nombresColumnas
      modelo <- coef(models, i)
      nbetas[names(modelo)] <- modelo
      nbetas <- betas - nbetas
      errores <- c(errores,sum(nbetas^2))
    }
    plot(errores,xlab="r best set",ylab="norm(betas-betas_r)", main="Diferencia entre beta y el beta del nbest=r")
  
    remove(i, nbetas, modelo, errores)
  }
  
  # Limpieza de variables del ambiente
  {
    remove(n,p,k, nCeros, betas, indicesDeCeros, models)
    remove(datos, train, test, matrizDiseno, nombresColumnas, seleccion)
  }
}
