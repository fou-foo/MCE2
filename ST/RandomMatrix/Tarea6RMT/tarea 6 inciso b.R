
f.densidad <-function(N,M,beta)
{ 
  # Entradas:
      # N (numeric): numero de variables 
      # M (numeric): numero de observaciones
      # beta(numeric): indica el ensamble LOE (1), LSE(4) y (2) LUO
  if(beta == 1)
  {
    #Caso LOE
    c <- N/M # la razon entre las dimensiones de la matriz
    H <- matrix(rnorm(n = N*M,mean = 0,sd = 1),nrow = N,ncol = M ) #construimos la matriz densa
    H <- H%*%t(H) # la hacemos simetrica
    VP <- eigen(W)$values
    }
  if(beta == 2)
  {
    #CASO LUE
    c <- N/M
    Real <- rnorm(N*M,mean = 0,sd = 1)
    Imaginaria <- rnorm(N*M,mean = 0,sd = 1)
    H<- matrix(complex(real = Real,imaginary = Imaginaria),nrow = N,ncol = M )
    H_t<- t(matrix(complex(real = Real,imaginary = -Imaginaria),nrow = N,ncol = M ))
    W <- H%*%(H_t)
    VP <- eigen(W)$values
  }
  if(beta == 4)
  {
    # caso LSE
    library(QZ)
    Real <- rnorm(N*M,mean = 0,sd = 1)
    Imaginaria <- rnorm(N*M,mean = 0,sd = 1)
    A <- matrix(complex(real = Real,imaginary = Imaginaria),nrow = N,ncol = M )
    A_c <- matrix(complex(real = Real,imaginary = -Imaginaria),nrow = N,ncol = M )
    
    Real <- rnorm(N*M,mean = 0,sd = 1)
    Imaginaria <- rnorm(N*M,mean = 0,sd = 1)
    B <- matrix(complex(real = Real,imaginary = Imaginaria),nrow = N,ncol = M )
    B_c <- matrix(complex(real = Real,imaginary = -Imaginaria),nrow = N,ncol = M )
    
    aux1 <- cbind(A,B)
    aux2 <- cbind(-B_c,A_c)
    H <- rbind(aux1,aux2)
    W <- H%*%H(H)
    VP <- eigen(W)$values
  }
  return(VP)
}
set.seed(0)
#######Esta parte dibuja el histograma con la N,M y beta de parametros (c = N/M con N<M)
N <- 100
M <- 800
beta <- 4
m <- f.densidad(N, M, beta)
L <- array(0,dim = c(length(M),0))
for (i in 1:5000) 
{
  m2 <- f.densidad(N,M,beta)
  L <- cbind(L, m2)
  #print(i)
}
hist(L,breaks = 100,border = TRUE)
library(ggplot2)
qplot(as.vector(L),geom = "freqpoly",breaks = seq(min(L),max(L),50))

###### Hasta aqu?

##### Ahora el grafico con puntos y uso la funcion p(VP,beta,N)
p <- function(x,beta,N)
{
  # x (numeric): soporte
  # N (numeric): 
  1/(beta*N)*p_MP(x/(beta*N))
}
p_MP <- function(y)
{
  # soporte de la distribucion 
  zeta_menos <- (1-c^(-1/2))^2
  zeta_mas <- (1+c^(-1/2))^2
  a <- 1 / (2*pi*y) * sqrt( ( y-zeta_menos)*(zeta_mas - y))
  return(a)
}
set.seed(0)
VP <- f.densidad(N=100,M=200,beta=1)
c <- 100/200
plot(VP/100, p(VP,beta = 1,N = 100)*100, pch= 20,col="hotpink",xlim = c(-1,16),ylab = "p(x)", ylim=c(0,.5))
VP <- f.densidad(N=100,M=800,beta=1)
c <- 100/800
points(VP/100, p(VP,beta = 1,N = 100)*100, pch= 20,col="palegreen4")
VP <- f.densidad(N=100,M=200,beta=2)
c <- 100/200
points(VP/200, p(VP,beta = 2,N = 100)*200, pch= 18,col="paleturquoise")
VP <- f.densidad(N=100,M=800,beta=2)
c <- 100/800
points(VP/200,p(VP,beta = 2,N = 100)*200,pch= 18,col="yellow3")
VP <- f.densidad(N=100,M=200,beta=4)
c <- 100/200
x <- 0
points(c(0, VP/400, 7), p(c(0, VP, 7),beta = 4,N = 100)*400, pch= 4,col="blue", type='l')
VP <- f.densidad(N=100,M=800,beta=4)
c <- 100/800
points(VP/400, p(VP,beta = 4,N = 100)*400, pch= 4,col="red", type='l')
legend("topright", inset=0,
       c("b = 1"," b = 1","b= 2"," b =2","c=1/2 b =4","c=1/8 b=4"),
       col=c("hotpink","palegreen4","paleturquoise","yellow3","blue","red"),
       horiz=F, cex=.6,pch = c(19,19,19,19,4,4),xjust = 0,yjust = 0)
