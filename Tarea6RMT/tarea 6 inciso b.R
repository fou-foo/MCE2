f<-function(N,M,beta)
{ 
  if(!(beta == 1 || beta ==2 || beta ==4 ))
  {
    print("beta no valida")
    break
  }
  
  if(beta == 1)
  {
    c<- N/M
    H<- matrix(rnorm(n = N*M,mean = 0,sd = 1),nrow = N,ncol = M )
    H_t<-t(H)
    W<-H%*%H_t
    VP<-eigen(W)$values
    }
  
  
  if(beta == 2)
  {
    c<- N/M
    Real<- rnorm(N*M,mean = 0,sd = 1)
    Imaginaria<- rnorm(N*M,mean = 0,sd = 1)
    H<- matrix(complex(real = Real,imaginary = Imaginaria),nrow = N,ncol = M )
    H_t<- t(matrix(complex(real = Real,imaginary = -Imaginaria),nrow = N,ncol = M ))
    W<-H%*%H_t
    VP<-eigen(W)$values
  }
  
  if(beta == 4)
  {
    library(QZ)
    Real<- rnorm(N*M,mean = 0,sd = 1)
    Imaginaria<- rnorm(N*M,mean = 0,sd = 1)
    A<- matrix(complex(real = Real,imaginary = Imaginaria),nrow = N,ncol = M )
    A_c <- matrix(complex(real = Real,imaginary = -Imaginaria),nrow = N,ncol = M )
    
    Real<- rnorm(N*M,mean = 0,sd = 1)
    Imaginaria<- rnorm(N*M,mean = 0,sd = 1)
    B<- matrix(complex(real = Real,imaginary = Imaginaria),nrow = N,ncol = M )
    B_c<- matrix(complex(real = Real,imaginary = -Imaginaria),nrow = N,ncol = M )
    
    aux1<-cbind(A,B)
    aux2<-cbind(-B_c,A_c)
    H<-rbind(aux1,aux2)
    W<-H%*%H(H)
    VP<-eigen(W)$values
  }
  
  return(VP)
}
set.seed(1)
#######Esta parte dibuja el histograma con la N,M y beta de parametros (c = N/M con N<M)
N<-100
M<-800
beta<-4
L<-array(0,dim = c(length(f(N,M,beta)),0))
for (i in 1:5000) {
  L<-cbind(L,f(N,M,beta))
  print(i)
}
hist(L,breaks = 100,border = T)
library(ggplot2)
qplot(as.vector(L),geom = "freqpoly",breaks = seq(min(L),max(L),50))

###### Hasta aquí

##### Ahora el grafico con puntos y uso la funcion p(VP,beta,N)
p<-function(x,beta,N)
{
  1/(beta*N)*p_MP(x/(beta*N))
}

p_MP<-function(y)
{
  zeta_menos<-(1-c^(-1/2))^2
  zeta_mas<-(1+c^(-1/2))^2
  1/(2*pi*y)*sqrt((y-zeta_menos)*(zeta_mas-y))
}


VP<-f(N=100,M=200,beta=1)
c<-100/200
plot(VP,p(VP,beta = 1,N = 100),pch= 20,col="hotpink",xlim = c(0,1600),ylab = "p(x)")

VP<-f(N=100,M=800,beta=1)
c<-100/800
points(VP,p(VP,beta = 1,N = 100),pch= 20,col="palegreen4")

VP<-f(N=100,M=200,beta=2)
c<-100/200
points(VP/2,p(VP,beta = 2,N = 100)*2,pch= 18,col="paleturquoise")

VP<-f(N=100,M=800,beta=2)
c<-100/800
points(VP/2,p(VP,beta = 2,N = 100)*2,pch= 18,col="yellow3")

VP<-f(N=100,M=200,beta=4)
c<-100/200
points(VP/4,p(VP,beta = 4,N = 100)*4,pch= 4,col="black")

VP<-f(N=100,M=800,beta=4)
c<-100/800
points(VP/4,p(VP,beta = 4,N = 100)*4,pch= 4,col="red")

legend("topright", inset=0,c("c= 1/2 b = 1","c=1/8 b = 1","c= 1/2 b= 2","c= 1/8 b =2","c=1/2 b =4","c=1/8 b=4"), col=c("hotpink","palegreen4","paleturquoise","yellow3","Black","Red"), horiz=F, cex=.6,pch = c(19,19,19,19,4,4),xjust = 0,yjust = 0)

