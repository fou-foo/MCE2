procesoPoisson<-function(lambda, t)
{
  intervalos <- 1000 # fija el numero de intervalos
  dt <- t/intervalos #optiene el ancho
  tiempos <- seq(0, t , by= dt) #discretiza el dominio
  resultados <- rep(0, intervalos+2)
  for(i in 1:intervalos+1 ){
    resultados[i+1]<- resultados[i] + rbinom(1,1,lambda*dt) #esta es la linea majica
  }
  return(resultados) 
}

tiempos<-seq(0, 10 , by= 10/1000)
set.seed(0)
ppss1 <- procesoPoisson(2,10)
ppss2 <- procesoPoisson(2,10)
ppss3 <- procesoPoisson(2,10)
plot(stepfun(tiempos,ppss1), verticals= FALSE, xlab="tiempo",ylab="ocurrencias", main = "Trayectoria lambda=2 (primera repeticion)" )
plot(stepfun(tiempos,ppss2), verticals= FALSE, xlab="tiempo",ylab="ocurrencias", main = "Trayectoria lambda=2 (segunda repeticion)" )
plot(stepfun(tiempos,ppss3), verticals= FALSE, xlab="tiempo",ylab="ocurrencias", main = "Trayectoria lambda=2 (tercera repeticion)" )


procesoPoisson2<-function(N,lambda,t)
{
  intervalos <- 1000
  dt <- t/intervalos
  tiempos <- seq(0, t , by= dt)
  resultados <- rep(0, intervalos+2)
  for(i in 1:N){
    acum <- 0
    for(i in 1:intervalos +1){
      
      acum= acum + rbinom(1,1,(lambda*dt +10^-6)) 
    }
    resultados[i+1]=acum
    
  }
  return(resultados)
}

simpropois <- procesoPoisson2(N = 10^3, lambda = 2, t =1) #algo te fallo 

#Grafica de las proporciones por la cantidad de aguilas observadas
plot(prop.table(table(simpropois)),
     type="h",
     col="darkred", 
     xlab = "", 
     ylab="", 
     main = "Comparacion entre simulacion y distribucion teorica" )
par(new=TRUE)
#Grafica de la distribucion teorica Geom(p=0.1)
z<-sort(unique(simpropois))
plot((z), dpois(z,0.5),
     type="p",
     axes= F,  
     col="dodgerblue4",
     xlab = "exitos", 
     ylab="Probabilidad" )
legend("topright", inset=.01, title="",
       c("simulacion","Poisson(lambda=1/2)"), fill=c("darkred","dodgerblue4"), horiz=F)

##########################
#######
procesoPoisson.foo <- function(lambda, t)
{
  intervalos <- 10000 # fija el numero de intervalos
  dt <- t/intervalos #optiene el ancho
  tiempos <- seq(0, t , by= dt) #discretiza el dominio
  resultados <- rbinom( intervalos ,1, lambda*dt+ 10**(-6)) #esta es la linea majica
  return(cumsum(resultados)) 
}

set.seed(0)
ppss1 <- procesoPoisson.foo(2,10)
tiempos<-seq(0, 10 , length=length(ppss1)-1  )
plot(stepfun(tiempos,ppss1), verticals= FALSE, xlab="tiempo",ylab="ocurrencias", main = "Trayectoria lambda=2 (primera repeticion)" )
####################################################


#segunda parte
####################
Poisson.foo <- function(lambda, N, t)
{
  lambda <- lambda
  t <- t
  function(N){
    intervalos <- 10000 # fija el numero de intervalos
    dt <- t/intervalos #optiene el ancho
    tiempos <- seq(0, t , by= dt) #discretiza el dominio
    resultados <- rbinom( intervalos ,1, lambda*dt+ 10**(-6)) #esta es la linea majica
    ultimo <- cumsum(resultados)[length(resultados)]
    return(ultimo)
  }
}

set.seed(0)
N <- 10**4
foo <- Poisson.foo(lambda = 0.5, t = 1)
poisoon.final <- mapply(foo, rep(1,N))
plot(table(poisoon.final))
