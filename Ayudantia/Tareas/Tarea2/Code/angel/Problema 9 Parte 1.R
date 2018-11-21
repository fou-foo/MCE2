bernoulli<- function(p) {
  # p es la probabilidad de obtener un exito
  experimento<-c(0,1)
  probabilidad<-c(1-p,p)
  # sum es 1 solo si se escogio el segundo elemento, con una probabilidad de p
  #Luego se regresa 0 en caso de fracaso y 1 en caso de exito
  sum(sample(experimento,1,replace=TRUE,prob=probabilidad))
}

poisson<- function(T,lambda) {
  # T es la cantidad de Tiempo
  # Lambda es la tasa de llegadas estimada para el tiempo T
  partes<-1000
  dt<-T/partes
  epsilon<-1/1000000
  # Resultado tiene el resultado de hacer un experimento bernoulli 
  # con probabilidad de exito proporcional al tamanio del intervalo
  # es decir lambda/partes
  # en tiempo se guarda la particion del tiempo T  
  resultado<-rep(0,times=partes)
  tiempo<-rep(0,times=partes)
  for(i in 1:partes)
    resultado[i]<-bernoulli(lambda/partes+epsilon)
  for(i in 0:(partes-1)) {
    if(resultado[i+1]==1) {
      print(c("Aparicion en el momento ",(i+1)*dt),quote=FALSE)
    }
    tiempo[i+1]<-(i+1)*dt
  }
  mensaje<-paste("Trayectoria Poisson con lambda = ",lambda,"en un tiempo T = ",T)
  # acumuludado guarda el cantidad total de exitos a lo largo del tiempo
  acumulado<-cumsum(resultado)
  plot(x=tiempo,y=acumulado,xlab="Tiempo",ylab="Numero Eventos",main=mensaje,type="s")
}
poisson(10,2)
poisson(10,2)
poisson(10,2)
