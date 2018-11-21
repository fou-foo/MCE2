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
    tiempo[i+1]<-(i+1)*dt
  }
  # La suma del vector resultado es el numero de eventos que se tuvieron
  sum(resultado) 
}

simula<- function(N,T,lambda) {
  # N es la cantidad de simulaciones
  # T es tiempo
  # lambda es la tasa de llegadas en el tiempo T
  muestra<- rep(0,times=N)
  for(i in 1:N) 
    muestra[i]<-poisson(T,lambda)
  max.muestra<-max(muestra)
  probabilidad.Empirica<- table(muestra)/N
  probabilidad.Analitica<- dpois(0:(max.muestra-1),lambda = lambda)
  mensaje<- paste("Numero de eventos con tasa lambda = ",lambda)
  plot(1:length(probabilidad.Analitica),probabilidad.Analitica, xlab=mensaje, ylab="Probabilidad",
       main="Comparacion entre experimentacion (azul) y datos analiticos (rojo)",
       type="h", col="red")
  lines(1:length(probabilidad.Empirica)+0.1,probabilidad.Empirica, type="h", col="blue")
  
} 

simula(10000,1,1/2)
bien
