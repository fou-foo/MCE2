#0 Sol, 1 Aguila
lanza<- function(p){
  # Recibe como parametro la probabilidad de obtener aguila
  moneda<-c(0,1)
  probabilidad<-c(1-p,p)
  # Regresa la suma de sacar 1 elemento, es decir 0 si es Sol, 1 si es Aguila
  sum(sample(moneda,1,replace=TRUE,prob=probabilidad))
}

obten.Aguilas<- function(r,p) {
  # r es la cantidad de aguilas que queremos, p la probabilidad de obtener 1 aguila
  cantidad.Lanzamientos<-0
  cantidad.Aguilas<-0
  while(cantidad.Aguilas!=r) {
    cantidad.Aguilas<- cantidad.Aguilas+lanza(p)
    cantidad.Lanzamientos<- cantidad.Lanzamientos+1
  }
  # regresa la cantidad de tiros necesarios para obtener r aguilas
  cantidad.Lanzamientos
}

simula<- function(N,r,p) {
  # N es la cantidad de experimentos que queremos hacer
  # r la cantidad de aguilas que queremos
  # p la probabilidad de obtener 1 aguila
  ans<-rep(0,times=N) # ans lo inicializo como un vector del tamaño adecuado
  for(i in 1:N ) 
    ans[i]<-obten.Aguilas(r,p) # conforme se obtiene el resultado lo guardo 
                               # en su posicion correspondiente
  ans
} 
do.comparison <- function(N,r,p) {
  muestra<-simula(N,r,p)
  max.muestra<-max(muestra)
  probabilidad.Empirica<- table(muestra)/N
  probabilidad.Analitica<- dnbinom(0:(max.muestra-1),size=r,prob=p)
  mensaje<- paste("Cantidad de tiros hasta obtener ",r, " aguilas", "con probabilidad p= ",p)
  plot(1:length(probabilidad.Analitica),probabilidad.Analitica, xlab=mensaje, ylab="Probabilidad",
       main="Comparacion entre experimentacion (azul) y datos analiticos (rojo)",
       type="h", col="red")
  lines(1:length(probabilidad.Empirica)+0.1,probabilidad.Empirica, type="h", col="blue")
}
do.comparison(1000000,2,0.2)
do.comparison(1000000,7,0.1)
do.comparison(1000000,2,0.2)
do.comparison(1000000,7,0.1)



