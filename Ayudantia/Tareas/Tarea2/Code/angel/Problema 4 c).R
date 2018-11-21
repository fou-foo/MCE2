#0 Sol, 1 Aguila
lanza<- function(p){
  # Recibe como parametro la probabilidad de obtener aguila
  moneda<-c(0,1)
  probabilidad<-c(1-p,p)
  # Regresa la suma de sacar 1 elemento, es decir 0 si es Sol, 1 si es Aguila
  sum(sample(moneda,1,replace=TRUE,prob=probabilidad))
}

obten.Aguila<- function(p) {
  # p es la probabilidad de obtener aguila
  cantidad.Lanzamientos<-1
  while(lanza(p)!=1) {
    cantidad.Lanzamientos<- cantidad.Lanzamientos+1
  }
  # Regresa la cantidad de lanzamientos necesarios para obtener un aguila
  cantidad.Lanzamientos
}

simula<- function(N,p) {
  # N es la cantidad de experimentos a realizar, y p la probabilidad de obtener aguila
  ans<-rep(0,times=N) # ans lo inicializo como un vector del tamaño adecuado
  for(i in 1:N ) 
    ans[i]<-obten.Aguila(p) # conforme se obtiene el resultado lo guardo 
                            # en su posicion correspondiente
  ans
} 
do.comparison <- function(N,p) {
  muestra<-simula(N,p)
  max.muestra<-max(muestra)
  probabilidad.Empirica<- table(muestra)/N
  probabilidad.Analitica<- dgeom(0:(max.muestra-1),p)
  mensaje<- paste("Cantidad de tiros hasta obtener 1 aguila", "con probabilidad p= ",p)
  plot(1:length(probabilidad.Analitica),probabilidad.Analitica, xlab=mensaje, ylab="Probabilidad",
       main="Comparacion entre experimentacion (azul) y datos analiticos (rojo)",
       type="h", col="red")
  lines(1:length(probabilidad.Empirica)+0.1,probabilidad.Empirica, type="h", col="blue")
  print(c("En este caso p vale ",p),quote=FALSE)
  promedio<-mean(muestra)
  desviacion<-sd(muestra)
  print(c("El promedio es ",promedio),quote=FALSE)
  print(c("La desviacion estandar es ",desviacion),quote=FALSE)
}


do.comparison(1000000,0.5)
do.comparison(1000000,0.1)
do.comparison(1000000,0.01)
print("Observo que conforme N crece, los datos experimentales se ajustan mejor a los datos analiticos")

