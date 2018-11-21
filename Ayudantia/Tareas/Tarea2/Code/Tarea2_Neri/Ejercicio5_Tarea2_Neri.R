# Inferencia Estadistica
# Tarea02_Ejercicio05
# Por Felipe Neri Chairez Cuellar
#
lanzamientos = function (N,r,p){
vector<-rep(0,times=N)     #Genera un vector 0 inicializado de longitud N
moneda<-c("aguila","sol")  #Resultados de la moneda
  for (i in 1:N){
    k<-0      #Se reinicia el contador antes de entrar al ciclo while
    
    while (k<r){
     #resultado_moneda simula el resultado de un lanzamiento con probabilidad p de exito
      resultado_moneda <- sample(moneda,size=1,replace=TRUE,prob=c(p,1-p))
      vector[i]<-vector[i]+1   #Contador del numero de lanzamientos por experimento
      if(resultado_moneda=="aguila"){
        k<-k+1    #Contador del numero de exitos (aguilas)
      }
    }
  }
return (vector)  
}

#  Genera la grafica de las frecuencias normalizadas de los experimentos.
# -Sintaxis: 
#  lanzamientos( N , r , p )
#  Considerando parametros N=10^6, r=2, p=0.1 :
x<-lanzamientos(1000000,2,0.1) #Guarda la funcion en una variable auxiliar
tabla<-prop.table(table(x))    #Genera tabla de porcentajes del total de experimentos
plot(tabla,ylab ='Frecuencia normalizada',xlab='Espacio muestral',col="blue")
# Distribucion Binomial Negativa
# Genera una simulacion de la distribucion binomial negativa con la funcion dnbinom
dbn<-dnbinom(seq(min(x):max(x)),size=2,prob=0.1)
lines(dbn,type="p",col="black") #grafica de puntos color negro


