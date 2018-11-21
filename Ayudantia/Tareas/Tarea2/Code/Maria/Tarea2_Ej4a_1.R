# Tarea 2, Ejercicio 4
# María Guadalupe Garrido Espinosa


#p ¿Qué tan cargada está la moneda 
#n ¿Cuántas repeticiones quieres?

sim_geom<-function(p){

  conteo=0;#We initialize the counter in 1 recuerda lo que vimos del operador '<-' y no es necesario ';' en cada linea
  res_moneda=c(1:2)#1 es el sello, 2 el águila
  continua=1;#Give a value to enter to the while
#  simulacion=rep(0,n);#Inicializamos el vector de ceros de tamaño n
  
  while( continua!=0)
  {
    res_lanzam=sample(res_moneda,1,replace = TRUE,prob =c(1-p,p))#Simulamos lanzamiento
    if(res_lanzam==2)
      {#Evaluamos si el resultado fue águila
        continua=0;conteo=conteo+1
      }
    else 
      {#Si fue sello seguimos tirando
        continua=1;conteo=conteo+1}
      }
  returnValue(conteo);
}

simular<-function(n,p){
  simulacion=rep(0,n)
  suma=0;
  for (i in c(1:n)) 
  {
    simulacion[i]=sim_geom(p)
    suma=suma+simulacion[i];
  }
  
  cat("El promedio es: ",suma/n);
  cat("\n");
  cat("La desviación estándar es: ",sqrt(sum((simulacion-(suma/n))^2)/(n-1)) )
  cat("\n");
  returnValue(simulacion);
  
}

obtener_grafico <-function(n,p){ 
  
  simulacion=simular(n,p)#Repetimos el experimento 10^4
  
  resultados=data.frame(table(simulacion)) #contamos para obtener las frecuencias
  
  #Hacemos un data frame que tenga las prob de la geométrica 
  dim_geom=max(as.numeric(as.character(resultados$simulacion)));
  intentos=c(0:dim_geom)
  geo_dat=dgeom(0:dim_geom,p)
  geom_prob=data.frame(intentos,geo_dat)
  
  #Cambiamos el nombre a las columnnas
  colnames(geom_prob)=c("Intentos","Prop_geom")
  colnames(resultados)=c("Intentos","Prop_exp")
  
  geom_prob$Intentos=geom_prob$Intentos+1;
  #Hacemos un merge para tener la información condensada
  comp=merge(resultados,geom_prob, by="Intentos", all.x=TRUE)
  
  #Graficamos las frecuencias y el resultado de la geométrica
  plot(as.numeric(as.character(comp$Intentos) ),comp$Prop_exp/n,
        type="h",col="cornflowerblue",xlab = "Lanzamientos",ylab = "Frecuencia",lwd = 3,
        main=paste("Frecuencias normalizadas con p=",p))
  par(new=TRUE)
  points(as.numeric(as.character(comp$Intentos) ),comp$Prop_geom,type="p",col="red")
}

#Realizamos simulaciones para las distintas p solicitadas 10^4
obtener_grafico(10000,0.3);
obtener_grafico(10000,0.5)
obtener_grafico(10000,0.1)
obtener_grafico(10000,0.01)

#¿Qué oberva?
#En general, esta forma de simular una variable geométrica es buena
#aunque cuando p=0.01 tiende a sobreestimar poco las probabilidades.

siempre que simules fija una semilla para hacerlo reproducible
set.seed(0)
#Realizamos simulaciones para las distintas p solicitadas 10^6
obtener_grafico(1000000,0.3);
obtener_grafico(1000000,0.5)
obtener_grafico(1000000,0.1)
obtener_grafico(1000000,0.01)

#¿Qué oberva?
#Cuado se incrementa el número de simulaciones a 10^6, prácticamente la
#sobreestimación desaparece.
#Cuando la probabilidad es más pequeña y el número de simulaciones mayor, 
#la cola de la distribución tiende a ser más pesada
ok 