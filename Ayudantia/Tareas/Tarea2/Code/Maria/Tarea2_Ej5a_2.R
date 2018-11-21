# Tarea 2, Ejercicio 5
# María Guadalupe Garrido Espinosa
#

#p ¿Qué tan cargada está la moneda 
#n ¿Cuántas repeticiones quieres?

sim_geom<-function(p,r){
  
  conteo=0;#It counts the number of flips until an eagle
  res_moneda=c(1:2)#1 es el sello, 2 el águila
  continua=1;#Give a value to enter to the while
  raguilas=0;#Itinialice the number of eagle to 0
  
  while( continua!=0)
  {
    res_lanzam=sample(res_moneda,1,replace = TRUE,prob =c(1-p,p))#Simulamos lanzamiento
    #cat("Lanzamiento",res_lanzam,"\n")
    
     if(res_lanzam==2)
      {#Evaluamos si el resultado fue águila y contamos cuántas águilas llevamos
        conteo=conteo+1; raguilas=raguilas+1;
        
        if( raguilas == r)#Si ya aparecieron r aguilas termina el experimento
        {continua=0;}
        else
        {continua=1}
        #cat("Es éxito y van ",raguilas," aguilas y los intentos van en ",conteo,"\n")
      }
    else 
    {#Si fue sello seguimos tirando
      continua=1;conteo=conteo+1
      #cat("Fracaso y los intentos van en ",conteo,"\n")
    }
  }
  returnValue(conteo);
}

simular<-function(n,p,r){
  simulacion=rep(0,n)
  suma=0;
  for (i in c(1:n)) 
  {
    simulacion[i]=sim_geom(p,r)
    suma=suma+simulacion[i];
  }
  
  cat("El promedio es: ",suma/n);
  cat("\n");
  cat("La desviación estándar es: ",sqrt(sum((simulacion-(suma/n))^2)/(n-1)) )
  returnValue(simulacion);
}

obtener_grafico <-function(n,p,r){ 
  
  simulacion=simular(n,p,r)#Repetimos el experimento 10^4
  
  resultados=data.frame(table(simulacion)) #contamos para obtener las frecuencias
  
  #Hacemos un data frame que tenga las prob de la binomial negativa 
  dim_bneg=max(as.numeric(as.character(resultados$simulacion)));
  intentos=c(0:dim_bneg)
  bneg_dat=dnbinom(0:dim_bneg,r,p)
  bneg_prob=data.frame(intentos,bneg_dat)
  
  #Cambiamos el nombre a las columnnas
  colnames(bneg_prob)=c("Intentos","Prop_bneg")
  colnames(resultados)=c("Intentos","Prop_exp")
  
  bneg_prob$Intentos=bneg_prob$Intentos+r;
  #Hacemos un merge para tener la información condensada
  comp=merge(resultados,bneg_prob, by="Intentos", all.x=TRUE)
  
  #Graficamos las frecuencias y el resultado de la geométrica
  plot(as.numeric(as.character(comp$Intentos) ),comp$Prop_exp/n,
       type="h",col="cornflowerblue",xlab = "Lanzamientos",ylab = "Frecuencia",lwd = 3,
       main=paste("Frecuencias normalizadas con p=",p))
  par(new=TRUE)
  points(as.numeric(as.character(comp$Intentos) ),comp$Prop_bneg,type="p",col="red")

}


test_vector=obtener_grafico(10000,0.2,2)
test_vector=obtener_grafico(10000,0.2,7)
test_vector=obtener_grafico(10000,0.1,2)
test_vector=obtener_grafico(10000,0.1,7)

#La función de masa más adecuada para modelar este tipo de experimentos es 
#una binomial negativa
ok, lento pero bien 