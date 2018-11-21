#María Guadalupe Garrido Espinosa
#Ejercicio 2
#
#Simule 10^4 veces un proceso de Poisson N con lambda= 1/2 y hasta el tiempo t = 1. 
#Haga un histograma de N(1) en su simulación anterior y compare contra la 
#distribución de Poisson correspondiente


sim_proc_poi<-function(distancia,t_final,lambda)
{
  tiempo=seq(distancia,t_final,by=distancia)
  parametro=lambda*distancia
  trayectoria=1:length(tiempo)
  suma=0
  
  #Con la binomial vemos si en el tiempo T sucede o no un éxito
  simular_binomial<-function(n,p){
    res_moneda=c(0:1)
    simulacion=rep(0,n)
    for (i in c(1:n)) 
    {
      simulacion[i]=sample(res_moneda,1,replace = TRUE,prob =c(1-p,p));
      
    }
    returnValue(simulacion);
  }
  
  #El siguiente vector contiene los resultados de la binomial 
  res_binomial=simular_binomial((t_final)/distancia,parametro)
  "
  for(i in 1:length(res_binomial))
  {
    suma=suma+res_binomial[i]
    trayectoria[i]=suma
  }
  
  #Los resultados los ingresamos en un data frame
  trayectoria_f=data.frame(tiempo,trayectoria)
  "
  returnValue(sum(res_binomial)) 
}

#Simulamos el proceso 10^6 trayectorias y guardamos el N(1)
#Todas ellas tienen como t_final=1 y lambda=1/2

num_sim=1000000
sim_poi=c(1:num_sim)*0

for(i in 1:num_sim)
{
  sim_poi[i]=sim_proc_poi(0.001,1,1/2)  
}

#Obtenemos el frame 
frame_sim=as.data.frame(table(sim_poi))

#Hacemos un data frame que tenga las prob de la exponencial
dim_poi=max(as.numeric(as.character(frame_sim$sim_poi)));
intentos=c(0:dim_poi)
poi_dat=dpois(0:dim_poi,1/2)
poi_prob=data.frame(intentos,poi_dat)

#Cambiamos el nombre a las columnnas
colnames(poi_prob)=c("Intentos","Prop_poi")
colnames(frame_sim)=c("Intentos","Prop_exp")

#Hacemos un merge para tener la información condensada
comp=merge(frame_sim,poi_prob, by="Intentos", all.x=TRUE)

#Graficamos las frecuencias y el resultado de la exponencial
plot(as.numeric(as.character(comp$Intentos) ),comp$Prop_exp/num_sim,
     type="h",col="cornflowerblue",xlab = "Lanzamientos",ylab = "Frecuencia",lwd = 3,
     main=paste("Gráfico de frecuencias"))
par(new=TRUE)
points(as.numeric(as.character(comp$Intentos) ),comp$Prop_poi,type="p",col="red")
