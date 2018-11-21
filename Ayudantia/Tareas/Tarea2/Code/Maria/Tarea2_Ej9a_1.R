#María Guadalupe Garrido Espinosa
#Ejercicio 2
#
#Escriba una función en R que simule una aproximación al proceso Poisson a 
#partir de las 5 hipótesis que usamos en clase para construir tal proceso. 
#Usando esta función, simule tres trayectorias de un proceso Poisson con lambda= 2 
#sobre el intervalo [0,10] y grafíquelas


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
  
  for(i in 1:length(res_binomial))
  {
    suma=suma+res_binomial[i]
    trayectoria[i]=suma
  }
  
  #Los resultados los ingresamos en un data frame
  trayectoria_f=data.frame(tiempo,trayectoria)
 
  returnValue(trayectoria_f) 
}


#Generamos 3 trayectorias
tray_1=sim_proc_poi(0.001,10,2)

tray_2=sim_proc_poi(0.001,10,2)

tray_3=sim_proc_poi(0.001,10,2)

#Graficamos las trayectorias
plot(stepfun(tray_1$tiempo,c(0,tray_1$trayectoria)),verticals = FALSE,
     main=expression(paste("Trayectorias de un Proceso de Poisson con ",lambda," = 2")),
     xlab = "Tiempo",ylab = "Éxitos",col="cornflowerblue",lwd = 5, , ylim=c(0,25))
par(new=TRUE)
plot(stepfun(tray_2$tiempo,c(0,tray_2$trayectoria)),col="red",verticals = FALSE,
     main="",
     xlab = "",ylab = "",lwd = 5, ylim=c(0,25))
par(new=TRUE)
plot(stepfun(tray_3$tiempo,c(0,tray_3$trayectoria)),col="brown4",verticals = FALSE,
     main = "",
     xlab = "",ylab = "",lwd = 5, ylim=c(0,25))


bien
