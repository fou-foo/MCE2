
#################### #
#### Ejercicio 9 ### #
#################### #

library("ggplot2")

# Escriba una funci´on en R que simule una aproximaci´on al proceso Poisson a partir de las 5
# hip´otesis que usamos en clase para construir tal proceso. Usando esta funci´on, simule tres
# trayectorias de un proceso Poisson ?? = 2 sobre el intervalo [0, 10] y grafiquelas. 
# correspondiente.


T=10

dt=T/1000
dt_long=seq(0,10,dt)
lambda=2

#Simulacion de las trayectorias
bernoulli1 <- sample(c(1,0), length(dt_long), replace=T, prob = c(lambda*dt+10e-6,1-lambda*dt+10e-6))
bernoulli2 <- sample(c(1,0), length(dt_long), replace=T, prob = c(lambda*dt+10e-6,1-lambda*dt+10e-6))
bernoulli3 <- sample(c(1,0), length(dt_long), replace=T, prob = c(lambda*dt+10e-6,1-lambda*dt+10e-6))

#Agregar resultados
result <- data.frame(dt=dt_long, datos=cumsum(bernoulli1), Simulacion="Primer trayectoria")
result <- rbind(result,data.frame(dt=dt_long, datos=cumsum(bernoulli2), Simulacion="Segunda trayectoria"))
result <- rbind(result,data.frame(dt=dt_long, datos=cumsum(bernoulli3), Simulacion="Tercer trayectoria"))

#Grafica
qplot(data=result, x=dt, y=datos, color=Simulacion , geom=c("step","point"),
      xlab="Tiempo",ylab="N(t)",main="Simulaciones del Proceso de Poisson" )


# simule 10E4 veces un proceso de Poisson N con ?? = 1/2 y hasta el tiempo t = 1. Haga un
# histograma de N(1) en su simulacion anterior y compare contra la distribucion de Poisson



proceso_poisson <- function(lambda, T_s){
  dt_s=T_s/1000
  dt_long_s=seq(0,T_s,dt_s)
  bernoulli <- sample(c(1,0), length(dt_long_s), replace=T, prob = c(lambda*dt_s,1-lambda*dt_s))
  n_1 <- sum(bernoulli)
  return(n_1)
  
}

output <- replicate(10000,proceso_poisson(.5,1))
plot(table(output)/10000,xlab="Éxitos",ylab="Frecuencia Relativa/Probabilida" )
lines(x=0:5, y=dpois(0:5,.5), col="#8c0000", type = "p", lwd=2)
muy bien 