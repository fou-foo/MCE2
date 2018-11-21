
#################### #
#### Ejercicio 4 ### #
#################### #

# a) Considere una moneda desequilibrada que tiene probabilidad p de obtener aguila. Usando
# el comando sample, escriba una funcion que simule N veces lanzamientos de esta moneda
# hasta obtener un aguila. La funcion debera recibir como parametros a la probabilidad
# p de obtener aguila y al numero N de veces que se repite el experimento; y tendra que
# regresar un vector de longitud N que contenga el numero de lanzamientos hasta obtener
# un aguila en cada uno de los N experimentos.

library(ggplot2)

# Funcion que realiza un experimento generando un numero fijo de lanzamientos
# y selecciona el primero
one_exp <- function(p){
  
  lanzamientos <- sample(c(1,0), (1/p)*100, replace = T, prob = c(p,1-p))
  first <- which(lanzamientos==1)[1]
  return(first)
  
  
}

# start_time <- Sys.time()
# output <- replicate(1000000, one_exp(p))
# end_time <- Sys.time()
# end_time - start_time = 11.48321 mins

# Funcion que realiza un experimento generando lanzamientos a través de un while hasta obtener el primer éxito
one_exp_2 <- function(p){
  
  ind_exito=0
  num_lanz=0
  
  while(ind_exito<1){
    lanzamiento <- sample(c(1,0), 1, replace = T, prob = c(p,1-p))
    num_lanz <- num_lanz+1
    ind_exito <- lanzamiento
    
  }
  return(num_lanz)

}


# start_time <- Sys.time()
# output_2 <- replicate(1000000, one_exp_2(p))
# end_time <- Sys.time()
# end_time - start_time = 31.67937 mins
#Al correr esta funcion tardo mas tiempo por lo cual tomaremos la primera "one_exp"

######################### # 
#### FUNCION GENERAL #### #
######################### # 
geom_lanz <- function(N,p){
  resultado <- replicate(N, one_exp(p))
  return(resultado)
}

# Usando la funcion anterior simule N = 10E4 veces una variable aleatoria Geom(p) para
# p = 0.5, 0.1, 0.01. Grafique las frecuencias normalizadas en color azul. Sobre esta ultima
# figura empalme en rojo la grafica de la funcion de masa correspondiente. ¿Que observa?

result_5  <- geom_lanz(10000,.5 )
result_1  <- geom_lanz(10000,.1 )
result_01 <- geom_lanz(10000,.01)


plot(table(result_5)/10000, col="#000072", ylab="Probabiidad", xlab="Experimentos", 
     main="Distribución de Probabilidad
     Geométrica", sub="N=1000, p=0.5")

lines(dgeom(0:max(result_5), .5), col="#8c0000", lty=6)



# c) Repita el inciso anterior para N = 106
# Ademas calcule el promedio y la desviacion
# estandar de las simulaciones que realizo ¿Que observa?

result_5  <- geom_lanz(1000000,.5 )
result_1  <- geom_lanz(1000000,.1 )
result_01 <- geom_lanz(1000000,.01)

