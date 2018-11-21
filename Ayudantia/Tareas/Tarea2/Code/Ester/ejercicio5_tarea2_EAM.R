
#################### #
#### Ejercicio 5 ### #
#################### #

# Usando las ideas del inciso anterior escriba una funcion en R que simule N veces los lanzamientos
# de moneda hasta obtener r aguilas. La funcion debera recibir como parametros a
# la probabilidad p de obtener aguila, al numero r de aguilas a observar antes de detener el
# experimento y al numero N de veces que se repite el experimento; y tendra que regresar un
# vector de longitud N que contenga el numero de lanzamientos hasta obtener las r ´aguilas en
# cada uno de los N experimentos. 


# Funcion que realiza un experimento generando un numero fijo de lanzamientos
# y selecciona el r-ésimo
one_exp_r <- function(p, r){
  
  lanzamientos <- sample(c(1,0), (1/p)*r*50, replace = T, prob = c(p,1-p))
  first <- which(lanzamientos==1)[r]
  return(first)
  
  
}

######################### # 
#### FUNCION GENERAL #### #
######################### # 

binoneg_lanz <- function(N,p,r){
  resultado <- replicate(N, one_exp_r(p,r))
  return(resultado)
}

# Grafique las frecuencias normalizadas de los experimentos
# para N = 10E6, p = 0.2, 0.1 y r = 2, 7 y comparelos contra la funcion de masa de la distribucion
# mas adecuada para modelar este tipo de experimentos.


res_0.2_2 <-  binoneg_lanz(1000000,.2, 2)
res_0.1_2 <-  binoneg_lanz(1000000,.1, 2)
res_0.2_7 <-  binoneg_lanz(1000000,.2, 7)
res_0.1_7 <-  binoneg_lanz(1000000,.1, 7)


## Gráfica p=.2, r=2,   N=1000000
plot(table(res_0.2_2 )/1000000, col="#000072", ylab="Probabiidad", xlab="Experimentos", 
     main="Distribución de Probabilidad
     Binomial Negativa", sub="N=1,000,000; p=0.2; r=2")
lines(dnbinom(0:max(res_0.2_2),2, .2), col="#8c0000", lty=6, lwd=2)

## Gráfica p=.1, r=2,   N=1000000
plot(table(res_0.1_2 )/1000000, col="#000072", ylab="Probabiidad", xlab="Experimentos", 
     main="Distribución de Probabilidad
     Binomial Negativa", sub="N=1,000,000; p=0.1; r=2")
lines(dnbinom(0:max(res_0.1_2),2, .1), col="#8c0000", lty=6, lwd=2)

## Gráfica p=.2, r=7,   N=1000000
plot(table(res_0.2_7 )/1000000, col="#000072", ylab="Probabiidad", xlab="Experimentos", 
     main="Distribución de Probabilidad
     Binomial Negativa", sub="N=1,000,000; p=0.2; r=7")
lines(dnbinom(0:max(res_0.2_7),7, .2), col="#8c0000", lty=6, lwd=2)

## Gráfica p=.1, r=7,   N=1000000
plot(table(res_0.1_7 )/1000000, col="#000072", ylab="Probabiidad", xlab="Experimentos", 
     main="Distribución de Probabilidad
     Binomial Negativa", sub="N=1,000,000; p=0.1; r=7")
lines(dnbinom(0:max(res_0.1_7),7, .1), col="#8c0000", lty=6, lwd=2)
tus graficas estan desfasadas te doy .9