####################### #
# Tarea 1 - Ejercicio 7 #
####################### #
rm(list = ls())

# a) calculo de las probabilidades en R
# X ~ Binomial(123, 0.31)

#creamos funcion que calcula la probabilidad para un valor
prob_bin <- function(x, n, p){
  #input: x:valor para el cual se quiere conocer la funcion de probabilidad
  #       n: tamaño elementos
  #       p: probabilidad evento
  #output: valor de la funcion de probabilidad
  fprob =  choose(n, x) * p^x *(1-p)^(n-x) #utilizar el operador '<-' tiene ventajas sobre el '='
  return(fprob)
}
# P(X=0)
(prob_0 <- prob_bin(0,123,0.31))

# P(X=123)
(prob_bin(123,123,0.31))

# P(X=62)
(prob_bin(62,123, 0.31))

##
# P(0<=X<=10)
prob_10 <- 0
for(i in 0:10){
  prob_10 <- prob_10 + prob_bin(i, 123, .31)
}
prob_10

# P(0<X<=10)
# P(0<X<=10) = P(0<=X<=10) - P(X=0)

(prob_sin0 <- prob_10 - prob_0)

# P(0<=X<10)
# P(0<=X<10) = P(0<=X<=10) - P(X=10)
(prob_sin10 <- prob_10 - prob_bin(10, 123, 0.31))

##
# P(X<=10)
#Dado que aplica para enteros positivos => P(X<=10) = P(0<=X<=10)
prob_10

# P(X>11)
# P(X>11) = 1- P(X<=11) = 1- [P(X<=10) + P(X=11)]
prob_may11 = 1 - (prob_10 + prob_bin(11, 123, 0.31))
prob_may11

#### Calculo con R

# P(X=0)
dbinom(0, 123, .31)

# P(X=123)
dbinom(123, 123, .31)

# P(X=62)
dbinom(62, 123, .31)

##
# P(0<=X<=10)
pbinom(10, 123, .31)

# P(0<X<=10)
# P(0<X<=10) = P(0<=X<=10) - P(X=0)
pbinom(10, 123, .31) - dbinom(0, 123, .31)

# P(0<=X<10)
# P(0<=X<10) = P(0<=X<=10) - P(X=10)
pbinom(10, 123, .31) - dbinom(10, 123, .31)

##
# P(X<=10)
#Dado que aplica para enteros positivos => P(X<=10) = P(0<=X<=10)
pbinom(10, 123, .31) 

# P(X>11)
# P(X>11) = 1- P(X<=11) = 1- [P(X<=10) + P(X=11)]
1- pbinom(11, 123, .31) 

# Cuantiles parte importante de este ejercicio era el construir tu propia funcion de quantiles
quantile(rbinom(1000, 123, .31))
# te doy 2/3 del ejercicio 