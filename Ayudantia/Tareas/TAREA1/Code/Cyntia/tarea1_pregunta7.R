#Alumna: Ramirez Islas Cynthia Mariangel
#Tarea 1 
#Ejercicio 7

###############################################################################
#Inciso a)Escriba un programa que calcule las probabilidades directamente de la función de masa

#Función que calcula probabilidad de masa de una v.a. Binomial(123,0.31)
#Entrada: "X"  
#Salida: función de distribución, es decir, P[X=x]
distbinom<-function(x){
  n=123
  p=0.31
  
   fx=(factorial(n)/(factorial(x)*factorial(n-x)))*(p^x)*(1-p)^(n-x)
  
  return(fx)
}

#Calulcar las siguientes probabilidades:
#Inciso a I)
#P(X = 0)
distbinom(0)
#P(X = 123)
distbinom(123)
#P(X = 62)
distbinom(62)
#P(X = 62)
distbinom(62)
#Inciso a II)
#P(0 <= X <=10)= F(10)-F(0-)
sum(distbinom(0:10))-distbinom(0)
#P(0 < X <= 10)= F(10)-F(1)
sum(distbinom(0:10))-distbinom(1)
#P(0 <=X < 10)
sum(distbinom(0:9))-distbinom(0)
#Inciso a III)
#P(X > 11)=1-P(x<=10)
1-sum(distbinom(0:10))
#P(X <= 10)
sum(distbinom(0:10))

##################################################################################################
#####Inciso b)Calcule las probabilidades del inciso anterior usando pbinom y dbinom.
#Inciso b I)
#P(X = 0)
dbinom(0,123,.31)
#P(X = 123)
dbinom(123,123,.31)
#P(X = 62)
dbinom(62,123,.31)
#####Inciso b II)
#P(0 <= X <=10)
#Opción 1: Usando pbinom  
pbinom(10,123,.31, TRUE,FALSE)-dbinom(0,123,.31)
#Opción 2: Usando dbinom en ambos 
sum(dbinom(0:10,123,.31))-dbinom(0,123,.31)
#P(0 < X <= 10)= F(10)-F(1)
#Opción 1: Usando pbinom 
pbinom(10,123,.31, TRUE,FALSE)-dbinom(1,123,.31)
#Opción 2: Usando dbinom en ambos
sum(dbinom(0:10,123,.31))-dbinom(1,123,.31)
#P(0 <=X < 10)
#Opción 1: Usando pbinom 
pbinom(9,123,.31, TRUE,FALSE)-dbinom(0,123,.31)
#Opción 2: Usando dbinom en ambos
sum(dbinom(0:9,123,.31))-dbinom(0,123,.31)
#####Inciso b III)
#P(X > 11)=1-P(x<=10)
#Opción 1: Usando pbinom 
1-pbinom(10,123,.31, TRUE,FALSE)
#Opción 2: Usando dbinom 
1-sum(dbinom(0:10,123,.31))
#P(X <= 10)
#Opción 1: Usando pbinom 
pbinom(10,123,.31, TRUE,FALSE)
#Opción 2: Usando dbinom 
sum(dbinom(0:10,123,.31))

########################################################################################
#Escriba un programa en R que calcule los cuantiles de 0.25, 0.5 y 0.75. >Existe alguna
#funcion en R que calcule cuantiles?

#Cuantil 0.25
qbinom(.25,123,.31,TRUE, FALSE)
#Cuantil 0.5
qbinom(.5,123,.31,TRUE, FALSE)
#Cuantil 0.75
qbinom(.75,123,.31,TRUE, FALSE)
#parte importante de este inciso era construir tu propia funcion de cuantiles
#te doy 2.5/3 de este ejercicio  
  
  
  
  