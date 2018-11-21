# n y p son los parametros de una binomial B(n,p)
n<-123
p<-0.31
# Implementacion de la funcion de densidad de la binomial
# Recibe x (int) como parametro y calcula su evaluacion
# en la funcion de densidad de la binomial con parametros n y p
get.Prob<-function(x) {
  ans<-choose(n,x)*(p^x)*(1.0-p)^(n-x)
  ans
}

# Obtencion del primer bloque de probabilidades
# evaluando directamente en get.Prob
# P(X = 0)
ans1<-get.Prob(0) 
print( paste("P(X = 0) = ", ans1), quote=FALSE)
# P(X = 123)
ans2<-get.Prob(123) 
print( paste("P(X = 123) = ", ans2), quote=FALSE)
# P(X = 62)
ans3<-get.Prob(62)
print( paste("P(X = 62) = ", ans3), quote=FALSE)

# Obtencion del segundo bloque de probabilidades 
# sacando la suma de evaluar un rango en get.Prob
# P(0 <= X <= 10)
ans4 <- sum( get.Prob(0:10) )
print( paste("P(0 <= X <= 10) = ", ans4), quote=FALSE)
# P(0 < X <= 10)
ans5 <- sum( get.Prob(1:10) )
print( paste("P(0 < X <= 10) = ", ans5), quote=FALSE)
# P(0 <= X < 10)
ans6 <- sum( get.Prob(0:9) )
print( paste("P(0 <= X < 10) = ", ans6), quote=FALSE)

# Obtencion del tercer bloque de probabilidades
# mediante la suma de get.Prob en un rango en la segunda
# y mediante complemento en la primera
# P(X > 11) = 1- P(X <= 11)
ans7 <- 1-sum( get.Prob(0:11) )
print( paste("P(X > 11) = ", ans7), quote=FALSE)
# P(X <= 10)
ans8 <- sum( get.Prob(0:10) )
print( paste("P(X <= 10) = ", ans8), quote=FALSE)
#bien
