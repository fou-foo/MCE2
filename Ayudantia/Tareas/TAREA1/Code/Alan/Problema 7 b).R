# n y p son los parametros de una binomial B(n,p)
n <- 123
p <- 0.31

# Obtencion del primer bloque de probabilidades
# evaluando directamente con la funcoin dbinom
# P(X = 0)
ans1 <- dbinom(0, n, p)
print( paste("P(X=0) = ", ans1), quote=FALSE)
# P(X = 123)
ans2 <- dbinom(123, n, p)
print( paste("P(X=123) = ", ans2), quote=FALSE)
# P(X = 62)
ans3 <- dbinom(62, n, p)
print( paste("P(X=62) = ", ans3), quote=FALSE)

# Obtencion del segundo bloque de probabilidades 
# usando la funcion pbinom(k,n,p) que por default da la probabilidad
# P(X <= k )
# P(0 <= X <= 10)
ans4<-pbinom(10, n, p)
print( paste("P(0 <= X <= 10) = ", ans4), quote=FALSE)
# P(0 < X <= 10) = P( X <= 10) - P( X = 0) 
ans5<-pbinom(10, n, p)-dbinom(0,n,p)
print( paste("P(0 < X <= 10) = ", ans5), quote=FALSE)
# P(0 <= X < 10) = P (X <= 9
ans6<-pbinom(9, n, p)
print( paste("P(0 <= X < 10) = ", ans6), quote=FALSE)

# Obtencion del tercer bloque de probabilidades
# mediante el uso de pbinom(k,n,p,lower.tail = FALSE) que
# nos da el valor de P(X > k)
# P(X > 11)
ans7<-pbinom(11, n, p, lower.tail = FALSE)
print( paste("P(X > 11) = ", ans7), quote=FALSE)
# P(X <= 10)
ans8<-pbinom(10, n, p)
print( paste("P(X <= 10) = ", ans8), quote=FALSE)
print("Se aprecian una diferencia minima entre usar una funcion propia",quote=FALSE)
print("y la implementada por r",quote=FALSE)
#bien



