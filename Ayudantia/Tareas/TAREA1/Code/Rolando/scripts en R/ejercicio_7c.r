#Parte (c)
#Escriba un programa en R que calcule los cuantiles de 0.25, 0.5 y 0.75. 
#which(pbinom(1:n, n, p) > .25) 
#Da la lista de los valores de x en los que p(x) >.25
#Después se toma el primer elemento de dicha lista y se resta 1
library(ggfortify)

n <- 123
p <- .31

#Cuantil .25
which(pbinom(0:n, n, p) > .25)[1]-1

#Cuantil .5
which(pbinom(0:n, n, p) > .5)[1]-1

#Cuantil .75
which(pbinom(0:n, n, p) > .75)[1]-1


#¿Existe alguna función en R que calcule cuantiles?
#Sí, en general quantile, pero para este caso funciona qbinom

#Cuantil .25
qbinom(.25, n,p)

#Cuantil .5
qbinom(.5, n,p)

#Cuantil .75
qbinom(.75, n,p)
