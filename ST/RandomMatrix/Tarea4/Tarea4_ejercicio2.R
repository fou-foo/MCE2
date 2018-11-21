rm(list = ls())
set.seed(0) # fijamos una semilla
n <- 10     # fijamos las dimensiones de la matriz
    #Construimos una matriz simetrica GOE
M <- matrix( rnorm(n*n), ncol = n, nrow = n )
H <- (M +t(M))/2
round(H, 2)
library(pracma) # utilizamos un algoritmo poco preciso para calcular la
                # eigen descomposicion
eigen <- eigjacobi(H)
X <- eigen$D
O <- eigen$V
Jacobiano <- matrix( rep (0, (n*(n+1)/2)**2),
                          ncol =  n*(n+1)/2, nrow = n*(n+1)/2 )
epsilon <- 1e-3 # fijamos la perturbacion
columna <- 1    # un contador para iterar facilmente
for (i in 1:n)
{
    for(j in i:n)
    {
        E <- diag(rep(0, n))     # generamos la matriz de perturbacion por cada entrada
        E[i, j] <- E[j, i] <- 1  # de la matriz simetrica
        H.prima <- H + epsilon*E # perturbamos
        eigen.aux <- eigjacobi(H.prima) # nueva eigen-descomposicion
        X.prima <- eigen.aux$D
        O.prima <- eigen.aux$V
        d.X <- (X - X.prima)/epsilon    # val. prop. perturbados
        d.O <- (t(O) %*% (O.prima - O) )/epsilon # vect. prop. perturbados
        Jacobiano[1:n, columna] <- d.X    # guardamos las perturbaciones de cada entrada
        Jacobiano[ (n+1):( n*(n+1)/2 ), columna] <- d.O[upper.tri(d.O)]
        columna <- columna + 1
    }
}
#########################
library(matrixcalc)
empirico <- abs( det(Jacobiano) ) # calculamos el valor absoluto de la matriz-Jacobiano que simulamos
(teorico <- 1/abs( det( vandermonde.matrix(X, n) ) )) # calculamos el determinante de vandermonde de los val.prop
(error <- abs( (teorico-empirico) / (teorico ))) # Calculo del error relativo

