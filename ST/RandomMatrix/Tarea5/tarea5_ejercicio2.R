library(Matrix) # packages con la implementacion de matrices 'sparse'
library(irlba)  # package con la implementacion de Lanczos para matrices esparcidas
beta <- 1       # la beta que señala el tipo de ensemble
set.seed(0)     # fijamos una semilla
n <- 1e9        # dimension de la matriz a simular
alpha <- 10     # el numero magico
m <- 10         # numero de repeticiones
k <- round(alpha*n**(1/3)) #reduciendo dimensionalidad
                           # resultado de que se conoce la distro de las tridiagonales
d.f <- beta*(n: (n - k+ 2)) # la convergencia de los val. prop. se ve dominada por las primeras 'k' filas
maxi.vals.prop <- vector(mode='numeric', length = m)
time1 <- Sys.time()
for (x in 1:m)
{
    i <- 1:k
    j <- 1:k
    H <- sparseMatrix(i,j,x=rep(0,k)) #inicializacion de matriz sparse
    index.diagonal.sup <- as.matrix(cbind( 1:(k-1),2:k))
    index.diagonal <- as.matrix(cbind(1:k, 1:k))
    colnames(index.diagonal.sup) <- colnames(index.diagonal)  <- c('x', 'y')
    H[index.diagonal] <-  rnorm(k)
    H[index.diagonal.sup] <- sapply(d.f ,
                                    function(x)
                                    {
                                        rchisq(n = 1, df=x )**.5
                                    }) # rellenamos la diagonal superior
    H <- (H + t(H))/sqrt(4*n*beta) # hacemos simetrica la matriz y escalamos
    valor <- partial_eigen(H, n=1, symmetric = T,
                           maxit=5000, tol=1e-4) #valor propio más grande
    maxi.vals.prop[x] <- valor$values
}
time2 <- Sys.time()
(time <- time2 - time1)
##################
library(RMTstat)
plot(density(dtw(maxi.vals.prop, beta=1)),
             main = "Mil valores propios más grandes con n =1e9x1e9",
             xlab='', ylab='', col='purple')
legend("topright", legend=c("Distribución Teorica", "Distribución muestral"),
       col=c("orange", "purple"),cex = 0.8, lwd = 0.8)
par(new=TRUE)
plot(density(maxi.vals.prop), col="orange")
