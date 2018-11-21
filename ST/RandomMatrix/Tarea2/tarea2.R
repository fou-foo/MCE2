setwd("C:/Users/fou-f/Desktop/Third/ST/RandomMatrix/Tarea2")
dir()


Wigner.semi.circulo.init2 <- function(N, caso)
{
  # Entradas
  ## N (int) dimension de la matriz
  #Regreza uuna funcion para simular obetener los vect. propis de una matriz
    caso <- caso
  function(N)
  {
    if(caso=='GOE')
    {
      matriz <- matrix(rnorm(N**2), ncol = N)
      matriz <- (matriz + t(matriz)) / 2
      val.pro <- eigen(matriz)$values
    } 
    return(abs(diff(val.pro)))
  }
}
set.seed(0)
N <- 10
M <- 10000
GOE <- Wigner.semi.circulo.init2(caso = 'GOE')
muestra.GOE <- mapply(FUN = GOE, rep(N, M) )
#dim(muestra.GOE) #en cada columna hay una realizacion de la simulacion
dim(muestra.GOE) <- c(1, dim(muestra.GOE)[1]* dim(muestra.GOE)[2])
s <- data.frame(GOE.x = t(muestra.GOE))
ggplot(s, aes(x = GOE.x)) + 
  geom_histogram(aes(y=..density..,fill=I('purple')))+
  theme_minimal()  + ylab('densidad') + xlab('') + 
  ggtitle('M simulaciones, N=10 caso GOE') + 
  stat_function(fun=function(x) (pi*x/2)*exp(-pi*x**2/4), colour='orange')

###################################
GUE <- Wigner.semi.circulo.init(caso = 'GUE')
muestra.GUE <- mapply(FUN = GUE, rep(N, M) )
dim(muestra.GUE) #en cada columna hay una realizacion de la simulacion
dim(muestra.GUE) <- c(1, dim(muestra.GUE)[1]* dim(muestra.GUE)[2])
muestra.GUE <- muestra.GUE * (1/(2*N)**(.5))  #correcion
hist(muestra.GUE)
#############################
###################################
GSE <- Wigner.semi.circulo.init(caso = 'GSE')
muestra.GSE <- mapply(FUN = GSE, rep(N, M) )
dim(muestra.GSE) #en cada columna hay una realizacion de la simulacion
dim(muestra.GSE) <- c(1, dim(muestra.GSE)[1]* dim(muestra.GSE)[2])
muestra.GUE <- muestra.GUE * (1/(4*N)**(.5)) 
hist(muestra.GUE)
