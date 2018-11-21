rm(list=ls())
library("ggplot2")
#Funcion que simula el lanzamiento de una moneda
lanzamiento <- function(p){
  resultado <- sample(c(0,1), size = 1, prob =c(1-p,p))
  return(resultado)
}
#Simula un proceso de Poisson a travÃ©s de subdivisiones
simulacion_poisson <- function(lambda, x_max, n_divisiones) {
  longitud <- x_max / n_divisiones
  p <- lambda * longitud + 10^-6
  simulacion <- replicate(n_divisiones, lanzamiento(p))
  s_acumulada <- data.frame((1:n_divisiones)*longitud, cumsum(simulacion))
  names(s_acumulada) <- c("x", "N")
  return(s_acumulada)
}


#Muestra la trayectoria de un proceso de Poisson
trayectoria_poisson <- function(lambda, x_max, n_divisiones) {
  s_acumulada <- simulacion_poisson(lambda, x_max, n_divisiones)
  ggplot() +
    geom_step(data=s_acumulada, mapping=aes(x=x, y=N)) +
    labs(title = paste("Proceso de Poisson homogeneo con lambda = ", lambda)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
}

lambda <- 2
x_max <- 10
n_divisiones <- 1000
#Trayectoria1
trayectoria_poisson(lambda,x_max, n_divisiones)
#Trayectoria2
trayectoria_poisson(lambda,x_max, n_divisiones)
#Trayectoria3
trayectoria_poisson(lambda,x_max, n_divisiones)
# y la segunda sección del  9 ??