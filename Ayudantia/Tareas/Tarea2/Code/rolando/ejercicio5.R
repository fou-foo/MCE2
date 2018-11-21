rm(list=ls())
library("ggplot2")
library(scales)

#Funcion que simula el lanzamiento de una moneda
lanzamiento <- function(p){
  resultado <- sample(c(0,1), size = 1, prob =c(1-p,p))
  return(resultado)
}
#Realiza lanzamientos hasta obtener r 1s
#Regresa el número de lanzamientos realizados
experimento <- function(p,r){
  l_totales <- 0
  l_acertados <-0
  while(l_acertados < r ) {
    if(lanzamiento(p) == 1) {
      l_acertados <- l_acertados + 1
    }
    l_totales <- l_totales + 1
  }
  return (l_totales)
}

#Funcion que repite la funcion experimento(p) N veces
#Regresa un vector con el numero de lanzamientos por cada simulacion
simula_lanzamientos <- function(p,r,N) {
  return(replicate(N, experimento(p,r)))
}

#Grafica la funcion de densidad de una distribucion
grafica_densidad <- function(dproba) {
  grafica <- ggplot(dproba, aes(x=x, y=fx, xend = x, yend = 0)) +
    geom_segment(size=1, colour = "darkturquoise") +
    geom_point(size = .5, shape = 1, colour = "darkturquoise") +
    scale_x_discrete(breaks = pretty(0: nrow(dproba), n = 10)) +
    labs(title = "Función de masa") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  return(grafica)
}

#Grafica la comparacion de dos distribuciones
grafica_comparacion <- function(dproba1, dproba2) {
  dproba12 <- rbind(dproba1, dproba2)
  grafica <- ggplot(dproba12, aes(x=x, y=fx, xend = x, yend = 0,
                                  group=group, col=group, fill=group, linetype = group)) +
    scale_linetype_manual(values=c(2,1)) +
    scale_colour_manual(values=c("firebrick1", "darkturquoise")) +
    geom_segment(size=1) +
    geom_point(size = .5, shape = 1) +
    scale_x_discrete(breaks = pretty(0: nrow(dproba1), n = 10)) +
    labs(title = "Comparación de distribuciones") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  return(grafica)
}

#Simula una VA binomial negativa BN(p,r)
#N es el numero de repeticiones
simular_experimento <- function(p,r,N){
  print(paste("Simulación de BN(p,r) con parámetros p =", p,", r =", r,", N =",N ))
  simulacion <- replicate(N, experimento(p,r))
  print(paste("La media es:", mean(simulacion)))
  print(paste("La desviación estándar es:", sd(simulacion)))
  t_contingencias <- table(factor(simulacion, levels = 0:max(simulacion)))
  t_prop <- prop.table(t_contingencias)
  d_simulacion <- as.data.frame(t_prop)
  x <- 0:max(simulacion)
  d_geom <- data.frame(x, dnbinom(x-r,size= r, prob = p)) #reparametrizacion
  names(d_simulacion) <- c("x","fx")
  names(d_geom) <- c("x","fx")
  d_simulacion$group <- "Simulacion(p,r,N)"
  d_geom$group <- "BN(p,r)"
  gdensidad <- grafica_densidad(d_simulacion) +
    labs(subtitle = paste("p =", p,", r =", r,", N =",N ))
  gcomparacion <- grafica_comparacion(d_simulacion, d_geom) +
    labs(subtitle = paste("p =", p,", r =", r,", N =",N ))
  list(gdensidad,gcomparacion)
}

#Demostracion de la funcion simula_lanzamientos
p <- .2
r <- 2
N <- 30
simula_lanzamientos(p,r,N)

#Se establecen los parametros de cada simulacion
p <- .2
r <- 2
N <- 10^6
simular_experimento(p,r,N)

p <- .1
r <- 2
N <- 10^6
simular_experimento(p,r,N)

p <- .2
r <- 7
N <- 10^6
simular_experimento(p,r,N)

p <- .1
r <- 7
N <- 10^6
simular_experimento(p,r,N)
