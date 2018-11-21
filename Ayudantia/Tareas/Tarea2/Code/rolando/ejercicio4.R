rm(list=ls())
library("ggplot2")
library(scales)

#Funcion que simula el lanzamiento de una moneda
lanzamiento <- function(p){
  resultado <- sample(c(0,1), size = 1, prob =c(1-p,p))
  return(resultado)
}

#Realiza lanzamientos hasta obtener un 1, regresa el numero de lanzamientos
experimento <- function(p){
  l_fallidos <-0
  while(lanzamiento(p) != 1) {
    l_fallidos <- l_fallidos + 1
  }
  return (l_fallidos)
}

#Funcion que repite la funcion experimento(p) N veces
#Regresa un vector con el numero de lanzamientos por cada simulacion
simula_lanzamientos <- function(p,N) {
  return(replicate(N, experimento(p)))
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


#Simula una VA geometrica, p es el parametro de G(p)
#N es el numero de repeticiones
simular_experimento <- function(p,N){
  print(paste("Simulación de Geom(p) con parámetros p =", p, ", N =",N ))
  simulacion <- replicate(N, experimento(p))
  print(paste("La media es:", mean(simulacion)))
  print(paste("La desviación estándar es:", sd(simulacion)))
  t_contingencias <- table(factor(simulacion, levels = 0:max(simulacion)))
  t_prop <- prop.table(t_contingencias)
  d_simulacion <- as.data.frame(t_prop)
  x <- 0:max(simulacion)
  d_geom <- data.frame(x, dgeom(x,p))
  names(d_simulacion) <- c("x","fx")
  names(d_geom) <- c("x","fx")
  d_simulacion$group <- "Simulacion(p,N)"
  d_geom$group <- "G(p)"
  gdensidad <- grafica_densidad(d_simulacion) +
    labs(subtitle = paste("p =", p, ", N =",N ))
  gcomparacion <- grafica_comparacion(d_simulacion, d_geom) +
    labs(subtitle = paste("p =", p, ", N =",N ))
  list(gdensidad,gcomparacion)
}

#Demostracion de la funcion simula_lanzamientos
p <- .3
N <- 30
set.seed(0)
simula_lanzamientos(p,N)

#Se establecen los parametros de cada simulacion
p <- .5
N <- 10^4
simular_experimento(p, N)

p <- .1
N <- 10^4
simular_experimento(p, N)

p <- .01
N <- 10^4
simular_experimento(p, N)

p <- .5
N <- 10^6
simular_experimento(p, N)

p <- .1
N <- 10^6
simular_experimento(p, N)

p <- .01
N <- 10^6
simular_experimento(p, N)
ok 