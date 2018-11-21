#Simulación de lanzamientos de monedas.
#Al final del arhivo se encuentran los parámetros para realizar la simulación
library("ggplot2")

#Función que simula el lanzamiento de n monedas, con probabilidad de éxito p
lanzamiento <- function(n,p){
  muestra <- sample(c(0,1), size= n, replace = TRUE, c(1-p,p))
  return(muestra)
}

#Función que simula el lanzamiento de n monedas 
#se repite tantas veces como indica 'repeticiones'
simula_lanzamientos <- function(n,p,repeticiones) {
  experimento <- replicate(repeticiones, lanzamiento(n,p))
  return(experimento)
}

#Función que calcula la tabla de frecuencias de 'simulacion'
obtener_frecuencias <- function(simulacion) {
  contador <- apply(simulacion,2, function(x) length(x[x ==1]))
  return(as.data.frame(table(contador)))
}

#Función que crea una grafica de frecuencias a partir de una tabla de frecuencias.
grafica_frecuencias <- function(tabla_frecuencias) {
  names(tabla_frecuencias) <- c("x", "Frecuencia")
  grafica <- ggplot(data = tabla_frecuencias, aes(x = x, y = Frecuencia)) +
    geom_bar(stat = "identity", width=.7, fill="lightblue") +
    ylab("Frecuencia de x") +
    labs(title = "Distribución de frecuencias") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) #+# por este simbolo tu codigo no jalaba, eso me dice que no lo probaste 
                # lo suficiente antes de enviarmelo 
  return(grafica)
}

#Función que crea una grafica de la función de masa.
grafica_probabilidades <- function(distribucion_proba) {
  names(distribucion_proba) <- c("x","fx")
  grafica <- ggplot(data = distribucion_proba, aes(x = x, y = fx)) +
    geom_segment(aes(xend = x, yend = 0), size = .2) +
    geom_point(size = .05) +
    labs(title = "Función de masa") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  return(grafica)
}

#Función que regresa la comparación de dos distribuciones
grafica_comparacion_probabilidades <- function(dproba1, dproba2) {
  names(dproba1) <- c("x","fx")
  names(dproba2) <- c("x","fx")
  dproba1$group <- "Simulación"
  dproba2$group <- "B(n,p)"
  dproba12 <- rbind(dproba1, dproba2)
  grafica <- ggplot(dproba12, aes(x=x, y=fx, xend = x, yend = 0,
                                  group=group, col=group, fill=group, linetype = group)) +
    scale_linetype_manual(values=c(2,1)) +
    geom_segment(size=1) +
    geom_point(size = 3, shape = 4) +
    labs(title = "Comparación de distribuciones") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  return(grafica)
}

#--------#
# Se establecen los parámetros para la simulación
# Primer caso, p = .5
# Para el caso p = .3, sólo se cambia p <- .5, por p <- .3 y se ejecuta

p <- .5
lanzamientos <- 10
repeticiones <- 10^6
set.seed(13)

#Se realiza la simulación
simulacion <- simula_lanzamientos(lanzamientos, p, repeticiones)  

#Se obtiene la tabla de frecuencias
tabla_frecuencias <- obtener_frecuencias(simulacion) #rapido ! esa es la ventaja de hacer funciones
                      # veo que ya te desenvuelves en el lenguaje si buscas hacer m�s eficiente tu codigo
                      # reviza las funciones lapply, mapply y similares
#Muestra los primeros tres resultados de la simulación
simulacion[,1:3]

#Se grafica la distribución de frecuencias
grafica_frecuencias(tabla_frecuencias)

#Se grafica la distribución de probabilidad
distribucion_proba <- data.frame(tabla_frecuencias[1],tabla_frecuencias[,2]/sum(tabla_frecuencias[,2]))
grafica_probabilidades(distribucion_proba)

#Distribución binomial para comparar los resultados obtenidos
x <- seq(0,lanzamientos,by = 1)
d_binomial <- data.frame(x, dbinom(x,lanzamientos,p))
names(d_binomial) <- c("x", "fx")

#Se grafica la comparación entra las distribuciones de probabilidad
grafica_comparacion_probabilidades(distribucion_proba, d_binomial)


#muy bien  