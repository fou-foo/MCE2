#Al final del arhivo se encuentran los parámetros para realizar la simulación
library("ggplot2")

#Obtiene una muestra aleatoria de la población
obtener_muestra <- function(poblacion, tamaño) {
  muestra <- sample(poblacion, tamaño, replace = FALSE)
}

#Simula extracciones tantas veces como 'repeticiones'
simula_extracciones <- function(poblacion, tamaño, repeticiones) {
  experimento <- replicate(repeticiones, obtener_muestra(poblacion, tamaño))
  return(experimento)
}

#Función que calcula la tabla de frecuencias de 'simulacion'
obtener_frecuencias <- function(simulacion) {
  contador <- apply(simulacion,2, function(x) length(x[x == 1]))
  return(as.data.frame(table(contador)))
}

#Función una grafica de frecuencias a partir de una tabla de frecuencias.
grafica_frecuencias <- function(tabla_frecuencias) {
  names(tabla_frecuencias) <- c("x", "Frecuencia")
  grafica <- ggplot(data = tabla_frecuencias, aes(x = x, y = Frecuencia)) +
    geom_bar(stat = "identity", width=.7, fill="lightblue") +
    ylab("Frecuencia de x") +
    labs(title = "Distribución de frecuencias") +
    theme(plot.title = element_text(hjust = 0.5))
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
  dproba2$group <- "H(95,46,20)"
  dproba12 <- rbind(dproba1, dproba2)
  grafica <- ggplot(dproba12, aes(x=x, y=fx, xend = x, yend = 0,group=group, col=group, fill=group, linetype = group)) +
    scale_linetype_manual(values=c(2,1)) +
    geom_segment(size=1) +
    geom_point(size = 3, shape = 4) +
    labs(title = "Comparación de distribuciones") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  return(grafica)
}

# Se establecen los parámetros para la simulación
grises <- 46
blancas <- 49
bolas <- c(rep(1,grises), rep(0,blancas))
tamaño <- 20
repeticiones <- 10^6

# ¿Cuál es la probabilidad de que al extraer 20 bolas de la urna 5 de ellas sean grises?
dhyper(5,grises,blancas, tamaño)

#Se realiza la simulación
simulacion <- simula_extracciones(bolas, tamaño, repeticiones)

#Se obtiene la tabla de frecuencias
tabla_frecuencias <- obtener_frecuencias(simulacion)

#Muestra los primeros tres resultados de la simulación
simulacion[,1:3]

#Se grafica la distribución de frecuencias
grafica_frecuencias(tabla_frecuencias)

#Se grafica la distribución de probabilidad
distribucion_proba <- data.frame(tabla_frecuencias[1],tabla_frecuencias[,2]/sum(tabla_frecuencias[,2]))
grafica_probabilidades(distribucion_proba)


#Distribución hipergeométrica para comparar los resultados obtenidos
x <- seq(0,tamaño,by = 1)
d_hiper <- data.frame(x, dhyper(x,grises,blancas, tamaño))
names(d_hiper) <- c("x", "fx")

#Se grafica la comparación entra las distribuciones de probabilidad
grafica_comparacion_probabilidades(distribucion_proba, d_hiper)