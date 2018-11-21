library("ggplot2")
#Parte (a) 
#Escriba un programa en R que reproduzca las gráficas de las funciones de distribución
#acumulada y de masa de la distribución uniforme que aparecen en las notas del curso.

#Gráfica de la función de masa de x
grafica_masa_uniforme <- function(n) {
  x <- c(1:n)
  proba <- 1/length(x)*rep(c(1), times = length(x)) #Se calcula el vector de probabilidades
  masa_np <- data.frame(x, proba)
  names(masa_np) <- c("x","fx")
  p <- ggplot(masa_np, aes(x = x, y = fx)) +
    geom_segment(aes(xend = x, yend = 0), size = .2) +
    geom_point(size = .05) +
    labs(title = "Función de masa") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = 1:n)
  return(p)
}

#Gráfica de función de probabilidad acumulada de x
grafica_proba_uniforme <- function(n){
  x <- c(1:n)
  proba <- 1/length(x)*rep(c(1), times = length(x)) #Se calcula el vector de probabilidades
  cdf_x <- sapply(x, function(z) sum(proba[1:z]))
  cdf_u <- data.frame(x, cdf_x)
  names(cdf_u) <- c("x", "Fx")
  xend <- c(seq(2,n),NA)
  yend <- cdf_u$Fx
  p <- ggplot(data = cdf_u, aes(x=x, y = Fx, xend = xend, yend= yend)) +
    geom_segment() +
    geom_point() +  
    geom_point(aes(x=xend, y=Fx), shape=1) +
    labs(title = "Función de probabilidad acumulada") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = 1:n) +
    scale_y_continuous(breaks = seq(0,1,.2))  
  return(p)
}


#Graficas de masa y probabilidad acumulada para n = 5
grafica_masa_uniforme(5)
grafica_proba_uniforme(5)

#Graficas de masa y probabilidad acumulada para n = 10
grafica_masa_uniforme(10)
grafica_proba_uniforme(10)

#Graficas de masa y probabilidad acumulada para n = 50
grafica_masa_uniforme(50)
grafica_proba_uniforme(50)

#Parte (c)
#Usando la función sample simule una muestra de tamaño 10 000 de la distribución
#U (1, . . . , 10). Fijando la semilla en 13 (set.seed(13))
set.seed(13)
n <- 10
x <- c(1:n)
proba <- 1/length(x)*rep(c(1), times = length(x)) #Se calcula el vector de probabilidades
muestra <- sample(x, size= 10000, replace = TRUE, proba)
datos = as.data.frame(table(muestra))
names(datos) <- c("x", "Frecuencia")
#Se muestran los resultados de la simulación
datos
#Calcule la media
mean(datos[,2]) #calculaste los estadisticos de las frecuencias no de la muestra
#con tus objetos quedaba asi 
mean(muestra); var(muestra)
#Calcule la varianza
var(datos[,2])

#Parte (d)
#Grafique las frecuencias de la simulación anterior.
p <- ggplot(datos, aes(x = x, y = Frecuencia)) +
  geom_bar(stat = "identity", width=.7, fill="lightblue") +
  ylab("Frecuencia de x") +
  labs(title = "Distribución de frecuencias") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,1000,200))
p
# bonitas graficas, pero te doy 3/4 del ejercicio