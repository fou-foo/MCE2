setwd("~/Desktop/MCE2/CD2/Imagenes/TareaFiltros") # cambio de edirectorio de trabajo
######################## package necesarios
library(imager) #libreria para leer imagenes
########################
# funciones
frontera <- function(M.copy, n.mascara)
{
    # Entradas:
    # M.copy (matrix) : Matriz con la informacion de la imagen en escala de grises
    # n.mascara (int): dimension del filtro cuadrado
    # Salidas:
    # M.copy (matrix): Matriz con la informacion de la imagen en escala de grises con la condicion de frontera
    h <- dim(M.copy)[2]
    w <- dim(M.copy)[1]
    for(i in 1:round(n.mascara/2)) # vamos a implementar la frontera de borde de manera sencillita
    {
        for(j in 1:w)
        {
            M.copy[ j ,i ] <- 0
            M.copy[ j,  h-i] <- 0
        }
        for(j in 1:h)
        {
            M.copy[ i , j  ] <- 0
            M.copy[ w-i , j ] <- 0
        }
    }
    return(M.copy)
}
AplicaFiltro <- function(M.copy, mascara)
{
    # Entradas:
    # M.copy (matrix): Imagen en escala de grises
    # mascara (matrix): Filtro a aplicar
    # Salidas:
    # M.copy (matrix): Imagen con el filtro aplicado
    h <- dim(M.copy)[2]
    w <- dim(M.copy)[1]
    n.mascara <- dim(mascara)[1]
    for(i in (round(n.mascara/2) + 1):(w-round(n.mascara/2))) # Aplicamos la mascara
    {
        for(j in (round(n.mascara/2) + 1):(h-round(n.mascara/2)))
        {
            c <- 0
            for(k in -(round(n.mascara/2) -1 ):( round(n.mascara/2)-1))
            {
                for(l in -(round(n.mascara/2) -1 ):( round(n.mascara/2)-1))
                {
                    c <- c + mascara[k+2,l+2]* M.copy[i+k, j+l]
                }
            }
            M.copy[i,j] <- c
        }
    }
    return(M.copy)

}
#########################################
# main
imagen.nombre <- 'los_amantes.jpg'
imagen <- load.image(imagen.nombre) #lectura de imagen
dim(imagen) #por curiosidad vemos sus dimensiones
imagen <- as.cimg(imagen) # cast a clase del package 'imager'
plot(imagen)            #visualizamos la imagen
gray.imagen <- as.cimg( grayscale(imagen)) #cambiamos a escala de grises
plot(gray.imagen) #imagen en grises
gc() # garbaje colector
M <- as.matrix(gray.imagen) # es mas comodo trabajar con matrices
# construccion del filtro
n.mascara <- 3   #numero impar
mascara.simple <- matrix( (1/(1))*c(0, 0, 0,
                             0, 0, 1,
                             0, -1, 0 ), byrow = TRUE, ncol = n.mascara )  #construcción de la mascara
n.mascara <- 5   #numero impar
k <- 80 #por si usamos los filtros de sobel
mascara.cuadratica <- matrix( (1/(k))*c(4, 4, 4, 4, 4,
                             4, 2, 2, 2, 4,
                             4, 2, 0, 2, 4,
                             4, 2, 2, 2, 4,
                             4, 4, 4, 4, 4    ), byrow = TRUE, ncol = n.mascara )  #construcción de la mascara

M.copy <- M
    # aplicacion de filtro de aristas horizontales
plot(as.cimg(M.copy)) # visualizamos la imagen, otra vez
M.copy <- frontera(M.copy, n.mascara=3 )
plot(as.cimg(M.copy)) # visualizamos la imagen con las condiciones de frontera
M.copy <- AplicaFiltro(M.copy, mascara.simple)
plot(as.cimg(M.copy))
save.image(as.cimg(M.copy), file=paste0(imagen.nombre,'filtroSencillo.jpg') )
        # aplicacion de filtro esoterico
M.copy <- M
plot(as.cimg(M.copy)) # visualizamos la imagen, otra vez
M.copy <- frontera(M.copy, n.mascara=5 )
plot(as.cimg(M.copy)) # visualizamos la imagen con las condiciones de frontera
M.copy <- AplicaFiltro(M.copy, mascara.cuadratica)
plot(as.cimg(M.copy)) # esperaba algo mas esoterico :<
save.image(as.cimg(M.copy), file=paste0(imagen.nombre,'filtroesoterico.jpg') )
