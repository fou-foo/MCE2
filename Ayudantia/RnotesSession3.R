###############################
# Scatterplot
###############################
datos <- iris
dim(iris)
plot(datos$Sepal.Length)
plot(datos$Sepal.Length, datos$Sepal.Width)
range(datos$Sepal.Length)
plot(datos$Sepal.Length, datos$Sepal.Width, xlim=c(4,6))
abline(v=5, col='red3', lwd=2) #vertical
abline(h=3, col='navy', lwd=3) #horizontal
abline(a=0, b=1/2, col='orange', lty='dashed', lwd=3)
######### Diferentes tamaños no jala el reciclyng
plot(datos$Sepal.Length, datos$Sepal.Width[1:149])
plot(datos$Sepal.Length, datos$Sepal.Width, col=colors()[1:150], cex=2, pch=20)
plot(datos$Sepal.Length, datos$Sepal.Width, col=colors()[1:50], cex=2, pch=1:150)
?pch  #mas facil de buscar en la doc
plot(datos$Sepal.Length, datos$Sepal.Width, col=colors()[1:50], cex=2, pch=1:25)
lines(seq(4.5, 8, by=.5), runif(8, 2, 4), col='red', lwd=6)
################################################
pairs(datos)
set.seed(0)
x <- rnorm(100)
x <- c(x, NA)
mean(x)
mean(x[!is.na(x)])
mean(x, na.rm = TRUE)
plot(c(1,2), c(0,NA))
######################################
# otro ejemplo de polimorfismo
######################################
cor(datos$Sepal.Length, datos$Sepal.Width)
cor(datos[, 1:4])
?cor
#####################################
# Capitulo 5
#####################################
?dev.off
#####################################
#####################################
## seleccion de los 100 ejercicios
#####################################
#####################################
# ejercicio 4
tip <- function()
{
  print('Total de la cuenta? ')
  total <- as.numeric(scan()) # con el libro de Advanced R veremos el trycatch
  propina <- round(total * 0.15) + 1 #hay que ser gen. cuando se pueda
  print(paste0('La propina es de: ', propina))
}
tip()
# porque funciona vectorizado ?
######################################
# ejercicio 7
factorial.vec <- function()
{
  print('Factorial de un entero ?')
  numero <- as.numeric(scan())
  entero <- round(numero)
  return(prod(1:entero))
}
factorial.vec() # ?esta vectorizada, como la vectorizarias?
# como probarias el limite superior de la entrada ?
# y de manera eficiente ?
#
#
#
#
#
factorial <- function(x)
{
  numero <- as.numeric(x)
  entero <- round(numero)
  return(prod(1:entero))
}
indice <- 1
while (factorial(indice))
{
  indice <- indice*1000

}
# diferencia con R version 3.5
#######################################
# ejercicio 20
class(Titanic)
(titanic.df <- Titanic)
print(titanic.df <- Titanic)
titanic.df <- as.data.frame(titanic.df)
str(titanic.df)
estadisticos <- function()
{
  print('Qué clase te interesa?') #comentario sobre los acentos en codigo
  clase <- readline()
  titanic.df <- Titanic
  titanic.df <- as.data.frame(titanic.df)
  subconjunto <- subset(titanic.df, as.character(Class)== clase )
  salida <- table(subconjunto$Survived)
  salida.df <- as.data.frame(salida)
  return(salida.df)
}
estadisticos()
# table(titanic.df$Class, titanic.df$Survived)
#########################################
# ejercicio 24
#########################################
WorldPhones <- as.data.frame(WorldPhones)
x <- WorldPhones$N.Amer
d <- apply(WorldPhones, 2, function(x)
  {
     diff(x)/x[1:(length(x)-1)]
})
d <- as.data.frame(d)
d$N.Amer
n <- dim(WorldPhones)[1]
test <- (WorldPhones$N.Amer[2:n] - WorldPhones$N.Amer[1:(n-1)])/ WorldPhones$N.Amer[1:(n-1)]
d$N.Amer -test #verificamos
#########################################
# ejercicio 32
#########################################
decil.70 <- quantile(iris$Petal.Length, probs = 0.7)
subconjunto <- subset(iris, Petal.Length >= decil.70)
table(subconjunto$Species)
########################################
# ejercicio 35
########################################
medianas.por.specie <- tapply(iris$Petal.Length, iris$Species, median)
class(medianas.por.specie)
medianas.por.specie <- as.data.frame(medianas.por.specie)
########################################
# ejercicio 50
########################################
datos <- WorldPhones
datos <- as.data.frame(datos)
datos.porcentajes <- apply(datos, 1, function(x)
  {
    x/sum(x)
})
datos.porcentajes <- t(datos.porcentajes) # checar tipo de dato que regresa apply
image(t(datos.porcentajes)[1:3,])
dim(datos.porcentajes)
apply(datos.porcentajes, 1, sum)
datos.porcentajes
