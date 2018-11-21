#orden de operaciones 
1 / (2 + 17) - 1.5 + sqrt(2)
1 / 19 - 1.5 + sqrt(2)
(1 / 19) - 1.5 + sqrt(2)
( (1 / 19) - 1.5) + sqrt(2) # -> derecha izquierda

############################
# el vendito operador <-, <<-
simulacion <- rep(0, 10**6) 
for( i in 1:10**6)
{
    simulacion[i] <<- sample.int()
    return(NA)
     
}
a <- suma.x(17)
suma.x(17)
###########################
# dispatch y semillas
?runif
set.seed(0) # no abordar mucho este tema , mejor ver la documentacion de Knuth
r.a <- runif(n = 10, min = 10, max = 15)
r.a
set.seed(0)
r.a <- runif(min=10, max=15, n=10)
r.a
set.seed(0)
r.a <- runif(m=10, ma=15, n=10)
set.seed(0)
r.a <- runif(mi=10, ma=15, n=10)
r.a
set.seed(0)
r.a <- runif(15, 10, 10 )
r.a
##########################
# los enviroments, mas de esto en el Advanced R
ls(envir=baseenv())
?ls
class(baseenv())
str(baseenv())
#############################
# vectores
flotante <- 10.
typeof(flotante)
entero <- 10L
typeof(entero)
object.size(flotante)
object.size(entero)  #  :( chafa todo es double
###########
x <- 1:10
typeof(x)
x2 <- c(x, 'c')
x2
typeof(x2) #diferencia con 6L
lista <- list('c', 1:10)
lista[[1]]
lista[2]
# propiedad de vectores y listas LOS NOMBRES
lista <- list(letra = 'c', vector = x)
lista['vector']
class(lista['vector'])
lista[['vector']]
class(lista[['vector']])
########## los data.frames son listas de vectores
names(iris)
class(iris[, 'Species'])
class(iris$Species) 
names(x) <- paste0('chavelo.', 1:10)
x['chavelo.10']
########## como los diccionarios de python
### reciclyng 
c(1,2,3,4,5,6) + c(100,200)
# porque se rompe con data.frames ?
dim(iris)
iris$Species <- 1:149
##################
class(1:84)
class(c(1:84))
c(1:84) == 1:84
##########
?dim
x <- iris$Species[1:149]
dim(x) <- 149
dim(x)
length(x)
m <- matrix( 1:100, byrow = TRUE )
dim(m)
m
dim(m) <- c(2,50)
m
dim(m) <- c(50,2)
m
dim(m) <- c(25, 2, 2)  #arreglo tridimensional
m
################ bool
bool <- TRUE # vs T
object.size(bool)
TRUE == 1
identical(TRUE, 1)
?identical
typeof(TRUE)
###### vectorizacion 
logicalVec1 <- c(FALSE,FALSE,TRUE,TRUE)
logical <- FALSE
logical & logicalVec1 #recycling
logical <- rep(FALSE, 4)
logical | logicalVec1 
logical || logicalVec1  #unario
logical && logicalVec1  #unario
###################### otra vez el recycling
x <- 1:10
x[1:100]
x[1:5] <- 1
x
############### indices negativos
x <- 1:10
y <- x[-c(1:5)]
y
y <- x[-(1:5)] #casi todo esta vectorizado 
x[-1]
rev(x) # vs el elegante x[::-1]
1:50
50:1
############# indexando con booleanos
vector <- 20:30
logicalIndex <- vector > 25
logicalIndex
newVector <- vector[logicalIndex]
newVector
######
vector <- rep(1:10, each = 2)
vector
which(vector < 5)
which.min(vector < 5)
which.max(vector > 5) #regresa el primero solamente
###
maxi <- max(vector, na.rm = TRUE)
maximos <- which(vector == 5)
maximos
vector[maximos]
#######################
stringVec <- c("pear","apple","pineapple")
paste(stringVec, "juice")
paste0(stringVec, "juice")
?paste0
paste(stringVec, "juice", sep='') #paste0 es un alias
##############coercion 
string <- '123.2'
as.numeric(string) + 1
is.numeric(string)
####################
datos <- iris 
colnames(iris)
row.names(datos) <- paste0('panchito', 1:dim(iris)[1])
datos['panchito10', 'Species']
datos$Species[10]
######################
colnames(datos)
datos[, 'nueva'] <- 1
datos[, 7] <- 2
View(datos)
######### working directory
getwd()
set ...

##############
vector.string <- rep(c('foo', 'arbol', 'regex'), 2)
'foo' %in% vector.string #polimorfismo
1:2 %in% 1:10
match(x ='foo', table =vector.string)
