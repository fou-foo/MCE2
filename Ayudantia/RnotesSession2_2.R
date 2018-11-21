?read.csv
##########
datos <- iris 
table(datos$Species)# agrupa
levels(datos$Species) <- c('a', 'b', 'c')
set.seed(0)
datos <- data.frame(x = sample(1:10, 100, replace =TRUE),
                    y = sample(1:5, 100, replace =TRUE),
                    z = sample(1:2, 100, replace = TRUE))
table(datos$x, datos$y)
tabla <- table(datos$x, datos$y, datos$z)#mas aun multidimensional
  tabla
class(tabla)
str(tabla)
plot(tabla)
plot(table(datos$x))
attributes(tabla)
d.f <- as.data.frame(tabla)
d.f
order(d.f$Var1)
d.f <- d.f[order(d.f$Var1),] #order
d.f
####################
# histograma
#########
#simulemos algo con colas pesadas
set.seed(0)
n <- 1000
z <- (rnorm(n, 0, 15)**2 + rnorm(n, 10**2,100000)**2)**(.5) 
hist(z)
hist(z, breaks = 100)
hist(z, breaks = 1000)
hist(z[z < 250000])
range(z)
hist(z , breaks = seq(0, 305000, by=1000))
hist(z, breaks=50, col=colors()[1:50])
?hist
####################
# qqplots: ejercicio
#######
z <- rnorm(n, 0, 20)
w <- rnorm(n, 0,20)**2
hist(z, col=colors())
qqplot(z, w)
qqline(z, col='navy')
qqnorm(z)
qqline(z, col ='navy')
qqnorm(w)
qqline(w, col ='navy')
################################
# cummean ftp://cran.r-project.org/pub/R/web/checks/check_results_arturo.erdely_at_comunidad.unam.mx.html
######
sum(1:100)
cumsum(1:100)
cumprod(1:100)
cummax()
cummin()
library(stats)
plot(cumstats::cummean(z), type='l')
################## 
# APPLY's  y dplyr SQL
##########
library(dplyr)
iris %>%  group_by(Species) %>% summarise(mediaSepal.lenght = mean(Sepal.Length)) -> salida2
salida2
iris %>% select(Sepal.Length, Species) 
iris %>% select(Sepal.Length, Species) %>% mutate(x2 = Sepal.Length**2)
iris %>% select(Sepal.Length, Species) %>% mutate(x2 = Sepal.Length**2) -> salida
salida
tapply(iris$Sepal.Length, iris$Species, mean  )
##########################
str(iris)
sapply(iris,class)
apply(iris[, 1:4], 2, mean)
############################ Ejemplo en linux
set.seed(0)
x <- rnorm(100)
fivenum(x)
quantile(x)
