#Tarea 1, Ejercicio 5
#Felipe Neri Chairez Cuellar
#
# a)
# Grafica PMF Uniforme Discreta n=5
n<-5
x<-seq(1,n)
y<-rep(1/n,n)
plot(x,y,xlab='Espacio Muestral',ylab = 'Probabilidad',main = 'PMF Uniforme Discreta n=5',ylim = c(0,1))
###############################
#Grafica CDF Uniforme Discreta n=5
n<-5
x<-seq(1,n)
y<-cumsum(rep(1/n,n))
plot(x,y,xlab='Espacio Muestral',ylab = 'Probabilidad acumulada',main = 'CDF Uniforme Discreta n=5',ylim = c(0,1),type = 's')
########################################
# Grafica PMF Uniforme Discreta n=10
n<-10
x<-seq(1,n)
y<-rep(1/n,n)
plot(x,y,xlab='Espacio Muestral',ylab = 'Probabilidad',main = 'PMF Uniforme Discreta n=10',ylim = c(0,1))
###############################
#Grafica CDF Uniforme Discreta n=10
n<-10
x<-seq(1,n)
y<-cumsum(rep(1/n,n))
plot(x,y,xlab='Espacio Muestral',ylab = 'Probabilidad acumulada',main = 'CDF Uniforme Discreta n=10',ylim = c(0,1),type = 's')
########################################
# Grafica PMF Uniforme Discreta n=50
n<-50
x<-seq(1,n)
y<-rep(1/n,n)
plot(x,y,xlab='Espacio Muestral',ylab = 'Probabilidad',main = 'PMF Uniforme Discreta n=50',ylim = c(0,1))
###############################
#Grafica CDF Uniforme Discreta n=50
n<-50
x<-seq(1,n)
y<-cumsum(rep(1/n,n))
plot(x,y,xlab='Espacio Muestral',ylab = 'Probabilidad acumulada',main = 'CDF Uniforme Discreta n=50',ylim = c(0,1),type = 's')
########################################
########################################
# c)
#fija la semilla
set.seed(13) 
w=seq(1:10)
sample(w,size=10000,replace=TRUE,prob=NULL)
#Tabla de frecuencias
tabla_frecuencia<-table(sample(w,size=10000,replace=TRUE,prob=NULL))
tabla_frecuencia
#Promedio
mean(sample(w,size=10000,replace=TRUE,prob=NULL))
#Varianza
var(sample(w,size=10000,replace=TRUE,prob=NULL))
#
# d)
#
plot(tabla_frecuencia,ylab ='Frecuencia',main='Grafica de Frecuencias',xlab='Espacio muestral')
# bien 
