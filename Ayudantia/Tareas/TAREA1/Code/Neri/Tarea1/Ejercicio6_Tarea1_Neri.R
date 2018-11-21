#Tarea1_Ejercicio6
#Felipe Neri Chairez Cuellar
# a)
#
moneda<-c(1,2)
sample(moneda,size=10,replace=TRUE,prob=NULL)
resultado_moneda<-factor(moneda)
levels(resultado_moneda)<- c("aguila","sol")
#
#Simulacion de 10 lanzamientos de una moneda
sample(resultado_moneda,size=10,replace=TRUE,prob=NULL)
#Simulación de 1000000 de lanzamientos
sample(resultado_moneda,size=1000000,replace=TRUE,prob=NULL)
tabla_frecuencia<-table(sample(resultado_moneda,size=1000000,replace=TRUE,prob=NULL))
tabla_frecuencia
plot(tabla_frecuencia,ylab ='Frecuencia',main='Grafica de Frecuencias',xlab='Espacio muestral')
#ok, en este caso la linea 13 es correcta porque la prob es constante sinembargo te falto un 0
#pero esta bien, hubiese sido bueno que comentaras ese detalle
#te doy .9 del ejercicio