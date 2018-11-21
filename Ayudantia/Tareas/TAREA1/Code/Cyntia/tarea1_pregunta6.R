#Alumna: Ramirez Islas Cynthia Mariangel
#Tarea 1 
#Ejercicio 6

###############################################################################
#Inciso a) Usando función sample, Simulacion de moneda equilibrada 10^6 veces  
#Entrada: n=# de simulaciones (10,000)
#Salida: Resultado primeros 3 lanz, gráfica de frecuencias y de probabilidad
#Consideremos "Águila=1"

#Inicialización de variables
aguilas<-0
# de simulaciones
n=10000000
#Simulación de 10 lanzamientos de moneda repitiendo proceso 10^6 veces 
for(i in 1:n){
  moneda<- sample(c(0,1), 10, replace = TRUE)
  aguilas[i]<-c(sum(moneda==1)) 

#Impresión de los primeros 3 resultados 
  if(i<=3) {
    print(paste("# de águilas en el lanzamiento",i)  )
    print(aguilas[i])
  }
}
# es muy tardado te recomiendo declarar el vector 'aguilas' fuera del for y rellenarlo

#Cálculo de frecuencias
frecuencia<- data.frame(table(aguilas))
#Cálculo de probabilidades
probabilidad<- prop.table(table(aguilas))

#Gráfica de frecuencias 
plot(frecuencia, main="Gráfica de frecuencias", xlab="# de águilas")
#Gráfica de probabilades, incluyendo gràfica de distribución teórica
plot(probabilidad, main="Función de Probabilidad ", xlab="# de águilas", ylab="Proporciones")

###################################################################################### 
#Inciso b) Usando la función dbinom grafique la función de masa de una distribución B(10; 0:5)
#sobre la gráfica de las proporciones
#Salida: Gráfica de probabilidad teórica vs simulación 

#Gráfica de probabilades, incluyendo gràfica de distribución teórica
plot(probabilidad, main="Función de Probabilidad ", xlab="# de águilas")
lines(dbinom(1:10,10,0.5), col="blue")
legend("topleft",c("Simulación","Dist. teórica"),col=c("black","blue"), lwd=1:2)

#¿Que observa?
#Podemos notar que para  una muestra de tamaño n , n lo suficientemente grande, 
#la simulación de la distribución se asemeja mucho a la distribución binomial 
#B(10; 0:5) teórica 

######################################################################################
#Inciso c) Repetir incisos anteriores para moneda desequilibrada con p = 0:3
#Entrada: n=# de simulaciones (10,000,000) 
#Salida: Resultado primeros 3 lanz, gráfica de frecuencias y de probabilidad
#Consideremos "Águila=1"

##Inicialización de variables
aguilas2<-0
# de simulaciones
m=100000
#Simulación de 10 lanzamientos de moneda desequilibrada,  10^6 veces 
for(i in 1:m){
  moneda2<-sample(c(0:1),10, TRUE, c(.7,.3))
  aguilas2[i]<-c(sum(moneda2==1)) 
  
  #Impresión de los primeros 3 resultados 
  if(i<=3) {
    print(paste("# de águilas en el lanzamiento",i)  )
    print(aguilas2[i])
  }
}

#Cálculo de frecuencias
frecuencia2<- data.frame(table(aguilas2))
#Cálculo de probabilidades
probabilidad2<- prop.table(table(aguilas2))

#Gráfica de frecuencias 
plot(frecuencia2, main="Gráfica de frecuencias")
#Gráfica de proporciones 
plot(probabilidad2, main="Función de Probabilidad ", xlab="# de águilas", ylab="Proporciones")

#Gráfica de probabilades, incluyendo gràfica de distribución teórica
plot(probabilidad2, main="Función de Probabilidad ", xlab="# de águilas", ylab="Probabilidad", ylim=c(0,.3))
lines(dbinom(1:10,10,0.3), col="blue")
legend("topright",c("Simulación","Dist. teórica"),col=c("black","blue"), lwd=1:2)

#¿Que observa?
#Podemos notar que para  una muestra de tamaño n , n lo suficientemente grande, 
#la simulación de la distribución se asemeja mucho a la distribución binomial b(10,.03) teórica 

# bien 



