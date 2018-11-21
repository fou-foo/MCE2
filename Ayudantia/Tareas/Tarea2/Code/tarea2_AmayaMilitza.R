#Elaborado por: Militza Amaya Moguel
#Inferencia Estadistica
# Tarea 2
# fecha: 11 septiembre 2018

# Ejercicio 4

# a)

# Función que simula el lanzamiento de una muneda-monedad- hasta obtener un aguila
funcion4<-function(p,N){
  vector4<-rep(0,N)
  moneda<-c(1,0) # donde 1 representa el resultado de caer águila
  
  for(i in 1:N){
    contador<-1
    
    while(sample(moneda,1,replace = T, prob = c(p,1-p))!=1){
      contador<-contador+1
    }
    vector4[i]=contador #recuerda utilizar el operador '<-'
  }
  return(vector4)
}

# b)

# Simulacion de una variable aleatoria geometrica con p=0.1, 0.5 y 0.01
#Grafica de frecuancias normalizadas
#Graficas de funcion de masa

n<-10^4

p05<-funcion4(0.5,n)     #para p=0.5
p01<-funcion4(0.1,n)     #para p=0.1
p001<-funcion4(0.01,n)   #para p=0.01

## grafica para p=0.5
v1<-(sort(unique(p05)))
plot(prop.table(table(p05)),col="blue",xlab = "",ylab="")
par(new=T)
plot(v1,dgeom(v1,0.5),col="red",xlab = "águilas",ylab="proporción")

## grafica para p=0.1
v2<-(sort(unique(p01)))
plot(prop.table(table(p01)),col="blue",xlab = "",ylab="")
par(new=T)
plot(v2,dgeom(v2,0.1),col="red",xlab = "águilas",ylab="proporción")

## grafica para p=0.01
v3<-(sort(unique(p001)))
plot(prop.table(table(p001)),col="blue",xlab = "",ylab="")
par(new=T) #procura siempre usar TRUE en lugar de T recuerda lo que hablamos sobre el dispatch de R
plot(v3,dgeom(v3,0.01),col="red",xlab = "águilas",ylab="proporción")
#prueba con la funcion 'points'
plot(prop.table(table(p001)),col="blue",xlab = "",ylab="")
points(v3,dgeom(v3,0.01),col="red",xlab = "águilas",ylab="proporción", type='l')

#¿Que se observa?
#Respuesta: Se observa que la grafica que se obtiene con la simulación
#se comportan de manera similar siempre que N sea grande.

# c)
#Simulacion para N=10^6
#ACOSTUMBRATE A FIJAR TU SEMILLA EN CODIGOS NO DETERMINISTAS, como las simulaciones
set.seed(0)
n2<-10^6

# Es muy lento ...
p205<-funcion4(0.5,n2)      #para p=0.5
p201<-funcion4(0.1,n2)      #para p=0.1
t1 <- Sys.time()
p2001<-funcion4(0.01,n2)    #para p=0.01
t1 <- Sys.time() - t1
t1
## grafica para p=0.5
v4<-(sort(unique(p205)))
plot(prop.table(table(p205)),col="blue",xlab = "",ylab="")
par(new=T)
plot(v4,dgeom(v4,0.5),col="red",xlab = "águilas",ylab="proporción")

## grafica para p=0.1
v5<-(sort(unique(p201)))
plot(prop.table(table(p201)),col="blue",xlab = "",ylab="")
par(new=T)
plot(v5,dgeom(v5,0.1),col="red",xlab = "águilas",ylab="proporción")

## grafica para p=0.01
v6<-(sort(unique(p2001)))
plot(prop.table(table(p2001)),col="blue",xlab = "",ylab="")
par(new=T)
plot(v5,dgeom(v5,0.01),col="red",xlab = "águilas",ylab="proporción")# aqui se te fue una variable
    #supongo que es esta linea
plot(v6,dgeom(v6,0.01),col="red",xlab = "águilas",ylab="proporción")# aqui se te fue una variable

#Media y desviación estandar de la simulacion

mean(p205)            #El promedio de p=0.5
sqrt(var(p205))       #Desviación estandar de p=0.5
#PRUEBA LA FUNCION
sd(p205)
mean(p201)            #El promedio de p=0.1
sqrt(var(p201))       #Desviacion estandar de p=0.1
mean(p2001)           #El promedio de p=0.01
sqrt(var(p2001))      #Desviación estandar p=0.01

#¿Que observa?
#Respuesta: similar al comentario de del ejercicio anterios.
#Las graficas de la distribucion teorica y la obtenidad de la simulacion
#son similares, pero esto se puede apreciar unicamente cuando N tiene
#valores grandes

# Ejercicio 5

#Funcion que simula lanzamiento de una moneda hasta obtener r aguilas
funcion5<-function(N,r,p){
  vector5<-rep(0,N)
  moneda<-c(1,0) # donde 1 representa el resultado de caer águila
  
  for(i in 1:N){
    contador<-0
    aguila<-0
    
    while(aguila<r){
      tiro <- sample(moneda,1,replace = T, prob = c(p,1-p))
      contador<-contador+1
      
      if(tiro==1){
        aguila=aguila+1
      }
    }
    vector5[i]=contador
  }
  return(vector5)
}

#Graficas de frecuancias normalizadas y funcion de masa

set.seed(0)
n3<-10^6

p1202<-funcion5(n3,2,0.2)
p1201<-funcion5(n3,2,0.1)
p1702<-funcion5(n3,7,0.2)
p1701<-funcion5(n3,7,0.1)

## grafica para p=0.2 y r=2
v7<-(sort(unique(p1202)))
plot(prop.table(table(p1202)),col="blue",xlab = "",ylab="")
par(new=T)
plot(v7,dnbinom(v7,2,0.2),col="red",xlab = "águilas",ylab="proporción")
# te volvio a fallar una variable en la ultima linea

## grafica para p=0.1 y r=2
v8<-(sort(unique(p1201)))
plot(prop.table(table(p1201)),col="blue",xlab = "",ylab="")
par(new=T)
plot(v8,dnbinom(n3-2,2,0.1),col="red",xlab = "águilas",ylab="proporción")
### SUPONGO QUE ES 
plot(v8,dnbinom(v8-2,2,0.1),col="red",xlab = "águilas",ylab="proporción")

## grafica para p=0.2 y r=7
v9<-(sort(unique(p1702)))
plot(prop.table(table(p1702)),col="blue",xlab = "",ylab="")
par(new=T)
plot(v9,dnbinom(n3-7,7,0.2),col="red",xlab = "águilas",ylab="proporción")
plot(v9,dnbinom(v9-7,7,0.2),col="red",xlab = "águilas",ylab="proporción")

## grafica para p=0.1 y r=7
v10<-(sort(unique(p1701)))
plot(prop.table(table(p1701)),col="blue",xlab = "",ylab="")
par(new=T)
plot(v10,dnbinom(n3-7,7,0.1),col="red",xlab = "águilas",ylab="proporción")
lo mismo ...
plot(v9,dnbinom(v9-11,7,0.2),col="red",xlab = "águilas",ylab="proporción")

###### Y el 9 ??

Como tu codigo no corrio hasta que lo altere (los parametros de las graficas) te doy .85 del segundo ejercicio y el primero correcto
Pero es muy lenta la simulación veremos en la ayudantia como lidiar con eso