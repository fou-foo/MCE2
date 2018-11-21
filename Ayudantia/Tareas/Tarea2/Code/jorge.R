
#Tarea 2   Inferencia Estad??stica
#Alumno: JORGE SANCHEZ GARCIA

#4 a)Crear una funcion que simule N veces lanzamientos de una moneda desequilibrada
#hasta obtener ??guila. La funcion recibe como parametros a la probabilidad y al numero N
#que se repite el experimento y regresa un vector de longitud N que contiene el numero 
#de lanzamientos hasta obtener un aguila en cada uno de los N lanzamientos.

u <- vector()  #Declaramos el vector que necesitaremos para guardar los Ns.
progeo<-function(p,N) {  #Funcion que simula los N lanzamientos.
  u <- vector()
  i=1
  for(i in 1:N) {  
    j<-1
    a <- sample(x=c("A","S"), size = 1, replace = T, prob = c(p,1-p)) #Genera aleatoriamente el "aguila" o "sol".
    if(a!="A"){  #Validamos si el primer resultado fue aguila.
    while(a!="A"){  #Si el primer resultado es distinto de "aguila", seguira el proceso hasta que obtengamos un exito.
      a <- sample(x=c("A","S"), size = 1, replace = T, prob = c(p,1-p))
      j=j+1  #Cuenta el numero de lanzamientos hasta obtener el "aguila"
    }
    }
    u[i] <- j  #Guarda el numero de lanzamientos en la memoria correspondiente al numero de lanzamiento.
  }
  return(u)
  
}
#Como ejemplo, evaluamos la funcion con p=0.3 y N=100.
b<-progeo(0.3,100)
b

#4b) Usando la funci??n anterior simule N = 10^4 veces una variable aleatoria Geom(p) 
#para p = 0.5, 0.1, 0.01. Grafique las frecuencias normalizadas en color azul. 
#Sobre ??sta ??ltima figura empalme en rojo la gr??afica de la funci??n de masa correspondiente.

#p=0.5,N=10^4
b<-progeo(0.5,10000)  #Genera nuestras xs con los par??metros indicados.
x<-seq(min(b):max(b)) #Calcula el intervalo de x que se usar?? en la distribuci??n geom??trica.
Frec<-prop.table(table(b)) #Tabula las frecuencias normalizadas
Frec   #Frecuencias normalizadas
plot(Frec, main = "SIMULACION DISTRIBUCION GEOMETRICA",ylab ="Proporcion",xlab="Espacio muestral",col="blue")
disgeo<-dgeom(x,0.5)
lines(disgeo,col="red",type = "b",lwd=1)  #Empalma la simulacion con la funcion de masa.

#p=0.1,N=10^4
b<-progeo(0.1,10000)  #Genera nuestras xs con los par??metros indicados.
x<-seq(min(b):max(b)) #Calcula el intervalo de x que se usar?? en la distribuci??n geom??trica.
Frec<-prop.table(table(b)) #Tabula las frecuencias normalizadas
Frec   #Frecuencias normalizadas
plot(Frec,main = "SIMULACION DISTRIBUCION GEOMETRICA",ylab ="Proporcion",xlab="Espacio muestral",col="blue")
disgeo<-dgeom(x,0.1)
lines(disgeo,col="red",type = "b",lwd=1)  #Empalma la simulacion con la funcion de masa.

#p=0.01,N=10^4
b<-progeo(0.01,10000)  #Genera nuestras xs con los par??metros indicados.
x<-seq(min(b):max(b)) #Calcula el intervalo de x que se usar?? en la distribuci??n geom??trica.
Frec<-prop.table(table(b)) #Tabula las frecuencias normalizadas
Frec   #Frecuencias normalizadas
plot(Frec, main = "SIMULACION DISTRIBUCION GEOMETRICA",ylab ="Proporcion",xlab="Espacio muestral",col="blue")
disgeo<-dgeom(x,0.01)
lines(disgeo,col="red",type = "b",lwd=1)  #Empalma la simulacion con la funcion de masa.

#??Qu?? observa? R: que la Distribucion Geometrica es una muy buena aproximacion a nuestro experimento.

#4c) Repita el inciso anterior para N = 10^6. 
#Adem??s calcule el promedio y la desviaci??n est??ndar de las simulaciones que realiz??.

N=1000000
#p=0.5,N=10^4
b<-progeo(0.5,N)  #Genera nuestras xs con los par??metros indicados.
x<-seq(min(b):max(b)) #Calcula el intervalo de x que se usar?? en la distribuci??n geom??trica.
Frec<-prop.table(table(b)) #Tabula las frecuencias normalizadas
Frec   #Frecuencias normalizadas
plot(Frec,main = "SIMULACION DISTRIBUCION GEOMETRICA",ylab ="Proporcion",xlab="Espacio muestral",col="blue")
disgeo<-dgeom(x,0.5)
lines(disgeo,col="red",type = "b",lwd=1)  #Empalma la simulacion con la funcion de masa.
#La media es:
mean(b)
#La varianza:
sd(b)

#p=0.1,N=10^6
b<-progeo(0.1,N)  #Genera nuestras xs con los par??metros indicados.
x<-seq(min(b):max(b)) #Calcula el intervalo de x que se usar?? en la distribuci??n geom??trica.
Frec<-prop.table(table(b)) #Tabula las frecuencias normalizadas
Frec   #Frecuencias normalizadas
plot(Frec, main = "SIMULACION DISTRIBUCION GEOMETRICA",ylab ="Proporcion",xlab="Espacio muestral",col="blue")
disgeo<-dgeom(x,0.1)
lines(disgeo,col="red",type = "b",lwd=1)  #Empalma la simulacion con la funcion de masa.
#La media es:
mean(b)
#La varianza:
sd(b)

#p=0.01,N=10^6
b<-progeo(0.01,N)  #Genera nuestras xs con los par??metros indicados.
x<-seq(min(b):max(b)) #Calcula el intervalo de x que se usar?? en la distribuci??n geom??trica.
Frec<-prop.table(table(b)) #Tabula las frecuencias normalizadas
Frec   #Frecuencias normalizadas
plot(Frec,main = "SIMULACION DISTRIBUCION GEOMETRICA",ylab ="Proporcion",xlab="Espacio muestral",col="blue")
disgeo<-dgeom(x,0.01)
lines(disgeo,col="red",type = "b",lwd=1)  #Empalma la simulacion con la funcion de masa.
#La media es:
mean(b)
#La varianza:
sd(b)

#5 Usando las ideas del inciso anterior escriba una funci??on en  que simule N veces 
#los lanzamientos de moneda hasta obtener r ??guilas. La funci??n deber?? recibir como
#par??metros a la probabilidad p de obtener ??guila, al n??mero r de ??gguilas a observar 
#antes de detener el experimento y al n??mero N de veces que se repite el experimento;
#y tendr?? que regresar un vector de longitud N que contenga el n??mero de lanzamientos 
#hasta obtener las r  ??guilas en cada uno de los N experimentos. 
#Grafique las frecuencias normalizadas de los experimentos para N = 10^6, p = 0.2, 0.1 y r = 2, 7 
#y comp??relos contra la funci??n de masa de la distribuci??n m??s adecuada para modelar este tipo 
#de experimentos.

launch<- function (N,r,p){ #Funcion que simula los lanzamientos
  v<-rep(0,times=N)     #Genera un vector 0 inicializado de longitud N
  moneda<-c("A","S")  #Resultados de la moneda A:??guila, S:Sol
  for (i in 1:N){
    k<-0      #Se reinicia el contador antes de entrar al ciclo while
    while (k<r){
      #resultado_moneda simula el resultado de un lanzamiento con probabilidad p de exito
      resultado_moneda <- sample(moneda,size=1,replace=TRUE,prob=c(p,1-p))
      v[i]<-v[i]+1   #Contador del numero de lanzamientos por experimento
      if(resultado_moneda=="A"){
        k<-k+1    #Contador del numero de exitos (aguilas)
      }
    }
  }
  return (v)  
}

#  Grafica de las frecuencias de los experimentos.
# -Parametros requeridos: 
#  N = 10^6  ;  r = 2, 7  ;  p = 0.1, 0.2

#r=2, p= 0.1
a<-launch(1000000,2,0.1)
tabla<-prop.table(table(a))
plot(tabla,main = "SIMULACION BINOMIAL NEGATIVA",ylab ="Proporcion",xlab="Espacio muestral",col="blue")
x<-seq(min(a):max(a)) #Calcula el intervalo de x que se usar?? en la distribuci??n binomial negativa.
disbneg<-dnbinom(x-3,size = 2,prob = 0.1)
lines(disbneg,col="red",type = "b",lwd=1)  #Empalma la simulacion con la funcion de masa.

#r=2, p= 0.2
a<-launch(1000000,2,0.2)
tabla<-prop.table(table(a))
plot(tabla,main = "SIMULACION BINOMIAL NEGATIVA",ylab ="Proporcion",xlab="Espacio muestral",col="blue")
x<-seq(min(a):max(a)) #Calcula el intervalo de x que se usar?? en la distribuci??n binomial negativa.
disbneg<-dnbinom(x-3,size = 2,prob = 0.2)
lines(disbneg,col="red",type = "b",lwd=1)  #Empalma la simulacion con la funcion de masa.

#r=7, p= 0.1
a<-launch(1000000,7,0.1)
tabla<-prop.table(table(a))
plot(tabla,main = "SIMULACION BINOMIAL NEGATIVA",ylab ="Proporcion",xlab="Espacio muestral",col="blue")
x<-seq(min(a):max(a)) #Calcula el intervalo de x que se usar?? en la distribuci??n binomial negativa.
disbneg<-dnbinom(x-3,size = 7,prob = 0.1)
lines(disbneg,col="red",type = "b",lwd=1)  #Empalma la simulacion con la funcion de masa.

#r=2, p= 0.2
a<-launch(1000000,7,0.2)
tabla<-prop.table(table(a))
plot(tabla,main = "SIMULACION BINOMIAL NEGATIVA",ylab ="Proporcion",xlab="Espacio muestral",col="blue")
x<-seq(min(a):max(a)) #Calcula el intervalo de x que se usar?? en la distribuci??n binomial negativa.
disbneg<-dnbinom(x-3,size = 7,prob = 0.2)
lines(disbneg,col="red",type = "b",lwd=1)  #Empalma la simulacion con la funcion de masa.

#Se concluye que la Distribucion Binomial Negativa es una muy buena aproximacion de nuestro experimento.

