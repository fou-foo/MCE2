# Inferencia Estadistica
# Tarea02_Ejercicio04
# Por Felipe Neri Chairez Cuellar
#

funcion_moneda<-function(N,p) {
  vector<-rep(0,times=N)
  moneda<-c("Aguila","Sol")
  for(i in 1:N) {  
    k<-0
    resultado_moneda <- sample(moneda, size = 1, replace = TRUE, prob = c(p,1-p))
       while(resultado_moneda!="Aguila"){  #Si el primer resultado es distinto de "aguila", seguira el proceso hasta que obtengamos un exito.
       resultado_moneda <- sample(moneda, size = 1, replace = TRUE, prob = c(p,1-p))
       k=k+1  #Cuenta el numero de lanzamientos hasta obtener el "aguila"
       }
    vector[i] <-k  #contador del numero de lanzamientos
  }
  return(vector)
}

# Fijar paramteros de la funcion para ejecutarla
x<-funcion_moneda(10000,0.4)
x


# b) 
# Evaluar la funcion con los parametros p= 0.5, 0.1. 0.01
# Caso particular para: p=0.5,N=10^4
a<-funcion_moneda(10000,0.5)  
x<-seq(min(a):max(a)) #Calcula el intervalo 
Frec<-prop.table(table(a)) #Frecuencias normalizadas
plot(Frec, col="blue")
dg<-dgeom(x,0.5)
lines(dg,col="red",type = "b",lwd=1)  #Empalma la simulacion con la funcion de masa.


#c)
# Evaluar la funcion con los parametros p= 0.5, 0.1. 0.01
# Caso particular para: p=0.5,N=10^6
b<-funcion_moneda(1000000,0.5) 
x<-seq(min(b):max(b)) #Calcula el intervalo 
Frec<-prop.table(table(b)) #Frecuencias normalizadas
plot(Frec, col="blue")
dg<-dgeom(x,0.5)
lines(dg,col="red",type = "b",lwd=1)  
#Media
mean(b)
#Varianza
sd(b)
repitelo por favor 
