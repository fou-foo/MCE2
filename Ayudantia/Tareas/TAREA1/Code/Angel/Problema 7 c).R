# n y p son los parametros de una binomial B(n,p)
n<-123
p<-0.31
# Se calculan los cuantiles mediante un while y el uso de la funcion
# pbinom(x,n,p) , inicializando cada vez en cero y aumentando hasta que 
# la probabilidad acumulada sea la deseada
cuantil1<-0
while(pbinom(cuantil1,n,p)<=0.25) {
  cuantil1<-cuantil1+1
}
cuantil2<-0
while(pbinom(cuantil2,n,p)<=0.50) {
  cuantil2<-cuantil2+1
}
cuantil3<-0
while(pbinom(cuantil3,n,p)<=0.75) {
  cuantil3<-cuantil3+1
}
print("Resultados utilizando un while:", quote=FALSE)
print( paste("El primer cuantil es : ", cuantil1), quote=FALSE)
print( paste("El segundo cuantil es : ", cuantil2), quote=FALSE)
print( paste("El tercer cuantil es : ", cuantil3), quote=FALSE)

# Usando la funcion qbinom podemos obtener los cuantiles
# solo es necesario pasar como parametro las probabilidades deseadas para
# para marcar los cuantiles
ProbCuantiles<-c(0.25,0.5,0.75)
Cuantiles<-qbinom(ProbCuantiles,n,p)
print("Los cuantiles con la funcion qbinom son : ", quote=FALSE)
print(Cuantiles)
#bien