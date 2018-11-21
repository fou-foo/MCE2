# a) No-ejemplo

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
#################
set.seed(0)
n<-10**5
# Es muy lento ...
t1 <- Sys.time()
p2001<-funcion4(0.01,n)    #para p=0.01
t1 <- Sys.time() - t1
t1
#############################
funcion4.foo <- function(p)
{
    contador <- 1
    while( sample( c(1, 0), 1, replace = TRUE, prob = c(p,1-p))!=1)
      {
        contador<-contador+1
    }
    return(contador)
}

Vectorizada <- function(parametros){
  
      mapply(FUN=funcion4.foo, rep( parametros[1], parametros[2] ))
  }
set.seed(0)
#n <- 10**6
t2 <- Sys.time()
vec <- Vectorizada( c(0.01,n) )
t2 <- Sys.time() - t2
t2
sum(p2001-vec)#iguales




v6 <- (sort(unique(p2001)))
plot(prop.table(table(p2001)),col="blue",xlab = "",ylab="")
par(new=TRUE)
plot(v6,dgeom(v6,0.01),col="red",xlab = "águilas",ylab="proporción")
###########
v6 <- (sort(unique(vec)))
plot(prop.table(table(vec)),col="blue",xlab = "",ylab="")
par(new=TRUE)
plot(v6,dgeom(v6,0.01),col="red",xlab = "águilas",ylab="proporción")
sum(p2001-vec)
####################EJEMPLO SERIO EN LINUX 

funcion4.foo <- function(p)
{
  contador <- 1
  while( sample( c(1, 0), 1, replace = T, prob = c(p,1-p))!=1)
  {
    contador<-contador+1
  }
  return(contador)
}
Vectorizada <- function(parametros){
  mapply(FUN=funcion4.foo, rep( parametros[1], parametros[2] ))
}
n <- 10**5*1
library(parallel)
set.seed(0)
t2 <- Sys.time()
vec <- Vectorizada( c(0.01,n) )
t2 <- Sys.time() - t2
t2

Vectorizada.multi <- function(parametros){
  mcmapply(FUN=funcion4.foo, rep( parametros[1], parametros[2] ), mc.cores = 3)
}
?mcmapply
set.seed(0)
t3 <- Sys.time()
vec2 <- Vectorizada.multi( c(0.01,n) )
t3 <- Sys.time() - t3
t3
sum(vec-vec2) #son iguales ?