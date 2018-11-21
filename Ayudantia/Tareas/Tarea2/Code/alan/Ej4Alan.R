moneda<-function(n,p){
  x<-c(1,2)
  vec<-rep(0,n)
  c<-0
  c1<-1
  aux<-0
  while(c1<=n){
    aux<-sample(x,size=1,replace=TRUE,prob=c(p,1-p))
    c++ #que onda aqui este operador no existe en R
    if(aux==1){
      vec[c1]<-c
      c1<-c1+1
      c<-0
    }
  }
  return(vec)
}
 ###Graficas###########################

vec<-moneda(10000,.1)
q<-prop.table(table(vec))
plot(q,axes=T,ylim=c(0,0.08),col="blue",main="Ej 4 p=0.1 n=10^4")
par(new=T)
z<-c(sort(unique(vec)))
plot(z+1,dgeom(z,.1),type="b",col="red",axes=F)


vec<-moneda(10000,0.5)
q<-prop.table(table(vec))
plot(q,axes=T,ylim=c(0,0.08),col="blue",main="Ej 4 p=0.5 n=10^4")
par(new=T)
z<-c(sort(unique(vec)))
plot(z+1,dgeom(z,0.5),type="b",col="red",axes=F)

vec<-moneda(10000,0.01)
q<-prop.table(table(vec))
plot(q,axes=T,ylim=c(0,0.08),col="blue",main="Ej 4 p=0.01 n=10^4")
par(new=T)
z<-c(sort(unique(vec)))
plot(z+1,dgeom(z,0.01),type="b",col="red",axes=F)
x <- 1
x++
  