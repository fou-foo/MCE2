sm<-function(n,p){
  m<-c(1,2)
  v<-rep(0,n)
  cont<-0
  for(i in 1:n){
    aux<-sample(m,size=1,replace=TRUE,prob=c(p,1-p))
    cont=cont+1
    while(aux!=1){
      cont<-cont+1;
      aux<-sample(m,size=1,replace=TRUE,prob=c(p,1-p))
    }
    v[i]=cont
    cont<-0
  }
  return (v)
}
#####greficas#####
vec<-sm(10000,0.1)
q<-prop.table(table(vec))
plot(a,axes=T,ylim=c(0,0.5),col="blue",main="Ej 4 p=0.5 n=10^4")
supongo que es vec 
hist(vec,axes=T,ylim=c(0,0.5),col="blue",main="Ej 4 p=0.5 n=10^4")

par(new=T)
z<-c(sort(unique(vec)))
plot(z+1,dgeom(z,.1),type="b",col="red",axes=F)
# de doy .25