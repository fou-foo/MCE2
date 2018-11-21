hasta<-function(p,r,n){
  x<-c(1,2)
  v<-rep(0,n)
  rep<-0
  c<-0
  aux<-0
  for(i in 1:n){
    c=0
    r=0
    while(r<3){
      aux<-sample(x,size=1,replace=TRUE,prob=c(p,1-p))
      c=c+1
      if(aux==1){
        r=r+1
      }
    }
    v[i]=c
   
  }
  return(v)
}
###r=2 p=0.2
vec<-hasta(0.2,2,10^4)
v<-table(factor(vec,levels = 2:max(vec)))
t<-table(vec)
tn<-t/10^4
plot(tn,col="blue")
bi<-rnbinom(10^4-2,size=2,prob=.2)##
curve(dnbinom(x-2,2,.2),from = 3,to=70,add = T,n=7)
table(factor(vec,levels = 0:max(vec)))

###r=2 p=0.1
vec<-hasta(0.1,2,10^4)
v<-table(factor(vec,levels = 2:max(vec)))
t<-table(vec)
tn<-t/1000
plot(tn,col="blue")##

###r=7 p=0.2
vec<-hasta(0.2,7,10^4)
v<-table(factor(vec,levels = 7:max(vec)))
t<-table(vec)
tn<-t/1000
plot(tn,col="blue")##


###r=7 p=0.1
vec<-hasta(0.1,7,10^4)
v<-table(factor(vec,levels = 7:max(vec)))
t<-table(vec)
tn<-t/1000
plot(tn,col="blue")##

