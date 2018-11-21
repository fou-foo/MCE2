#ej1
1-pgeom(4,.5)
1/.5

#ej2
1-pnbinom(1,3,.15)
3/.15

#ej3
dbinom(3,10,.4)

#ej4
lanzamientos<- function(p,N){
resultados<-rep(0,N)
for(i in 1:N){
  contador<-0
  muestra<-"s"
  while(muestra!="a"){
    muestra<-sample(x = c("a","s"),size = 1,replace = F,prob = c(p,1-p))
    contador<-contador+1
  }
  resultados[i]<-contador
}
return(resultados)
}

#fija una semila siempre que realices simulaciones
set.seed(0)
v1<-lanzamientos(.5,10^4)
v2<-lanzamientos(.1,10^4)
v3<-lanzamientos(.01,10^4)
t1<-table(v1)
t2<-table(v2)
t3<-table(v3)
df1<-data.frame(x=1:as.integer(names(t1[length(t1)])),proporciones=0)
df2<-data.frame(x=1:as.integer(names(t2[length(t2)])),proporciones=0)
df3<-data.frame(x=1:as.integer(names(t3[length(t3)])),proporciones=0)
for(i in 1:length(df1$x)){
  if(is.na(t1[as.character(i)])==F){
    df1$proporciones[i]<-t1[as.character(i)]
  }
}
for(i in 1:length(df2$x)){
  if(is.na(t2[as.character(i)])==F){
    df2$proporciones[i]<-t2[as.character(i)]
  }
}
for(i in 1:length(df3$x)){
  if(is.na(t3[as.character(i)])==F){
    df3$proporciones[i]<-t3[as.character(i)]
  }
}
dgeometrica<-function(x,p){
  return(dgeom(x-1,p))
}
df1$proporciones<-df1$proporciones/10^4
df2$proporciones<-df2$proporciones/10^4
df3$proporciones<-df3$proporciones/10^4

library(ggplot2)
plot1<-ggplot(as.data.frame(df1),aes(x=x,y=proporciones))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  geom_text(aes(label=round(proporciones,digits = 2)), vjust=-0.3, size=3.5)+
  stat_function(fun = dgeometrica,n=length(df1$x), color="red",args = list(p =.5))

print(plot1)
plot1<-ggplot(as.data.frame(df2),aes(x=x,y=proporciones))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
#  geom_text(aes(label=round(proporciones,digits = 2)), vjust=-0.3, size=3.5)+
  stat_function(fun = dgeometrica,n=length(df2$x), color="red",args = list(p =.1))

print(plot1)
#recuerda usar el operador ' <- '
plot1<-ggplot(as.data.frame(df3),aes(x=x,y=proporciones))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  #geom_text(aes(label=round(proporciones/104,digits = 2)), vjust=-0.3, size=3.5)+
  stat_function(fun = dgeometrica,n=length(df3$x), color="red",args = list(p =.01))

print(plot1)


v1<-lanzamientos(.5,10^6)
v2<-lanzamientos(.1,10^6)
v3<-lanzamientos(.01,10^6)
mean(v1);mean(v2);mean(v3)
sd(v1);sd(v2);sd(v3)
t1<-table(v1)
t2<-table(v2)
t3<-table(v3)
df1<-data.frame(x=1:as.integer(names(t1[length(t1)])),proporciones=0)
df2<-data.frame(x=1:as.integer(names(t2[length(t2)])),proporciones=0)
df3<-data.frame(x=1:as.integer(names(t3[length(t3)])),proporciones=0)
for(i in 1:length(df1$x)){
  if(is.na(t1[as.character(i)])==F){
    df1$proporciones[i]<-t1[as.character(i)]
  }
}
for(i in 1:length(df2$x)){
  if(is.na(t2[as.character(i)])==F){
    df2$proporciones[i]<-t2[as.character(i)]
  }
}
for(i in 1:length(df3$x)){
  if(is.na(t3[as.character(i)])==F){
    df3$proporciones[i]<-t3[as.character(i)]
  }
}
dgeometrica<-function(x,p){
  return(dgeom(x-1,p))
}
df1$proporciones<-df1$proporciones/10^6
df2$proporciones<-df2$proporciones/10^6
df3$proporciones<-df3$proporciones/10^6

plot1<-ggplot(as.data.frame(df1),aes(x=x,y=proporciones))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  geom_text(aes(label=round(proporciones,digits = 2)), vjust=-0.3, size=3.5)+
  stat_function(fun = dgeometrica,n=length(df1$x), color="red",args = list(p =.5))

print(plot1)
plot1<-ggplot(as.data.frame(df2),aes(x=x,y=proporciones))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
#  geom_text(aes(label=round(proporciones,digits = 2)), vjust=-0.3, size=3.5)+
  stat_function(fun = dgeometrica,n=length(df2$x), color="red",args = list(p =.1))

print(plot1)
plot1<-ggplot(as.data.frame(df3),aes(x=x,y=proporciones))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  #geom_text(aes(label=round(proporciones/104,digits = 2)), vjust=-0.3, size=3.5)+
  stat_function(fun = dgeometrica,n=length(df3$x), color="red",args = list(p =.01))

print(plot1)




### ej 5 
lanzamientos2<- function(p,r,N){
  resultados<-rep(0,N)
  for(i in 1:N){
    contador<-0
    fallas<-0
    while(fallas!=r){
      muestra<-sample(x = c("a","s"),size = 1,replace = F,prob = c(p,1-p))
      if(muestra=="a"){
        fallas<-fallas+1
      }
      contador<-contador+1
    }
    resultados[i]<-contador
  }
  return(resultados)
}

v1<-lanzamientos2(.2,2,10^6)
v2<-lanzamientos2(.1,2,10^6)
v3<-lanzamientos2(.2,7,10^6)
v4<-lanzamientos2(.1,7,10^6)

t1<-table(v1)
t2<-table(v2)
t3<-table(v3)
t4<-table(v4)

df1<-data.frame(x=1:as.integer(names(t1[length(t1)])),proporciones=0)
df2<-data.frame(x=1:as.integer(names(t2[length(t2)])),proporciones=0)
df3<-data.frame(x=1:as.integer(names(t3[length(t3)])),proporciones=0)
df4<-data.frame(x=1:as.integer(names(t4[length(t4)])),proporciones=0)

for(i in 1:length(df1$x)){
  if(is.na(t1[as.character(i)])==F){
    df1$proporciones[i]<-t1[as.character(i)]
  }
}
for(i in 1:length(df2$x)){
  if(is.na(t2[as.character(i)])==F){
    df2$proporciones[i]<-t2[as.character(i)]
  }
}
for(i in 1:length(df3$x)){
  if(is.na(t3[as.character(i)])==F){
    df3$proporciones[i]<-t3[as.character(i)]
  }
}
for(i in 1:length(df4$x)){
  if(is.na(t4[as.character(i)])==F){
    df4$proporciones[i]<-t4[as.character(i)]
  }
}
dbinomialnegativa<-function(x,r,p){
  return(dnbinom(x-r,r,p))
}
df1$proporciones<-df1$proporciones/10^6
df2$proporciones<-df2$proporciones/10^6
df3$proporciones<-df3$proporciones/10^6
df4$proporciones<-df4$proporciones/10^6

plot1<-ggplot(as.data.frame(df1),aes(x=x,y=proporciones))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  # geom_text(aes(label=round(proporciones,digits = 2)), vjust=-0.3, size=3.5)+
  stat_function(fun = dbinomialnegativa,n=length(df1$x), color="red",args = list(r=2,p =.2))
print(plot1)

plot1<-ggplot(as.data.frame(df2),aes(x=x,y=proporciones))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  # geom_text(aes(label=round(proporciones,digits = 2)), vjust=-0.3, size=3.5)+
  stat_function(fun = dbinomialnegativa,n=length(df2$x), color="red",args = list(r=2,p =.1))
print(plot1)

plot1<-ggplot(as.data.frame(df3),aes(x=x,y=proporciones))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  # geom_text(aes(label=round(proporciones,digits = 2)), vjust=-0.3, size=3.5)+
  stat_function(fun = dbinomialnegativa,n=length(df3$x), color="red",args = list(r=7,p =.2))
print(plot1)

plot1<-ggplot(as.data.frame(df4),aes(x=x,y=proporciones))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  # geom_text(aes(label=round(proporciones,digits = 2)), vjust=-0.3, size=3.5)+
  stat_function(fun = dbinomialnegativa,n=length(df4$x), color="red",args = list(r=7,p =.1))
print(plot1)


##ej7
1-dpois(2,6)-dpois(1,6)-dpois(0,6)

## ej 8
integrand<-function(x){6*x*(1-x)}
integrate(f = integrand,0,.4)
(3*(.4^2))-(2*(.4^3))

dbinom(2,6,.352)

##ej 9
procpoisson<-function(t,lambda){
  delta<-t/1000
  resultados<-data.frame(x=seq(0,t,delta),ocurrencias=rep(0,(t/delta)+1))
  aux<-0
    for(k in 0:((t/delta))){
    if((rbinom(1,1,(lambda*delta)+(10^-6)))==1){
      aux<-aux+1
    }
    resultados$ocurrencias[k+1]<-aux
    }
  return(resultados)
}

ej<-procpoisson(10,2)
plot(stepfun(ej$x[2:length(ej$x)], ej$ocurrencias,right = T),verticals=F,main="")
ej<-procpoisson(10,2)
plot(stepfun(ej$x[2:length(ej$x)], ej$ocurrencias,right = T),verticals=F,main="")
ej<-procpoisson(10,2)
plot(stepfun(ej$x[2:length(ej$x)], ej$ocurrencias,right = T),verticals=F,main="")


procpoisson2<-function(t,lambda){
  delta<-t/1000
  aux<-0
  for(k in 0:((t/delta))){
    if((rbinom(1,1,(lambda*delta)+(10^-6)))==1){
      aux<-aux+1
    }
  }
  return(aux)
}

ej2<-rep(0,10^4)
ej2<-replicate(10^4,procpoisson2(1,.5))
plot(table(ej2)/10^4,main="",xlab = "x",ylab = "f(x)")
curve(dpois(x,.5),from = 0,to = 5,n=6,col="red",add = T)

