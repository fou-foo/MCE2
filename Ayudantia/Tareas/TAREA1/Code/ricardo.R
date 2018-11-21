library(ggplot2)

#### 1
#b)
x1<-0
x2<-0
x3<-0
for(i in 19:29){
  x1<-x1+i
  x2<-x2+i^2
  x3<-x3+i^3
}
x1/11
x2/11
x3/11
#c)
x<-0
y<-0
for(i in 1:20){
  x<-x+i
  y<-y+i^2
}
(.05*x)^2
.05*y
#d)
y<-0
for(i in 1:6){
  y<-y+i
}
y/6


##### 3
dbinom(2,12,.1)+dbinom(1,12,.1)+dbinom(0,12,.1)
1-pbinom(0,6,.11087)
1-pbinom(2,6,.11087)


#### 4
choose(12,3)
8*3/12


#### 5
#a)
dbinom(0,12,.1)+dbinom(1,12,.1)+dbinom(2,12,.1)
pbinom(2,12,.1)

dbinom(2,6,.11087)+dbinom(1,6,.11087)+dbinom(0,6,.11087)
1-pbinom(0,6,.11087)
muestra<-sample(1:10,10000)


par(mfrow=c(1,2))
plot(rep(.2,5),type='h',main="PMF Uniforme Discreta n=5",xlab="x",ylab="f(x)")
plot(stepfun(1:5, seq(0,1,.2),right = T),verticals=F,ylim=c(0,1),main="CDF Uniforme Discreta n=5",xlab="(x)",ylab="F(x)")

plot(rep(.1,10),type='h',main="PMF Uniforme Discreta n=10",xlab="(x)",ylab="Probabilidad")
plot(stepfun(1:10, seq(0,1,.1),right = T),verticals=F,ylim=c(0,1),main="CDF Uniforme Discreta n=10",xlab="(x)",ylab="F(x)")

plot(rep(.02,50),type='h',main="PMF Uniforme Discreta n=50",xlab="(x)",ylab="Probabilidad")
plot(stepfun(1:50, seq(0,1,.02),right = T),verticals=F,ylim=c(0,1),main="CDF Uniforme Discreta n=50",xlab="(x)",ylab="F(x)")

par(mfrow=c(1,1))

#c)
set.seed(13)
muestra<-sample(1:10,10000,replace=T,prob=NULL)
mean(muestra)
var(muestra)
tabla<-table(muestra)
tabla
matriz<-matrix(0,10,2)
matriz[,1]<-1:10
matriz[,2]<-tabla
colnames(matriz)<-c("x","frecuencia")
df<-data.frame(x=as.character(matriz[,1]),frecuencia=matriz[,2])
plot1<-ggplot(as.data.frame(df),aes(x=x,y=frecuencia))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  geom_text(aes(label=frecuencia), vjust=-0.3, size=3.5)+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10"))
print(plot1)
#muy bien 

##6
conteos<-rep(0,10E6)
espacio_muestral<-c("aguila","sol")
i<-1
t <- proc.time()
while(i<=10E6){
  lanzamientos<-sample(espacio_muestral,10,replace=T,prob=NULL)
  conteos[i]<-length(which(lanzamientos=="aguila"))
  if(i<4){
    print(lanzamientos)
    print(conteos[i])
  }
  i<-i+1
  if(i%%1000000==0){
    print(i)
  }
}
proc.time()-t
tabla<-table(conteos)

matriz<-matrix(0,11,2)
matriz[,1]<-0:10
matriz[,2]<-tabla
colnames(matriz)<-c("x","frecuencia")
df<-data.frame(x=as.character(matriz[,1]),frecuencia=matriz[,2])
plot1<-ggplot(as.data.frame(df),aes(x=x,y=frecuencia))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  geom_text(aes(label=frecuencia), vjust=-0.3, size=3.5)+
  scale_x_discrete(limits=c("0","1","2","3","4","5","6","7","8","9","10"))
print(plot1)

n=11
matriz<-matrix(0,11,2)
matriz[,1]<-0:10
matriz[,2]<-tabla/length(conteos)
colnames(matriz)<-c("x","proporciones")
df<-data.frame(x=(as.character(matriz[,1])),proporciones=matriz[,2])
plot1<-ggplot(as.data.frame(df),aes(x=x,y=proporciones))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  geom_text(aes(label=trunc(proporciones*1000)/1000), vjust=-0.3, size=3.5)+
  scale_x_discrete(limits=c("0","1","2","3","4","5","6","7","8","9","10"))
print(plot1)

df<-data.frame(x=(matriz[,1]),proporciones=matriz[,2])
plot1<-ggplot(as.data.frame(df),aes(x=x,y=proporciones))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  geom_text(aes(label=trunc(proporciones*1000)/1000), vjust=-0.3, size=3.5)+
  stat_function(fun = dbinom,n=n, color="red",args = list(size =10, prob =.5))
print(plot1)








conteos2<-rep(0,10E6)
i<-1
t <- proc.time()
while(i<=10E6){
  lanzamientos2<-sample(espacio_muestral,10,replace=T,prob=c(.3,.7))
  conteos2[i]<-length(which(lanzamientos2=="aguila"))
  if(i<4){
    print(lanzamientos2)
    print(conteos2[i])
  }
  i<-i+1
  if(i%%1000000==0){
    print(i)
  }
}
proc.time()-t
tabla<-table(conteos2)


matriz<-matrix(0,11,2)
matriz[,1]<-0:10
matriz[,2]<-tabla
colnames(matriz)<-c("x","frecuencia")
df<-data.frame(x=as.character(matriz[,1]),frecuencia=matriz[,2])
plot1<-ggplot(as.data.frame(df),aes(x=x,y=frecuencia))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  geom_text(aes(label=frecuencia), vjust=-0.3, size=3.5)+
  scale_x_discrete(limits=c("0","1","2","3","4","5","6","7","8","9","10"))
print(plot1)

n=11
matriz<-matrix(0,11,2)
matriz[,1]<-0:10
matriz[,2]<-tabla/length(conteos2)
colnames(matriz)<-c("x","proporciones")
df<-data.frame(x=(as.character(matriz[,1])),proporciones=matriz[,2])
plot1<-ggplot(as.data.frame(df),aes(x=x,y=proporciones))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  geom_text(aes(label=trunc(proporciones*1000)/1000), vjust=-0.3, size=3.5)+
  scale_x_discrete(limits=c("0","1","2","3","4","5","6","7","8","9","10"))
print(plot1)

df<-data.frame(x=(matriz[,1]),proporciones=matriz[,2])
plot1<-ggplot(as.data.frame(df),aes(x=x,y=proporciones))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  geom_text(aes(label=trunc(proporciones*1000)/1000), vjust=-0.3, size=3.5)+
  stat_function(fun = dbinom,n=n, color="red",args = list(size =10, prob =.3))
print(plot1)




#### 7

#A)
#I
px0<-choose(123,0)*(.31^0)*((1-.31)^(123-0))
px123<-choose(123,123)*(.31^123)*((1-.31)^(123-123))
px62<-choose(123,62)*(.31^62)*((1-.31)^(123-62))
#II
p0_x_10<-choose(123,0)*(.31^0)*((1-.31)^(123-0))+
  choose(123,1)*(.31^1)*((1-.31)^(123-1))+
  choose(123,2)*(.31^2)*((1-.31)^(123-2))+
  choose(123,3)*(.31^3)*((1-.31)^(123-3))+
  choose(123,4)*(.31^4)*((1-.31)^(123-4))+
  choose(123,5)*(.31^5)*((1-.31)^(123-5))+
  choose(123,6)*(.31^6)*((1-.31)^(123-6))+
  choose(123,7)*(.31^7)*((1-.31)^(123-7))+
  choose(123,8)*(.31^8)*((1-.31)^(123-8))+
  choose(123,9)*(.31^9)*((1-.31)^(123-9))+
  choose(123,10)*(.31^10)*((1-.31)^(123-10))
p0x_10<-p0_x_10-choose(123,0)*(.31^0)*((1-.31)^(123-0))
p0_x10<-p0_x_10-choose(123,10)*(.31^10)*((1-.31)^(123-10))
#III
px11<-1-p0_x_10-choose(123,11)*(.31^11)*((1-.31)^(123-11))
px10<-p0_x_10  
#B)
#I
dbinom(0,123,.31)
dbinom(123,123,.31)
dbinom(62,123,.31)
#II
pbinom(10,123,.31)
pbinom(10,123,.31)-pbinom(0,123,.31)
pbinom(9,123,.31)
#III
1-pbinom(11,123,.31)
pbinom(10,123,.31)

#C)
cuantil<-function(p){
i<-0
suma<-0
while(suma<=p){
  suma<-suma+dbinom(i,123,.31)
  i<-i+1
}
q<-i-1
q
}
cuantil(.25)
cuantil(.5)
cuantil(.75)

qbinom(.25,123,.31)
qbinom(.5,123,.31)
qbinom(.75,123,.31)


#### 8
grises<-rep(0,10^6)
bg<-46
bb<-49
n<-20
i<-1
espacio_muestral<-c(rep("bg",bg),rep("bb",bb))

ocho<-function(){
  muestra<-sample(espacio_muestral,n,replace = F)
  return(length(which(muestra=="bg")))
}
muestra<-sample(espacio_muestral,n,replace = F)
grises[1]<-length(which(muestra=="bg"))
muestra; grises[1]
muestra<-sample(espacio_muestral,n,replace = F)
grises[2]<-length(which(muestra=="bg"))
muestra; grises[2]
muestra<-sample(espacio_muestral,n,replace = F)
grises[3]<-length(which(muestra=="bg"))
muestra; grises[3]

grises[4:10E6]<-replicate((10E6)-3,ocho())
tabla<-table(grises)
n=21
matriz<-matrix(0,21,2)
matriz[,1]<-0:20
matriz[,2]<-c(tabla,0)

colnames(matriz)<-c("x","frecuencia")
df<-data.frame(x=as.character(matriz[,1]),frecuencia=matriz[,2])
plot1<-ggplot(as.data.frame(df),aes(x=x,y=frecuencia))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  geom_text(aes(label=frecuencia), vjust=-0.3, size=3.5)+
  scale_x_discrete(limits=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"))
print(plot1)

matriz[,2]<-c(tabla/length(grises),0)
df<-data.frame(x=(matriz[,1]),proporciones=matriz[,2])
plot1<-ggplot(as.data.frame(df),aes(x=x,y=proporciones))+
  geom_bar(stat = "identity",fill=I("#3366FF"))+
  geom_text(aes(label=trunc(proporciones*1000)/1000), vjust=-0.3, size=3.5)+
  stat_function(fun = dhyper,n=n, color="red",args = list(m =bg, n =bb,k=20))
print(plot1)
