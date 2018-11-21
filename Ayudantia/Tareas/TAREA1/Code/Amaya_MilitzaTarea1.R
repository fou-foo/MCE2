# Ejercicio 5a
# Distribución uniforme para n=5
n<-5
x<-c(1:n)
fx<-rep(1/n,n)
plot(x,fx,type = "h",xlab = "x",ylab = "f(x)",main = "Función de masa")
fdacum1<-plot(ecdf(x),xlab = "x",ylab = "F(x)",main = "Función Acumulada")

#Distribución uniforme para n=10
n<-10
x<-c(1:n)
fx<-rep(1/n,n)
plot(x,fx,type = "h",xlab = "x",ylab = "f(x)",main = "Función de masa")
fdacum1<-plot(ecdf(x),xlab = "x",ylab = "F(x)",main = "Función Acumulada")

#Distribución uniforme para n=50
n<-50
x<-c(1:n)
fx<-rep(1/n,n)
plot(x,fx,type = "h",xlab = "x",ylab = "f(x)",main = "Función de masa")
fdacum1<-plot(ecdf(x),xlab = "x",ylab = "F(x)",main = "Función Acumulada")

#ejercicio 5c
x<-c(1:10)
set.seed(13)
muestra1<-sample(x,size = 10000,replace = T,prob = NULL)
tabla<-table(muestra1)
tabla
media<-mean(muestra1)
varianza<-var(muestra1)
media
varianza

#ejercicio 5d
hist(muestra1)
#se ve como si no estuviese definda en todo el soporte , prueba con la linea:
barplot(table(muestra1))

#ejercicio 6a y 6b

moneda<-c(0,1)    # 1 son águilas

muestra6<-sample(moneda,size = 10,replace = T,prob = NULL)

muestra6

numaguilas<-sum(muestra6)

numaguilas # conteo de águilas en 10 lanzamientos

frectiro<-c(rep(0,1000000)) #Mide el número de águilas

for(i in 1:1000000){ #Lanzamiento de moneda 1000000 veces
  frectiro[i]<-sum(sample(moneda, size = 10, replace=T, prob = NULL))
  }

hist(frectiro)

m<-c(0:10)

table(frectiro)

prop.table(table(frectiro))

plot(prop.table(table(frectiro)),type="h",col="blue",xlab = "aguilas",ylab = "frecuencia",main = "Comparación con Binomial")

par(new=T)

plot(dbinom(m,10,.5),type = "h",axes = FALSE,col="pink",xlab = " ",ylab = " ",main = " ")

#ok
#ejercicio 6c

moneda<-c(0,1) # 1 son aguilas

frectiro<-c(rep(0,1000000))

for(i in 1:1000000){
  frectiro[i]<-sum(sample(moneda, size = 10, replace=T, prob = c(.7,.3))) # los eventos ya no tienen la misma probabilidad
}

hist(frectiro)

m<-c(0:10)

table(frectiro)

prop.table(table(frectiro))

plot(prop.table(table(frectiro)),type="h",col="blue",xlab = "aguilas",ylab = "frecuencia",main = "Comparación con Binomial")

par(new=T)

plot(dbinom(m,10,.3),type = "h",axes = FALSE,col="pink",xlab = " ",ylab = " ",main = " ")

#ejercicio7a

n<-123

p<-.31

# P(x=0)
x<-0

choose(123,x)*(p)^(x)*(1-p)^(n-x)

# P(x=123)

x<-123

choose(123,x)*(p)^(x)*(1-p)^(n-x)

# P(x=62)

x<-62

choose(123,x)*(p)^(x)*(1-p)^(n-x)

#P(0<=x<=10)

a<-0

b<-10

px1<-0

for(i in a:b){ # Suma las probabilidades acumuladas en el intervalo
  px1<-px1+choose(123,i)*(p)^(i)*(1-p)^(n-i)
}
px1

# P(0<x<=10)

a<-1

b<-10

px1<-0

for(i in a:b){ # Suma las probabilidades acumuladas en el intervalo
  px1<-px1+choose(123,i)*(p)^(i)*(1-p)^(n-i)
}
px1

# P(0<=x<10)

a<-0

b<-9

px1<-0

for(i in a:b){ # Suma las probabilidades acumuladas en el intervalo
  px1<-px1+choose(123,i)*(p)^(i)*(1-p)^(n-i)
}
px1

# P(x>11)

a<-12

b<-123

px1<-0

for(i in a:b){ #Suma las probabilidades acumuladas en el intervalo
  px1<-px1+choose(123,i)*(p)^(i)*(1-p)^(n-i)
}
#la funcion 'choose' esta vectorizada no era necesario el ciclo 'for'
px1

# P(x<=10)

a<-0

b<-10

px1<-0

for(i in a:b){ #Suma las pobabilidades acumuladas en el intervalo
  px1<-px1+choose(123,i)*(p)^(i)*(1-p)^(n-i)
}
px1

#ejercicio 7b
dbinom(0,123,0.31) # P(X=0)
dbinom(123,123,0.31) #P(x=123)
dbinom(62,123,0.31) # P(X=62)

pbinom(10,123,0.31) #P(0<=x<=10)
pbinom(10,123,0.31)-pbinom(0,123,0.31) # P(0<x<=10)
pbinom(9,123,0.31) # P(0<=x<10)

1-pbinom(11,123,0.31) # P(X>11)
#la pobabilidad es tan pequeña que se considera nula
# por lo que al restarla a la unidad el resultado es 1

pbinom(10,123,0.31) # P(X>=10)

#ejercicio 7c
qbinom(0.25,123,0.31) # Cuantil 0.25
qbinom(0.5,123,0.31) #Cuantil 0.50
qbinom(0.75,123,0.31) # Cuantil 0.75

#ejercicio 8a

urna<-c(rep(1,46),rep(0,49)) # 1 son bolas grises

muestra3<-sample(urna,size = 20,replace = F,prob = NULL)

muestra3

numgrises<-sum(muestra3)

numgrises

#ejercicio 8b y 8c

frecgris<-c(rep(0,1000000)) # Mide la frecuencia de la bola gris

for(i in 1:1000000){ # Proceso para simular 1000000 repeticiones
  frecgris[i]<-sum(sample(urna, size = 10, replace=T, prob = NULL))#la grafica final esta mal porque en esta linea muestreas 10 en lugar de 20
}

hist(frecgris) 

m<-c(0:20)

table(frecgris)

prop.table(table(frecgris))

plot(prop.table(table(frecgris)),type="h",col="blue",xlab = "bola gris",ylab = "frecuencia",main = "Comparación Hipergeométrica")

par(new=T)

plot(dhyper(m,46,49,20),type = "h",axes = FALSE,col="pink",xlab = " ",ylab = " ",main = " ")

#ejecicio 8d
dhyper(5,46,49,20)
# casi bien solo se te paso el tamaño del muestreo en la linea 222
#te doy .9 de este ejercicio 




