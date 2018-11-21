#Alumna: Ramirez Islas Cynthia Mariangel
#Tarea 1 
#Ejercicio 8

#Una urna contiene 46 bolas grises y 49 bolas blancas. Usando la funcion sample en R, simule
#la extraccion sin reemplazamiento de 20 de estas bolas y cuente el numero de bolas grises
#que obtuvo

grises<-0
for(i in 1:100){
  for (i in 0:20){
urna<-sample(c(0:1),1, replace = FALSE, c(46/95,49/95)  )

grises[i]<-c(sum(urna==1))
#print(grises[i])
}
  if(i<=3) {
    print(paste("# de bolas grises",i)  )
    print(grises[i])
    }
  
} # no estas simuando el numero correcto de veces checa tu for dentro del for

#Cálculo de frecuencias
frecuencia<- data.frame(table(grises))
#Cálculo de probabilidades
probabilidad<- prop.table(table(grises))

#Gráfica de la hipergeométrica
plot(dhyper(x = c(1:20),95,46,20,log = FALSE), col="blue")


#te falto la proba que pregunta al final te doy 1/3 del ejercicio