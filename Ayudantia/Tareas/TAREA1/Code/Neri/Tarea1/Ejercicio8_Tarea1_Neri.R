#Tarea1_Ejercicio8
#Felipe Neri Chairez Cuellar
#
bolas<-c(rep(1,46),rep(2,49))
sample(bolas,size=20,replace=FALSE,prob=NULL)
#para traducir que 1=grises, 2=blancas
resultado_bolas<-factor(bolas)
levels(resultado_bolas)<- c("grises","blancas")
tabla_frec_bolas<-table(sample(resultado_bolas,size=20,replace=FALSE,prob=NULL))
tabla_frec_bolas
#
#problemas con el ciclo for
#
i<-NULL
for(i in 1:1000000){
  sample(bolas,size=20,replace=FALSE,prob=NULL)
  resulado_bolas<-factor(bolas)
  levels(resultado_bolas)<-c("grises","blancas")
  tabla_frec_bolas<-table(sample(resultado_bolas,size=20,replace=FALSE,prob=NULL))
  tabla_frec_bolas
}
# que paso despues ?
# y que paso con las buenas practicas de programacion en R que les comparti ?
# solo te falto almacenar tus resultados te doy .5 del ejercicio