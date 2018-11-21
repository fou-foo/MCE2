#####################################   INCISO A ########################################################

throwUntilSuccess <- function(p)  {
  # Funcion que simula el lanzar una moneda injusta, donde p es la probabilidad de obtener aguila ('A') 
  # y (1-p) es la probabilidad de obtener sol ('S'), que regresa el numero de tiros necesarios para obtener
  # la primera Aguila.
  # Entradas:
  # p --- Flotante entre 0 y 1 (Probabilidad de obtener aguila)
  
  Nturns = 0
  while (TRUE) {
    Nturns  = Nturns + 1
    if (sample(c('A', 'S') , 1, prob = c(p, 1-p)) == 'A') {
      break
    }
  }
  
  return(Nturns)
  
}

getDataUntilSuccess <- function(N,p) {
  # Usar la funcion throw until success N veces para generar los datos que se nos piden.
  # Entradas:
  # N --- Numero de veces a repetir el experimento
  # p --- Probabilidad de obtener un aguila
  
  return(replicate(N, throwUntilSuccess(p)))
  
}


############################################   INCISO B   ########################################################
N = 1e4
for( p in c(0.5,0.1,0.01)) {
  data_sim = getDataUntilSuccess(N,p)
  plot(table(data_sim) / N, xlab = "Numero de Tiros", ylab = "Frecuencia Relativa", col=rgb(0,0,1), main = paste("Simulacion vs Prob Teorica (p = ", p, " N = ", N, ")"))
  max_sim = max(data_sim)
  data_teo = dgeom(0:(max_sim-1), p)
  lines( 1:max_sim  , data_teo, col=rgb(1,0,0), type = 'h')
}

# OBSERVAMOS QUE:
# A medida que p decrece, el numero de tiros necesarios para obtener el primer exito va aumentando.
# I.e la distribucion va teniendo un sesgo positivo cada vez mas grande a medida que p disminuye.
# (i.e entre mas chica sea p cada vez es mas probable tener que realizar un mayor numero de tiros para conseguir
# observar r éxitos, lo cual es intuitivamente claro)


###########################################   INCISO C   ########################################################
N = 1e6

print("Estadisticas para las distribuciones generadas ,  N = 1E6")

for( p in c(0.5,0.1,0.01)) {
  data_sim = getDataUntilSuccess(N,p)
  plot(table(data_sim) / N, xlab = "Numero de Tiros", ylab = "Frecuencia Relativa", col=rgb(1,0,0), main = paste("Simulacion vs Prob Teorica (p = ", p, " N = ", N, ")"))
  max_sim = max(data_sim)
  print(paste("Para p = ", p))
  print(paste("Promedio: ", mean(data_sim), " Desviacion Estandar: ", sd(data_sim)))
  cat("\n")
  data_teo = dgeom(0:(max_sim-1), p)
  lines( 1:max_sim  , data_teo, col=rgb(0,0,1), type = 'h')
}

# OBSERVAMOS QUE:
# A Medida que p tiende a cero, la desviacion estandar de una geometrica tiende a mu.
# Esto tiene sentido debido a que mu = 1/p y var = (1-p)/p**2 por lo que cuanto p tiende a cero la varianza tiende
# a 1/p**2 , y por lo tanto la desviacion estandar tiende a 1/p = mu.
# Tambien, lo que ya es obvio a estas alturas es que a medida que N crezca los promedios y varianzas calculados
# tenderán a su valor teórico.



