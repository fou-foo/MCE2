#####################################  DEFINIR FUNCIONES AUXILIARES ########################################################

throwUntilRSucc <- function(p, r)  {
  # Funcion que simula el lanzar una moneda injusta, donde p es la probabilidad de obtener aguila ('A') 
  # y (1-p) es la probabilidad de obtener sol ('S'), que regresa el numero de tiros necesarios hasta obtener
  # r Aguilas. (Contando el ultimo tiro en que se obtiene la ultima águila).
  # Entradas:
  # p --- Flotante entre 0 y 1 (Probabilidad de obtener aguila)
  # r --- Numero de aguilas a obtener hasta detener el experimento
  
  Nturns = 0
  Nsucces = 0
  while (TRUE) {
    Nturns  = Nturns + 1
    if (sample(c('A', 'S') , 1, prob = c(p, 1-p)) == 'A') {
      Nsucces = Nsucces + 1
    }
    if (Nsucces == r) {
      break
    }
  }
  
  return(Nturns)
  
}

getCoinData <- function(N, p, r) {
  # Usar la funcion throwUntilRSucc N veces para generar los datos que se nos piden.
  # (En realidad son datos correspondientes a una BinomialNegativa con parametros r y p, como vimos en clase)
  # Entradas:
  # N --- Numero de veces a repetir el experimento.
  # p --- Probabilidad de obtener un aguila
  # r --- Numero de aguilas a obtener necesarias para detener el exprimento.
  
  return(replicate(N, throwUntilRSucc(p, r)))
  
}


#####################################   GENERAR GRAFICAS PEDIDAS ########################################################
# OBSERVACION
# Para que la PMF de la binomial negativa que trae programada R coincida con la que nosotros hicimos, hay que notar
# que en R la variable aleatoria es F = el numero de fallos obtenidos antes de observar r éxitos, mientras que para 
# nuestro caso la variable aleatoria de Interes es X = Numero de Tiros obtenidos hasta obtener r éxitos. 
# Solo hay que observar que Num Tiros = Num Fallos + Num exitos ----> F = X - r
# Por eso generamos los valores teóricos de la binomial negativa con esos valores de sus parametros.
# (Su soporte en este caso es F = 0, 1, 2, .... para los valores correspondientes de X = r, r+1, r+2, ... )

N = 1e6
for( p in c(0.2, 0.1)) {
  for (r in c(2, 7)) {
    
  # Obtener datos de la simulacion
  data_sim = getCoinData(N,p,r)
  # Graficar datos similados en rojo
  plot(table(data_sim) / N, xlab = "Numero de Tiros", ylab = "Frecuencia Relativa", col=rgb(1,0,0), main = paste("Simulacion vs Prob Teorica (p = ", p, " r = ", r, ")"))
  # Obtener maximo de los datos simulados para saber hasta donde generar la pmf teorica
  max_sim = max(data_sim)
  # Generar datos teoricos y graficarlos
  data_teo = dnbinom(0:(max_sim-r), r, p)
  lines( r:max_sim  , data_teo, col=rgb(0,0,1), type = 'h')
  
  }
}

