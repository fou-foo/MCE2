############################################### FUNCIONES AUXILIARES  ##############################################

simBernoulli <- function(N, T_max, lambda) {
  # Funcion que genera N valores de una V.A de Bernoulli con p = lambda * dt = lambda * T_max / N
  # I.e genera los correspondientes N valores de la V.A de Bernoulli para un proceso de poisson en el intervalo
  # [0, T_max] que particionamos en N subintervalos de longitud dt.
  dt = T_max / N 

  # Simular una V.A de Bernoulli con p = Lambda*dt para cada intervalo de los N intervalos
  p = lambda*dt
  outcomes = sample(c(1,0), N, prob = c(p, 1 - p), replace = TRUE)
  
  return(outcomes)
}

plotPoissonPath <- function(N, T_max, lambda) {
  # Funcion que recibe una lista de los resultados de la simulacion realizada con simBernoulli y produce los datos
  # que representan su correspondiente trayectoria de Poisson y la grafican.
  # ENTRADAS:
  # N: Int, Numero de subintervalos
  # Outcomes: Vector de Int, resultado de haber llamado a la función simBernoulli
  
  outcomes = simBernoulli(N, T_max, lambda)
  
  sum = 0
  counts = numeric(N)
  for (i in 1:N) {
    sum = sum + outcomes[i]
    counts[i] = sum
  }
  
  plot(counts, type = "s", main = paste("Trayectoria de Poisson, Lambda = ", lambda), xlab = "Tiempo", ylab = "Numero de Exitos hasta el tiempo t")
  
}

################################################ PRIMERA PARTE  ##################################################
# Plotear 3 trayectorias de poisson con los parametros pedidos

N = 1000
T_max = 10
lambda = 2

for (i in 1:3) {
  plotPoissonPath(N, T_max, lambda)
}


################################################ SEGUNDA PARTE  ###################################################
# Simular un proceso de poisson repetidamente para comparar con la PMF de poisson asociada al experimento.
N = 1e4
T_max = 1
lambda = 0.5

counts = numeric(N)

for (i in 1:N) {
  
  # Generar datos para un proceso poisson
  outcomes = simBernoulli(N, T_max, lambda) 
  
  # Contar exitos
  n = sum(outcomes)
  
  # Guardar en vector
  counts[i] = n
  
}

# Ya tenemos el vector de los datos necesarios para resolver la segunda parte

# Graficar datos simulados con barras rojas:
freqs = table(counts)
plot(freqs / N, col = rgb(1,0,0), main = "Simulación vs Teórico Poisson (lambda = 0.5)", ylab = "Probabilidad / Frecuencia Relativa", xlab = "Numero de Exitos")

# Encimar PMF Teorica de Poisson con lambda = 0.5 y graficarla con Puntos azules por mayor claridad.
max_sim = max(freqs)
data_teo = dpois(0:max_sim, 0.5)
lines( 0:max_sim  , data_teo, col=rgb(0,0,1), type='p', lwd=".5", pch=19)







