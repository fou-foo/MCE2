# Definir funcion propia para calcular Pbinom(n,k,p):

Prob_binom <- function(n,k,p) {
  # Funcion para calcular la pmf binomial con parametros n y p.
  # Entradas:
  # n (Int, numero de experimentos de bernoulli independientes)
  # p (Float de 0 a 1)
  # k (Int, numero de exitos deseado)
  
  return (choose(n,k)* p^(k) * (1-p)^(n-k))
}

#######################  INCISO A   ################################
print("INCISO A:")
cat("\n")
print(paste("P(X=0):", Prob_binom(123, 0, 0.31)))
print(paste("P(X=123):", Prob_binom(123,123,0.31)))
print(paste("P(X=62):", Prob_binom(123, 62, 0.31)))
print(paste("P(0 <= X <= 10):", sum(Prob_binom(123, 0:10, 0.31))))
print(paste("P(0 < X <= 10):", sum(Prob_binom(123, 1:10, 0.31))))
print(paste("P(0 <= X < 10):", sum(Prob_binom(123, 0:9, 0.31))))
print(paste("P( X > 11):",  1 - sum(Prob_binom(123, 0:11, 0.31))))
print(paste("P( X <= 10):", sum(Prob_binom(123, 0:10, 0.31))))
cat("\n")



##########################  INCISO B  #############################
# Aqui ya usamos las funciones que tiene r (pbinom, dbinom)
print("INCISO B")
cat("\n")
print(paste("P(X=0):", dbinom(0,123,0.31)))
print(paste("P(X=123):", dbinom(123,123,0.31)))
print(paste("P(X=62):", dbinom(62,123,0.31)))
print(paste("P(0 <= X <= 10):",pbinom(10, 123, 0.31)))
print(paste("P(0 < X <= 10):", pbinom(10, 123, 0.31) - dbinom(0, 123, 0.31)))
print(paste("P(0 <= X < 10):", pbinom(10, 123, 0.31) - dbinom(10, 123, 0.31)))
print(paste("P( X > 11):", pbinom(11, 123, 0.31, lower.tail = FALSE)))
print(paste("P( X <= 10):", pbinom(10, 123, 0.31)))


########################### INCISO C ################################
quantileBinom <- function(n,p,q) {
  # Regresar el valor del q-ésimo cuantil para una distribucion Binomial
  # con parametros n y p.
  # Entradas:
  # n (int) , parametro n de la distribucion Binomial deseada
  # p (float entre 0 y 1) , parametro p de la distribucion Binomial deseada
  # q (float entre 0 y 1), cuantil deseado
  
  quantile  = -1    # Se ve raro, pero por la fomra en que lo hicimos es la condicion inicial correcta.
  sum_prob = 0
  while (sum_prob < q) {
    quantile =  quantile + 1
    sum_prob = sum_prob + dbinom(quantile, n, p)
  }
  
  return (quantile)
  
}
cat("\n")
print("INCISO C")
# Imprimir quantiles:
cat("\n")
print(paste("El cuantil 0.25 para una distribucion Binomial con n = 123 y  p = 0.31 es: ", quantileBinom(123,0.31,0.25)))

cat("\n")
print(paste("El cuantil 0.5 para una distribucion Binomial con n = 123 y  p = 0.31 es: ", quantileBinom(123,0.31,0.5)))

cat("\n")
print(paste("El cuantil 0.75 para una distribucion Binomial con n = 123 y  p = 0.31 es: ", quantileBinom(123,0.31,0.75)))

cat("\n")
print("R tiene una funcion que ya calcula los cuantiles, para el caso de la distribucion binomial se llama qbinom.")
# bien 