###################################INCISO A)############################

# Obtener datos del numero de Aguilas(Tails)
num_heads = replicate( 1e6, sum((sample(c("H","T"), 10, replace = TRUE)) == "T"))
# Mostrar primeros 3 Resultados:
print("Numero de aguilas obtenido en los primeros 3 experimentos:")
print(num_heads[1:3])
# Mostrar Grafica de frecuencias:
plot(table(num_heads), xlab = "Número de Aguilas", ylab = "Frecuencia", main = "Frecuencia del Número de Águilas")
# Mostrar Probabilidades:
plot(table(num_heads) / length(num_heads), xlab = "Número de Aguilas", ylab = "Probabilidad", main = "P(Num Águilas = K) , P(Águila) = 0.5", col=rgb(1,0,0,1))

####################################INCISO B)##########################


#Obtener probabilidades:
probs_binom = dbinom(0:10, 10, 0.5)

# Plotear Binomial (tralsadamos un poco en x para dar el efecto deseado)
# OBS: En Azul se muestra la Binomial Teórica, en Rojo la Probabilidad Calculada con las frecuencias
lines(0:10 + .15, probs_binom , type="h", col = rgb(0,0,1,1))
cat("\n")
print("En rojo se muestran los datos simulados, en azul los datos teóricos obtenidos a partir de la función de distribución.")
#puedes agregar leyendas a las graficas
legend(.1,.1, legend='Leyenda')
#####################################INCISO C##############################

# Generar Datos
num_heads = replicate( 1e6, sum((sample(c("H","T"), 10, replace = TRUE, prob = c(0.7, 0.3))) == "T"))
probs_binom = dbinom(0:10, 10 ,0.3) # utiliza el operador '<-' para asignar tienen ventajas sobre el '='

# Probabilidades con Frecuencia (Rojo)
plot(table(num_heads) / length(num_heads), xlab = "Número de Aguilas", ylab = "Probabilidad", main = "P(Num Águilas = K) , P(Águila) = 0.3", col=rgb(1,0,0,1))
# Plotear Probabilidades Teoricas (Binomiales) en Azul Trasladadas
lines(0:10 + .15, probs_binom , type="h", col = rgb(0,0,1,1))

cat("\n")
print("La diferencia al cambiar P(Aguila) = 0.5 por p(Aguila) = 0.3 es que la distribución binomial correspondiente ahora esta sesgada (skewed) a la derecha.")
#bien 