# Obtener elementos (46 bolas grises , 49 bolas blancas y revolver)
bolas <-sample(c(rep('g',46), rep('b', 49)))

#Obtener datos de bolas grises:
num_grises = replicate(1000000, sum((sample(bolas, 20, replace = FALSE) == 'g')))

# Mostrar primeros 3 resultados:
print("Numero de bolas grises para primeros 3 experimentos:")
print(num_grises[1:3])


# Sacar frecuencias y graficar, Unname es para quitarle el label y que print se vea bonito.
freqs = table(num_grises)
plot(freqs, xlab = "Numero de bolas grises", ylab = "Frecuencia", main= "Histograma de frecuencias", xlim = c(0,20), cex.lab = 0.8, cex.main = 0.9)
cat("\n")
print(paste("La probabilidad de obtener exactamente 5 bolas grises al realizar el experimento es:", unname(freqs["5"] / 1000000)))


# Ahora hacemos histograma de frecuencias relativas (rojo) junto con los valores de la  hipergeometrica asociada al lado (azul)
plot( freqs / 1000000 , xlab = "Numero de bolas grises", ylab = "Frecuencia relativa / Probabilidad", main= "Frecuencia Relativa e Hipergeometrica Asociada", col = rgb(1,0,0), xlim = c(0,20), cex.lab=0.8, cex.main = 0.9)

data_teorica = dhyper(0:20, 46, 49, 20)
lines( 0:20 + 0.2, data_teorica, col=rgb(0,0,1), type = 'h')

cat("\n")
print("En rojo se muestran las frecuencias relativas obtenidas con la simulación, y en azul los datos teóricos de la distribución hipergeométrica asociada.")
# checa los comentarios anteriores sobre la funcion 'legend'