##############################################################
# Vectorizacion de la funcion que imputa los datos faltantes
# mediante el metodo hot-deck basado en distancias
# considerando la extension a variables no continuas
##############################################################
METODO.NNI.foo <- function(datosy.r, datosx.r, datosx.m, m)
{
   # Entradas:
     # datosx.r (data.frame): Conjunto de registros donadores
     # datosy.r (numeric): Vector con los valores de los registros donadores
     # datosx.m (data.frame): Conjunto de registros de los renglones a imputar
     # m (numeric) : Escalar con el numero de datos a imputar
  DONANTES.NNI <- vector(mode='numeric', length = m) # fijamos el tamaño de este vector para ahorrar tiempo de ejecucion
  for (j in 1:m)
  {
    # como el algoritmo es secuencial este for no se puede paralelizar
    renglon.imputar <- datosx.m[j,]
            # calculo de la distancia a cada renglon donador al renglon a imputar
    Diferencias <- apply(datosx.r, 1 ,FUN=
                         function (xx)
                         {
                             return(sum( (xx-renglon.imputar)**2)) # norma L2
                          })
    index <- which.min(Diferencias)
    DONANTES <- datosy.r[sample(index, 1)] # muestreo aleatorio
    DONANTES.NNI[j] <- DONANTES
  }
  DONANTES.NNI
}
