# Closure para poder paralelizar la funcion 'CarloMagno'
Rellena <- function(dataframe, dataframe.pares, index, crit, lag.max)
{
  # Entradas:
    # dataframe (data.frame): Conjunto de datos original
    # dataframe.pares (data.frame): Tabla con todas las posibles combinaciones por pares de variables
    # index (int): variable anonima
    # crit (character): Sttring con el criterio para determinar el orden del VAR(p) 
    # lag.max (int): Parametro para determinacion del orden del VAR(p) 
  # Salidas: Funcion que determina si dos variables son cointegradas
  data <- dataframe
  Cointegracion.pares <- dataframe.pares
  crit <- crit
  lag.max <- lag.max
  function(index)
  {
    series <- Cointegracion.pares[index, names(Cointegracion.pares)[1:2] ]
    series <- unlist(series)
    datos <- data[, series ]
    p.mini <- VARselect(y= datos, lag.max = lag.max, type = "const")$selection[crit]
    l <- ca.jo(datos, K= p.mini)
    rango1 <- l@cval[ 1  , significancia] # valor critico para rango <=1 El numero magico es porque siempre hay solo dos series
    estadistico1 <- l@teststat[1]
    resultado <- ifelse( estadistico1 < rango1, 'Si', 'No') 
    return(resultado)
  }
}

# Funcion para determinar si existe cointegracion en un VAR con mas de once variables, basado en el paper de 
# Discovering common trends in a large set of disaggregates: statistical procedures and their properties
# En el mismo paper se da otra propuesta para el caso de cientos
# requiere de las librerias 'vars' y parallel  
CarloMagno <- function(data, significancia='1pct', crit = 'FPE(n)')
{
  # Entradas:
  # data (data.frame): Conjunto de datos original
  # significancia (string): Significancia de la prueba '1pct', '5pct', '10pct'
  # crit (string): criterio para seleccion del orden del VAR(p) FIJO DE MOMENTO

  # Salidas: 
    # cointegracion.grande (character): Mensaje indicando si hay o no cointegracion 
  K <- dim(data)[2]
  # La primer etapa consite en encontrar cointegracion por pares
  Cointegracion.pares <- as.data.frame( t(combn(K, 2))) #las combinaciones por pares de variables
  Cointegracion.pares$Cointegran <- ''
  Rellena.cointegracion <- Rellena(dataframe=data, dataframe.pares=Cointegracion.pares,  crit=crit, lag.max=lag.max)
  Cointegracion.pares$Cointegran <- mapply(function(x){Rellena.cointegracion(x)}, 1:dim(Cointegracion.pares)[1])
  if( !('No' %in% Cointegracion.pares$Cointegran ) )
  {
    cointegracion.grande <- 'Todas las variables cointegran'
    return(cointegracion.grande)
  } else{
    cointegracion.grande <- 'No todas las variables cointegran'
  }
}
  
