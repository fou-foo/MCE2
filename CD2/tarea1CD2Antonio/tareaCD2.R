setwd("C:/Users/fou-f/Desktop/Third/CD2/Tarea1")
library(stringdist)
## Para estimar P(s), el modelo del lenguaje usaremos la frecuencia normalizada 
# dada por el corpus, con fines de velocidad en la implementación esta busqueda se reduce
# las palabras que comienzan con el mismo caracter 
## P(w|s) es 1/(1+d_i) con d_i la distancia de s a w 
corpus <- read.table('freq_es.txt', sep = " ", header = FALSE, 
                     encoding = 'UTF-8', stringsAsFactors = FALSE)
names(corpus) <- c('palabra', 'Ps')
saveRDS(corpus, file='corpus.RDS')
#################
limpieza <- function(archivo)
{
  # preprocesamiento de archivo: quitamos numeros, puntuaciones, simbolos frecuentes en twitter,
  # concadenaciones del ingles del tipo 'self-study' y url's
  # convertimos a mayusculas 
  library(quanteda)
  input <- tokens(archivo, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE,
                  remove_separators = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE,
                  remove_url = TRUE)
  input <- tolower(input)
  input <- paste0(input, collapse = " ")
  return(input)
}
#string <- 'ecuacion'
corrector.init <- function(string, corpus)
{
  # inicializador del corrector
  corpus <- corpus
  # normalizamos frecuencias
  corpus$Ps <- corpus$Ps/sum(corpus$Ps)
  primer.letra <- substr(corpus$palabra, 1, 1)
  function(string)
  {
    #obtenemos las distancias de la entrada a todas las palabras del corpus
    distancias <- stringdist(string, corpus$palabra, method = 'osa') 
    # candidatos a s
    indices <- which(distancias <= 2)
    #indices <- distancias #SUPUESTA mejora del tercer INCISO
    posibilidades.s <- corpus[indices, ]
    # candidatos a w|s -> comenzando con la primer letra
    primera <- substr(string, 1, 1)
    index <- which(primer.letra == primera)
    posibilidades.w <- corpus[index, ] 
    posibilidades.w$Pw <- (1/(1+distancias[index])) #ponderacion 
    #normalizamos 
    posibilidades.w$Pw <- posibilidades.w$Pw/sum(posibilidades.w$Pw)
    # Estimamos P(s)*P(w|s), con las frecuencias de las palabras y normalizando
    posibilidades <- merge(posibilidades.s, posibilidades.w, x.all=TRUE)
    posibilidades$Pw[is.na(posibilidades$Pw)] <- 0 #por si hay candidatos a w que no sean candidatos a s
    posibilidades$P <- posibilidades$Ps*posibilidades$Pw
    w <- posibilidades[which.max(posibilidades$P), ]
    remove(posibilidades, posibilidades.s, posibilidades.w)
    gc()
    return(as.character(w$palabra))
  }
}
Corrector <- corrector.init(corpus=corpus)
Corrector.multi <- function(x) mapply(FUN= Corrector, x )
archivo <- readLines(con <- file('C:\\Users\\fou-f\\Desktop\\Third\\CD2\\Tarea1\\SFU_Spanish_Review_Corpus\\coches\\no_1_3.txt', encoding = 'latin1'))
archivo <- paste0(archivo, collapse = ' ')
archivo <- limpieza(archivo)
x <- unlist(strsplit(archivo, ' '))
archivo.corregido <- Corrector.multi(x)
archivo.corregido <- unlist(archivo.corregido)
archivo.corregido <- paste0(archivo.corregido, collapse = ' ')
archivo.corregido
####################################################
# Midiendo el desempeño segun el paper de Whitelaw et al.
###################################################
# Primero redefinimos el corpus 
# extraemos el 10% de las palabras y les generamos faltas ortograficas 
set.seed(0)
corpus.extension <- corpus[ sample(1:dim(corpus)[1],100), ] 
corpus.extension$Ps <- floor(mean(corpus$Ps))
ruido <- function(string)
{
  # funcion para generar palabras mal escritas
  numero.ediciones <- sample(1:2, 1)
  ediciones <- sample(c('d', 'i', 'sus', 'swap'), numero.ediciones)
  for (i in ediciones)
  {
    if(i == 'd')
    {
      n <- sample(1:(nchar(string)), 1)
      x <- substr(string, 1, n-1 )
      y <- substr(string, n+1, nchar(string))
      string <- paste0(x,y, collapse = '')
    }
    if(i =='sus')
    {
      n <- sample(1:(nchar(string)), 1)
      c <- sample(letters, 1)
      x <- substr(string, 1,n-1)
      y <- substr(string, n+1, nchar(string))
      string <- paste0(x, c, y, collapse = '')
    }
    if(i =='i')
    {
      n <- sample(1:(nchar(string)), 1)
      c <- sample(letters, 1)
      x <- substr(string, 1,n-1)
      y <- substr(string, n, nchar(string))
      string <- paste0(x, c, y, collapse = '')
    }
    if( i=='swap')
    {
      n <- sample(1:(nchar(string)), 1)
      x <- substr(string, 1,n-1)
      y <- substr(string, n+2, nchar(string))
      x1 <- substr(string, n-1, n-1)
      x2 <- substr(string, n, n)
      string <- paste0(x, x2, x1, y, collapse = '')
    }
  }
  return(string)
}
Ruido <- function(x) mapply(FUN=ruido, x)
corpus.prueba <- data.frame(palabra = Ruido(as.character(corpus.extension$palabra)))
corpus.prueba$Ps <- corpus.extension$Ps
test <- rbind(corpus.extension, corpus.prueba)
######## Lo probamos
corpus.final <- rbind(corpus, corpus.extension)
Corrector <- corrector.init(corpus=corpus.final)
Corrector.multi <- function(x) mapply(FUN= Corrector, x )
archivo <- limpieza(as.character(test$palabra))
x <- unlist(strsplit(archivo, ' '))
archivo.corregido <- Corrector.multi(x)
archivo.corregido <- unlist(archivo.corregido)
test$salida <- c(archivo.corregido[1:195], '*', archivo.corregido[196:199])
row.names(test) <- NULL
test$calificacion <- FALSE
calificador <- cbind(test[1:100,], test[101:200,])
names(calificador) <- c("palabra", "freq", "salida", "calificacion",
                        "palabraAlterada", "freqAlterada", "salidaAlterada",
                        "calificacionAlterada")
calificador$calificacion <- (calificador$palabra==calificador$salida)
calificador$calificacionAlterada <- (calificador$salidaAlterada==calificador$palabra) 
library(xtable)
sum(calificador$calificacion)
sum(calificador$calificacionAlterada)
calificador$freq <- NULL
calificador$freqAlterada <- NULL
row.names(calificador)<-NULL
xtable(calificador)
#####################################
# Cambiando corpus
corpus <- read.table('CREA_total.TXT', sep = "\t", header = TRUE, 
                     encoding = 'latin1', stringsAsFactors = FALSE)
names(corpus) 
corpus$Frec.normalizada<- NULL
names(corpus) <- c('palabra', 'Ps')
row.names(corpus) <- NULL
#################
set.seed(0)
corpus.extension <- corpus[ sample(1:dim(corpus)[1],25), ] 
corpus.extension$Ps <- floor(mean(corpus$Ps, na.rm=TRUE))
corpus.prueba <- data.frame(palabra = Ruido(as.character(corpus.extension$palabra)))
corpus.prueba$Ps <- corpus.extension$Ps
test <- rbind(corpus.extension, corpus.prueba)
############### Lo probamos
corpus.final <- rbind(corpus, corpus.extension)
Corrector <- corrector.init(corpus=corpus.final)
Corrector.multi <- function(x) mapply(FUN= Corrector, x )
archivo <- limpieza(as.character(test$palabra))
x <- unlist(strsplit(archivo, ' '))
archivo.corregido <- Corrector.multi(x)
archivo.corregido2 <- names((archivo.corregido))
test$salida <- '...'
test$salida <- archivo.corregido2[1:50]
row.names(test) <- NULL
test$calificacion <- FALSE
calificador <- cbind(test[1:25,], test[26:50,])
names(calificador) <- c("palabra", "freq", "salida", "calificacion",
                        "palabraAlterada", "freqAlterada", "salidaAlterada",
                        "calificacionAlterada")
calificador$calificacion <- (calificador$palabra==calificador$salida)
calificador$calificacionAlterada <- (calificador$salidaAlterada==calificador$palabra) 
library(xtable)
sum(calificador$calificacion)
sum(calificador$calificacionAlterada)
calificador$freq <- NULL
calificador$freqAlterada <- NULL
row.names(calificador)<-NULL
xtable(calificador)
archivo <- readLines(con <- file('C:\\Users\\fou-f\\Desktop\\Third\\CD2\\Tarea1\\SFU_Spanish_Review_Corpus\\coches\\no_1_3.txt', encoding = 'latin1'))
archivo <- paste0(archivo, collapse = ' ')
archivo <- limpieza(archivo)
x <- unlist(strsplit(archivo, ' '))
archivo.corregido <- Corrector.multi(x)
archivo.corregido2 <- unlist(archivo.corregido)
archivo.corregido2 <- paste0(archivo.corregido2, collapse = ' ')
archivo.corregido2
saveRDS(corpus.final, file='corpus.RDS')
