### Ejercicio 3
#
#################
# construccion de un embedding para traducir
################################
library(wordVectors)
library(tm)
library(tsne)
library(tm)
library(quanteda)
library(dplyr)
##########################
# funciones
M.td.spanol <- function(direccion)
{
    #Obtencion de los conteos de palabras
    # direccion (string): con la ruta donde se encuentran los archivos
    corpus <- Corpus(DirSource(direccion, recursive=TRUE,
                               encoding = "ISO-8859-1"),
                     readerControl=list(language="es"))
    #preprocesamiento
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus,removeNumbers)
    corpus <- tm_map(corpus,removePunctuation)
    #corpus <- tm_map(corpus, content_transformer(tolower))
    corp <- corpus(corpus)
    tdm <- dfm(corp, tolower = TRUE, remove = NULL)
    return(tdm)
}
M.td.ingles <- function(direccion)
{
    #Obtencion de los conteos de palabras
    # direccion (string): con la ruta donde se encuentran los archivos
    # la forma facil es con esta implementacion 1
    # la otra es hacerlo a mano como lo hice en la tarea2
    corpus <- Corpus(DirSource(direccion, recursive=TRUE,
                               encoding = "UTF-8"),
                     readerControl=list(language="en"))
    #preprocesamiento
    corpus <- tm_map(corpus,removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus,removeNumbers)
    #corpus <- tm_map(corpus, content_transformer(tolower))
    corp <- corpus(corpus)
    tdm <- dfm(corp, tolower = TRUE, remove = NULL)
    return(tdm)
}
gc()
###########################
#Hace un par de dias hice los binarios
# system.time( {if (!file.exists("europarl-v7.es-en.en.bin")) {model.en.en = train_word2vec("ingles_ingles.bin",vectors=200,threads=7,window=12,iter=3,negative_samples=5)} else model.en.en = read.vectors("ingles_ingles.bin")})
# system.time( {if (!file.exists("europarl-v7.es-en.es.bin")) {model.en.es = train_word2vec("prep_europarl-v7.es-en.es","ingles_espanol.bin",vectors=200,threads=7,window=12,iter=3,negative_samples=5)} else model.en.es = read.vectors("ingles_espanol.bin")})

# Primero los embdddings de cada idioma
path <- '~\\Desktop\\Third\\CD2\\Tarea3CD2Antonio\\'
setwd(path)
# el salto de fe
#prep_word2vec(origin="europarl-v7.es-en.en",
#destination="prep_europarl-v7.es-en.en",lowercase=T,force=T)

#if (!file.exists("europeo.bin")) {model_en = train_word2vec("prep_europarl-v7.es-en.en","ingles_ingles.bin",
#                vectors=200, threads=7,window=14, iter=3, negative_samples=5)
#}
model.en.en <- read.vectors("ingles_ingles.bin")
dim(model.en.en)
# ahora al español
#prep_word2vec(origin="europarl-v7.es-en.es",destination="prep_europarl-v7.es-en.es",lowercase=T,force=T)

if (!file.exists("europarl-v7.es-en.es.bin"))
{
    model.en.es <- train_word2vec("prep_europarl-v7.es-en.es","ingles_espanol.bin",vectors=200,threads=7,window=12,iter=3,negative_samples=0)
}
model.en.es <- read.vectors("ingles_espanol.bin")
dim(model.en.es)

#t1 <- Sys.time()
## 2 Obten las 5k palabras mas usadas del corpus
#Espanol <- M.td.spanol('C:\\Users\\fou-f\\Desktop\\Third\\CD2\\Tarea3CD2Antonio\\es-en\\es-en\\europarl-v7.es-en.es\\')
#saveRDS(Espanol, file='Espanol.rds')
#Ingles <- M.td.ingles('C:\\Users\\fou-f\\Desktop\\Third\\CD2\\Tarea3CD2Antonio\\es-en\\es-en\\europarl-v7.es-en.en\\')
#saveRDS(Ingles, file='Ingles.rds')
#t1 <- Sys.time() - t1
#gc()
## Obtenemos las 5,000 palabras del español
corpus.esp <- readRDS('Espanol.rds')
corpus.esp  <- apply(corpus.esp, 2, function(x) if(x>0) x) #filtramos los ceros
corpus.esp <- as.data.frame(corpus.esp)
Espanol <- readRDS('Espanol.rds')
dim(Espanol)
Espanol  <- apply(Espanol, 2, function(x) if(x>0) x) #filtramos los ceros
Espanol <- as.data.frame(Espanol)
str(Espanol)
head(Espanol)
Espanol$Palabra.esp <- row.names(Espanol)
names(Espanol) <- c('Freq', 'Palabra.esp')
Espanol <- Espanol[order(Espanol$Freq, decreasing = TRUE), ]
head(Espanol)
#como parece haber casos raros los limpiamos
longitud <- nchar(Espanol$Palabra.esp)
table(longitud)
index <- which(longitud==1)
Espanol <- Espanol[-index,]
Espanol <- head(Espanol, 10000) #por si algo pasa
#model.en.es %>%
#closest_to(model.en.es[[Espanol$Palabra.esp, average=FALSE]],length(Espanol$Palabra.esp) ) -> a
#canal <- a$word
canal <- Espanol$Palabra.esp
####################
## Obtener la traduccion de las 5k palabras
Sys.setenv(GL_AUTH='C:\\Users\\fou-f\\Desktop\\Third\\CD2\\Tarea3CD2Antonio\\machinetranslation-219609-0f1c75d151ae.json') #fijamos la variable del sistema
library(googleLanguageR)
gl_auth("C:\\Users\\fou-f\\Desktop\\Third\\CD2\\Tarea3CD2Antonio\\machinetranslation-219609-0f1c75d151ae.json")
diccionario <- list()
for (i in 1:length(canal))
{
    Sys.sleep(.01)
    a <- gl_translate(canal[i], target='en', source='es')
    diccionario[[i]] <- a
    print(i)
}
#str(diccionario)
Imagen <- do.call(rbind.data.frame, diccionario)
names(Imagen) <- c('Ingles', 'Espanol')
write.csv(Imagen, file='Espanol.translate.csv')
a <- mapply(function(x)closest_to(model.en.en, x,1 )$word,
            Imagen$Ingles)
Imagen$parecido <- a
Imagen%>%filter(parecido == Ingles) -> Imagen #7620
#español
a <- mapply(function(x)closest_to(model.en.es, x,1 )$word,
            Imagen$Espanol)
Imagen$parecido.espanol <- a
Imagen%>%filter(parecido.espanol == Espanol) -> Imagen #6744
#repetidos
index <- match(unique(Imagen$Ingles), Imagen$Ingles )
Imagen <- Imagen[index, ]
index <- match(unique(Imagen$Espanol), Imagen$Espanol )
Imagen <- Imagen[index, ] #4599
X <- model.en.es[[Imagen$Espanol, average=FALSE]]
Z <- model.en.en[[Imagen$Ingles, average=FALSE]]
### ahora generaremos una matriz de $W$ traduccion
dim(X)
dim(Z)
W <- lm(Z~ X -1)
#summary(W)
W <- W$coefficients
##Ahora una prueba
set.seed(0)
W.inv <- solve(W)
Traductor.Esp.Ing.init <- function(palabra)
{
    palabra.1 <- closest_to(model.en.es, palabra, 1)$word
    vec.esp <- model.en.es[[palabra]]
    vect.ing <- vec.esp%*%W
    closest_to(model.en.en, vect.ing,1)$word
}
Traductor.Esp.Ing <- function(x)mapply(Traductor.Esp.Ing.init, x)
Traductor.Ing.Esp.init <- function(palabra)
{
    palabra.1 <- closest_to(model.en.en, palabra, 1)$word
    vec.ing <- model.en.en[[palabra]]
    vect.esp <- vec.ing%*%W.inv
    closest_to(model.en.es, vect.esp,1)$word
}
Traductor.Ing.Esp <- function(x)mapply(Traductor.Ing.Esp.init, x)
names(Imagen) <- c('Ingles', 'Espanol', 'DM.Ingles', 'DM.Espa')
Imagen$DM.Ingles <- Traductor.Ing.Esp(Imagen$Ingles)
Imagen$DM.Español <- Traductor.Esp.Ing(Imagen$Espanol)

sum(Imagen$Ingles==Imagen$DM.Ingles)
##############palabras del papaer#################

###########Error de Prueba####
# corpus del paper
Palabras <- c('emociones', 'protegida', 'imperio',
              'determinante', 'preparada', 'millas', 'hablamos',
              'destacaron', 'pets', 'mines', 'unacceptable',
              'prayers', 'shortstop', 'interaction', 'ultra',
              'beneficial', 'beds', 'connectivity', 'transform', 'motivation')
Definicion <- c('emotions','protected','empire','determinant',
                'prepared','miles','talk','highlighted',
                'mascotas','minas','inaceptable','rezo',
                'campocorto','interacción','muy','beneficioso',
                'camas','conectividad','transformar','motivación')
