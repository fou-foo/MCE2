path <- "C:/Users/fou-f/Desktop/Third/CD2/Tarea2/POSData/"
dir(path)
#pre procesamiento al archivo
row <- readLines(paste0(path, 'training.pos'))
str(row)
length(row)
y <- lapply( FUN = function(x)  {
    a <- unlist(strsplit(row[x], split='\t'))[1]
    return(a)
  },X= 1:length(row))
y <- unlist(y)
z <- lapply( FUN = function(x){
  a <- unlist(strsplit(row[x], split='\t'))[2]
  return(a)
},X= 1:length(row))
z <- unlist(z)
train <- data.frame(palabra=as.character(y), tag=as.character(z))
names(train) <- c('Palabra', 'Tag')
train$Palabra <- as.character(train$Palabra)
train$Tag <- as.character(train$Tag)
unique(train$Tag) # todos los posibles tags
texto <- paste0(train$Tag, collapse = ' ')
library(quanteda)
#obtenemos las frecuencias de los bigramas y ordenamos lexicografimente
grams.2 <- tokens(texto) %>%
  tokens_ngrams(n=2)
grams.2 <- grams.2$text1
apriori <- table(grams.2)
apriori <- as.data.frame(apriori)
apriori <- apriori[ order(apriori$grams.2, decreasing = TRUE), ]
apriori$grams.2 <- as.character(apriori$grams.2)
apriori$from <- mapply(apriori$grams.2, FUN=
                         function(x) strsplit(x, split='_')[[1]][1] )
apriori$to <- mapply(apriori$grams.2, FUN=
                         function(x) strsplit(x, '_')[[1]][2] )
Estados <- unique(train$Tag)
(Estados <- sort(Estados))
n <- length(Estados)
Markov <- matrix(rep(0, n**2), ncol = n )
colnames(Markov) <- row.names(Markov) <- Estados
row.names(apriori) <- apriori$grams.2
# obtenemos las frecuencias de los tags singletones
palabras <- as.data.frame(table(train$Tag))
colnames(palabras) <- c('Tag', 'Freq')
palabras <- palabras[ order(palabras$Tag), ]
row.names(palabras) <- palabras$Tag
for (i in Estados)
{
  for(j in  Estados)
  {
    par.tj.ti <- apriori[paste0(i, '_' ,j), 'Freq'] 
    salida.i <-  palabras[as.character(i), 'Freq']
    Markov[as.character(i), as.character(j)] <- par.tj.ti/salida.i
  }
}
index <- is.na(Markov)
sum(index)
Markov[index] <- 0
#verificamos
a <- apply(Markov, 1, sum)
a
image(Markov)
########### vamos por las verosimilitudes
library(dplyr) #agrupamos para contar los pares (word,tag)
vero <- train
vero$pares <- paste0(vero$Palabra, '_', vero$Tag)
vero <- vero %>% group_by(Palabra, Tag, pares) %>% summarise(Freq=n())
vero <- vero[order(vero$Tag, decreasing = FALSE),]
index <- grep("^$", vero$Palabra)
vero <- vero[-index, ]
p <- length(unique(vero$Palabra))
Verosimil <- matrix(rep(0, n*p), nrow = n, ncol = p)
row.names(Verosimil) <- Estados
colnames(Verosimil) <- sort(unique(vero$Palabra))
dim(Verosimil)
row.names(vero) <- vero$pares
for(tag in row.names(Verosimil))
{
  for (word in colnames(Verosimil))
  {
    palabra.tag <- as.numeric(vero[paste0( word, '_', tag), 'Freq'])
    no.tag <- palabras[as.character(tag), 'Freq']
    Verosimil[as.character(tag), as.character(word)] <- palabra.tag/ no.tag
  }
}
index <- is.na(Verosimil)
sum(index)
Verosimil[index] <- 0
#verificacion
a <- apply(Verosimil, 1, sum)
#names(a) <- NULL
a
image(Verosimil)
####################################################
#Tokenizacion 
# ejercicio b)
###################################################
texto.prueba <- "Your contribution to Goodwill will mean more than you may know ." 
texto <- strsplit(texto.prueba, split=' ')
texto <- unlist(texto)
index <- match(texto, colnames(Verosimil))
out <- Verosimil[,index ]
out <- as.data.frame(out)
salida <- list()
for (i in colnames(out))
{
  index <- which(out[,as.character(i)] >0 ) 
  elejidos <-  row.names(out)[index]
  salida <- c(salida, list(palabra.tag=c(i,elejidos )))
}
salida
###############################
# ejercicio d)
################################
library(HMM)
dim(Verosimil)
dim(Markov)
row.names(Verosimil)
row.names(Markov)
set.seed(0)
p <- rep(1/n,n)
sum(p)

hmm <- initHMM(States = row.names(Verosimil), 
               Symbols = colnames(Verosimil),
               startProbs = p,
               transProbs = Markov, 
               emissionProbs = (Verosimil))
texto <- c("Your contribution to Goodwill will mean more than you may know",".")
palabras <- unlist(strsplit(tolower(texto)," "))
salida <- data.frame(palabras=palabras, Tag=viterbi(hmm, palabras))
library(xtable)
xtable(salida)
##########ejercicio e
#
###################
texto <- 'Coming to Goodwill was the first step toward my becoming totally .'
palabras <- unlist(strsplit(tolower(texto)," "))
palabras %in% unique(train$Palabra)
#checar palabra poco comun
a <- train %>% select(Palabra)%>% group_by(Palabra) %>% summarise(n = n()) 
a <- a[order(a$n), ]
palabras[11] <- 'total'
salida <- data.frame(palabras=palabras, Tag=viterbi(hmm, palabras))
salida
library(xtable)
xtable(salida)
#
library(coreNLP)
initCoreNLP( "C:\\Users\\fou-f\\Desktop\\stanford-corenlp-full-2018-02-27",
             parameterFile = 'C:\\Users\\fou-f\\Desktop\\Third\\CD2\\Tarea2\\en.properties')
############suavizamiento 
#
#######################
texto <- 'Coming to Goodwill was the first step toward my becoming totally .'
palabras <- unlist(strsplit(tolower(texto)," "))
palabras %in% unique(train$Palabra)
#checar palabra poco comun
palabras[11] <- 'XX'
Markov2 <- rbind(Markov, rep(1/10**6, dim(Markov)[1] ))
Markov2 <- cbind(Markov2, rep(1/10**6, dim(Markov)[1]+1 ))
Verosimil2 <- rbind( Verosimil ,
                     rep(1/(10**6),
                                     dim(Verosimil)[2] ))
Verosimil2 <- cbind(Verosimil2, 
                    rep(1/(10**6), dim(Verosimil)[1]+1 ))
dim(Verosimil2)
row.names(Verosimil2) <- c(row.names(Verosimil2)[-53], 'XX')
colnames(Verosimil2) <- c(colnames(Verosimil2)[-9752], 'XX')
o <- 1/length(row.names(Verosimil2))
hmm <- initHMM(States = row.names(Verosimil2), 
               Symbols = colnames(Verosimil2),
               startProbs = o,
               transProbs = Markov2, 
               emissionProbs = (Verosimil2))

salida <- data.frame(palabras=palabras, Tag=viterbi(hmm, palabras))
library(xtable)
xtable(salida)
