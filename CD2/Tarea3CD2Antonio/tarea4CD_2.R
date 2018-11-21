######################################
# ejercicio2 a
######################################
# librerias
library(wordVectors)
library(tsne)
library(tm)
library(quanteda)
library(dendextend)
#######################################
# funciones
M.td.spanol <- function(direccion)
{
    #Obtencion de los conteos de palabras
    # direccion (string): con la ruta donde se encuentran los archivos
    corpus <- Corpus(DirSource(direccion, recursive=TRUE,
                               encoding = "UTF-8"),
                     readerControl=list(language="es"))
    #preprocesamiento
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus,removeNumbers)
    corpus <- tm_map(corpus,removePunctuation)
    #corpus <- tm_map(corpus, content_transformer(tolower))
    corp <- corpus(corpus)
    tdm <- dfm(corp, tolower = TRUE, remove = stopwords('es'))
    tdm <- dfm_tfidf(tdm)
    return(tdm)
}
#######################################
path <- '~\\Desktop\\Third\\CD2\\Tarea3CD2Antonio\\'
setwd(path)
dir()
# # creacion del mini embedding
# prep_word2vec(origin="spanish_billion_words_subset/",
#               destination="billions_sub.txt",
#               lowercase=T,force=T)
# if (!file.exists("billions_sub.txt"))
# {
#     model = train_word2vec("billions.txt","billions_vectors.bin",
#                            vectors=200,
#                            threads=7,
#                            window=12,
#                            iter=3,
#                            negative_samples=5)
# }
# model = train_word2vec("billions_sub.txt","billions_vectors.bin",
#                        vectors=200,
#                        threads=5,
#                        window=12,
#                        iter=3,
#                        negative_samples=5,force = T)
#############aqui empieza la accion
model <- read.vectors("billions_sub.bin")
dim(model)
set.seed(0)
#plot(model,perplexity=200) #visualizacion general
######
# eleccion de palabras
####
direccion <-"spanish_billion_words_subset\\"
#td <- M.td.spanol(direccion)
#saveRDS(td, file = "td_ejercicio2.rds")
td <- readRDS(file = "td_ejercicio2.rds")
apply(td, 1, summary)
dim(td) #478816 palabras
gc()
#limite 100,000
busqueda <- round(dim(td)[2]/100) #tamaño del sampleo .1porciento
set.seed(0)
t1 <- Sys.time()
documento1  <- colnames(td[1,as.numeric(td[1,])>20])
documento2 <- colnames(td[2,as.numeric(td[2,])>20])
documento3 <- colnames(td[3,as.numeric(td[3,])>20])
ingredients <- unique(colnames(td))[sample(1:dim(td)[2], busqueda)]
term_set <- lapply(ingredients,
                   function(ingredient)
                   {
                       nearest_words = model %>% closest_to(model[[ingredient]],10)
                       nearest_words$word
                   }) %>% unlist
subset <- model[[term_set,average=F]]
subset %>%
    cosineDist(subset) %>%
    as.dist %>%
    hclust -> arbol
t1 <- t1 - Sys.time()
colores <- c('#2894C0', rgb(71/255,230/255,127/255), 'purple')
dend <- as.dendrogram(arbol)
dend %>% color_labels ->dend
#plot(dend)
#labels_colors(dend) <- colorCodes[(term_set)][order.dendrogram(dend)]
dend <- color_branches(dend,k=3, col=colores)
plot(dend)
par(cex = 1)
dend %>% sort(type='nodes') %>% plot(main = "3-Clusters de una m.a. de términos(1%)")
clusters <- cutree(dend, k=3)
doc1.muestral <- names(clusters[clusters==1])
doc2.muestral <- names(clusters[clusters==2])
doc3.muestral <- names(clusters[clusters==3])
documento1 <- colnames(td[1,as.numeric(td[1,])>0])
documento2 <- colnames(td[2,as.numeric(td[2,])>0])
documento3 <- colnames(td[3,as.numeric(td[3,])>0])
set.seed(0)
index <- sample(1:length(doc1.muestral),length(doc1.muestral)/100 )
plot(model[[doc1.muestral[index],average=F]], perplexity=250)
sum(doc1.muestral %in% documento1)/sum(as.numeric(td[1,])>0)
sum(doc1.muestral %in% documento2)/sum(as.numeric(td[2,])>0)
sum(doc1.muestral %in% documento3)/sum(as.numeric(td[3,])>0)
###
set.seed(0)
index <- sample(1:length(doc2.muestral),length(doc2.muestral)/100 )
plot(model[[doc2.muestral[index],average=F]], perplexity=100)
sum(doc2.muestral %in% documento1)/sum(as.numeric(td[1,])>0)
sum(doc2.muestral %in% documento2)/sum(as.numeric(td[2,])>0)
sum(doc2.muestral %in%documento3)/sum(as.numeric(td[3,])>0)
#
set.seed(0)
index <- sample(1:length(doc3.muestral),length(doc3.muestral)/10 )
plot(model[[doc3.muestral[index],average=F]],perplexity=200)
sum(doc3.muestral %in% documento1)/sum(as.numeric(td[1,])>0)
sum(doc3.muestral %in% documento2)/sum(as.numeric(td[2,])>0)
sum(doc3.muestral %in% documento3)/sum(as.numeric(td[3,])>0)
par(cex=1)
set.seed(0)
index <- sample(1:length(documento1),length(documento1)/100 )
plot(model[[documento1[index], average=F]], perplexity=150)
set.seed(0)
index <- sample(1:length(documento2),length(documento2)/100 )
plot(model[[documento2[index], average=F]], perplexity=150)
set.seed(0)
index <- sample(1:length(documento3),length(documento3)/100 )
plot(model[[documento3[index], average=F]], perplexity=100)

#inciso b clusters

palabras.extranas <- as.data.frame(apply(td, 2, sum))
palabras.extranas$palabra <- row.names(palabras.extranas)
colnames(palabras.extranas) <- c( 'Rank', 'Palabra')
palabras.extranas <- palabras.extranas[ order(palabras.extranas$Rank),]
hist(palabras.extranas$Rank, xlim = c(0,100))
summary(palabras.extranas$Rank)
palabras <- head(palabras.extranas, 30)$Palabra
################
set.seed(0)
clustering <- kmeans(model, centers=model[[palabras,average=F]], iter.max = 100)
table(clustering$cluster)
doc1 <- names(clustering$cluster[clustering$cluster==5])
doc2 <- names(clustering$cluster[clustering$cluster==13])
doc3 <- names(clustering$cluster[clustering$cluster==21])
sum(doc1%in%documento1)/length(documento1)
sum(doc1%in%documento2)/length(documento2)
sum(doc1%in%documento3)/length(documento3)
sum(doc2%in%documento1)/length(documento1)
sum(doc2%in%documento2)/length(documento2)
sum(doc2%in%documento3)/length(documento3)
sum(doc3%in%documento1)/length(documento1)
sum(doc3%in%documento2)/length(documento2)
sum(doc3%in%documento3)/length(documento3)

