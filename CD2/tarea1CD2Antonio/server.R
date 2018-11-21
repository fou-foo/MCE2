library(shiny)
library(quanteda)
library(stringdist)

corpus <- readRDS('corpus.RDS')
limpieza <- function(archivo)
{
  library(quanteda)
  input <- tokens(archivo, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE,
                  remove_separators = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE,
                  remove_url = TRUE)
  input <- tolower(input)
  input <- paste0(input, collapse = " ")
  return(input)
}

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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$entrada <- output$entrada <- renderPrint({ input$input })
  
  output$predict <- renderPrint({
    archivo <- limpieza(as.character(input$input))
    x <- unlist(strsplit(archivo, ' '))
    archivo.corregido <- Corrector.multi(x)
    names(archivo.corregido) <- NULL
    paste0(archivo.corregido, collapse = ' ')
  })
})
  
