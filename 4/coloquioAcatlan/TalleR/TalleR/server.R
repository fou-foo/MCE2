####################################
#####    J. Antonio Garcia #########
#####   jose.ramirez@cimat.mx ######
####################################
library(plotly)
library(MASS)
library(shiny)
library(knitr)
library(rmarkdown)
#########################################
# Construccion del backend              #
#########################################
#lectura de datos simulados previamente
n <- 20
load('datos.RData') #discutiremos este binario en clase
####################################
server <- function(input, output) # notemos que 'server' tiene como entradas 'input', 'output'
{
  #seleccion de datos simulados
  a <- reactive({
      # La funcionalidad de este 'Closure' es recrear datos que despues usaremos para graficar
      # esta 'Closure' se activa en automatico cuando el usuario mueve el valor de 'input$d'
    d <- input$d # seleccion de dimension a graficas
    I <- diag(rep(1, d)) #creamos una matriz identidad de dimension 'd'
    pos <- pos[, 1:d] #los objetos 'pos' y 'neg' viven en el arvhico 'datos.Rdata'
    neg <- neg[, 1:d] #tomamos submuestras de los datos generados
    stack <- rbind(pos,neg)# las concadenamos para tener un objeto que acepte ggplot
    pos.mean <- apply(pos, 2, mean)
    neg.mean <- apply(neg, 2, mean)
    w <- ginv(I) %*% (pos.mean - neg.mean) # calculos necesarios para definir la linea verde
    w <- w/sum(w**2)**.5 #normalizamos el vector MDP
    X <- ginv(cov(pos)) %*% (pos.mean - neg.mean)
    X <- X/sum(X**2)**.5 #normalizamos el vector que define al frontera de Bayes
    stack$label <- 1
    stack$label[(n+1):(2*n)] <- -1
    Y <- X
    Y[-c(1,2)] <- 0
    Y[1] <- X[2]
    Y[2] <- -X[1] #elegimos un vector perpenticular a X
    Y <- Y/sum(Y**2)**.5
    sum(Y*w) #mas calculos
    M <- cbind(X, Y,  w)
    pos.proyec <- as.matrix(pos)%*%M
    neg.proyec <- as.matrix(neg)%*%M
    pos.proyec <- as.data.frame(pos.proyec)
    neg.proyec <- as.data.frame(neg.proyec)
    pos.proyec$label <- '+1'
    neg.proyec$label <- '-1'
    proyec <- rbind(pos.proyec, neg.proyec)
    proyec$w <- proyec$V2 * (w[1]/w[2])
    b <- list(proyec, w )
    return(b)
  })

  #construccion de scaterplot del tab DWD
  output$puntos <- renderPlotly({
    datos <- a() # aqui se hace invocacion de la 'closure' anterior para hacer un muestreo de los datos simulados
    proyec <- datos[[1]]
    w <- datos[[2]]
    fit <- lm(w ~ V2, data = proyec) #una regresion lineal
    pal <- c("purple", "orange") #definimos colores
    #construimos la grafica de scatterplot
    p1 <- ggplot(data = proyec, aes(x=V2, y=V3, color=label)) +
        geom_point() + stat_function(fun = function(z){z*(w[1]/w[2])}, aes(colour = I('MDP')), size=1.5) +
      geom_hline(yintercept=0, aes(colour=I('red')),     show.legend = NA)+
      ggtitle('Proyeccion en la direccion optima de Bayes y MaxDataPiling') +
      theme_minimal()+  xlab('Bayes') + ylab('') +
      scale_colour_manual(
        labels = c('-1', '+1', 'MDP'),
        values = c("purple", "orange", "green4") )+ theme(legend.title = element_blank())
    p2 <- ggplotly(p1) # construimos el grafico ggplot a uno plotly
    p2
  })



  #construccion de densidades del tab DWD
  output$Bayes <- renderPlotly({
    a <- a() #otravez seleccionamos datos
    proyec <- a[[1]]
    # y construimos el grafico
    p2 <- ggplot(data = proyec, aes(x=V3, fill=label, colour=label))+geom_density()+
      geom_rug(sides="b")+ggtitle('Distro en la direccion de Bayes') + theme_minimal()+
      xlab('Bayes') + ylab('') +
      scale_fill_manual(  labels = c('-1', '+1'), values = c("purple", "orange"))+
      scale_color_manual(  labels = c('-1', '+1'), values = c("purple", "orange"))+
      theme(legend.title = element_blank())
    p2 <- ggplotly(p2)
    p2
  })

  #construccion de los boxplots
  output$DWD <- renderPlotly({
    a <- a()
    proyec <- a[[1]]
    x <- subset(proyec, label == '+1')
    y <- subset(proyec, label == '-1')
    y$V3 <- abs(y$V3) #distancia al plano separador
    p <- plot_ly(y = ~x$V3, type = "box",
                 line = list(color = 'rgb(255,165,0)'),
                 name = "+1") %>%
      add_trace(y = ~y$V3,
                line = list(color = 'rgb(160,32,240)'),
                name = "-1"
                ) %>%
      layout(title = "Distancia a hiperplano MDP",
             xaxis = list(title = ' '),
                          yaxis = list(title = ''))

  })
}
