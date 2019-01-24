####################################
#####    J. Antonio Garcia #########
#####   jose.ramirez@cimat.mx ######
####################################
library(plotly)
library(shiny)
library(ggplot2)
library(cluster)
library(smacof)
library(plotly)
library(corpcor)
library(randomForestSRC)
theme_set(theme_bw())
#########################################
# Construccion del backend              #
#########################################
####################################
server <- function(input, output) {
  # preparacion de datos para MDS
 Folder <- c(rep("M",35),rep("C",81),rep("E",51))
  # Aplicaci?n MDS -------------------------------------------------------------------
  # Esto solo es para sacar los datos
  # En analisis de sensibilidad mostro que las 5 variables numericas mas importantes son
  importantes.numericas <- c('Enfriamiento.i_TORElev4', 'Enfriamiento.i_HSRElev4',
                             'Enfriamiento.i_TORElev5', 'Enfriamiento.i_HSRElev5',
                             'dRuido2')
  importantes.categoricas <- c('Garantias.dCapExcitacion', 'Garantias.dCapEficReg',
                               'Garantias.dCapPerd', 'Enfriamiento.i_ElevTempP3',
                               'iDestino')
  administrativas <- c(1,2,3,8,9)
  categoricas <- c(4, 5, 6, 11, 12, 18, 19, 20, 21, 28, 29, 30, 31, 35, 55,
                   57, 63, 64, 65, 66, 67, 69, 70, 71, 72, 73, 74, 75, 76, 77,
                   88, 89, 90, 100, 101, 111, 112, 114, 117, 130, 132, 133, 134,
                   135, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 170)
  embarque <- 29:31 # se eliminan 32 33 34 por estar en el conjunto de 'numericas'
  numericas <- c(13, 15, 17, 22, 23, 24, 25,  27, 32, 33, 34, 36, 37, 38, 45,
                 46, 47, 48, 49, 50, 58, 59, 60, 68, 78, 79, 80, 81, 82, 83, 84, 85,
                 86, 87, 91, 93, 94, 97, 98, 102, 106, 107, 126, 127, 128, 129)
  datos <- read.csv("dataframeVerdesPresentes.csv", stringsAsFactors = FALSE) #
  # cast de numericas a factores 'character' para que las salidas de 'predict' sean caracteres y no niveles
  datos[, categoricas] <- lapply(datos[, categoricas], function(x) factor(x))
  datos[, embarque] <- lapply(datos[, embarque], function(x) factor(x))
  # construccion de transformacion base para la app
  datos <- datos[, -c(administrativas, 26)]    # eliminamos las variables 'administrativas' por no ser importantes para la prediccion

  
  
  ############################3
    #load('MODELOS.rdata')
    load('entrada.rdata')
    load('MODELOSRF.rdata')
    seleccion <- entrada
    seleccion$Enfriamiento.i_TORElev4 <- 65
    
  #seleccion de datos simulados
  output$Enfriamiento.i_TORElev4select <- renderPrint({ as.numeric(input$Enfriamiento.i_TORElev4) })
  output$Enfriamiento.i_HSRElev4select <- renderPrint({ as.numeric(input$Enfriamiento.i_HSRElev4) })
  output$Enfriamiento.i_TORElev5select <- renderPrint({ as.numeric(input$Enfriamiento.i_TORElev5) })
  output$Enfriamiento.i_HSRElev5select <- renderPrint({ as.numeric(input$Enfriamiento.i_HSRElev5) })
  output$dRuido2select <- renderPrint({ as.numeric(input$dRuido2) })
  
  # variables categoricas
  output$Garantias.dCapExcitacionselect <- renderPrint({ input$Garantias.dCapExcitacion })
  output$Garantias.dCapEficRegselect <- renderPrint({ input$Garantias.dCapEficReg })
  output$Garantias.dCapPerdselect <- renderPrint({ input$Garantias.dCapPerd })
  output$Enfriamiento.i_ElevTempP3select <- renderPrint({ input$Enfriamiento.i_ElevTempP3 })
  output$iDestinoselect <- renderPrint({ input$iDestino })
  
  
  ############################################
  #construccion de proyeccion 
  output$plot1<-renderPlotly({
    DataFrame <- datos[, c(importantes.categoricas, importantes.numericas)]
    Disimilaridad <- daisy(DataFrame,  metric = "gower")
    
    # Hacemos MDS cl?sico en 2 dimensiones
    MDSc <- cmdscale(Disimilaridad, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE)
    
    # Sacamos las cpsas para LMDS
    Delta <- as.matrix(Disimilaridad)^2
    dmu <- colMeans(Delta)
    Lkg <- pseudoinverse(MDSc)
    Distancias <- daisy(rbind(seleccion, pronostico(), DataFrame),  metric = "gower")
    #Distancias <- daisy(rbind(seleccion, entrada, DataFrame),  metric = "gower")
    
    Delta.test <- t(as.matrix(Distancias)[1:2,-c(1,2)])^2
    Nuevos <- t(sapply(i <- 1:2, function(i) { -0.5*Lkg%*%(Delta.test[,i]-dmu) }))
    
    gg <- data.frame(D1 = MDSc[,1],D2 = MDSc[,2], c = Folder) # Puntos base
    nn <- data.frame(D1 = Nuevos[,1], D2 = Nuevos[,2]) # Puntos a proyectar
    
    a <- ggplot( gg, aes(x = D1, y = D2)) +
      geom_text(label = gg$c, colour = "black") +  
      xlab("Dimension 1") +
      ylab("Dimension 2") +
      ggtitle("Escalamiento multidimensional", subtitle = "Rojo:Manual Verde:Recomendacion") +
      geom_point(data = nn[2,], aes(x = D1, y = D2),
                 colour = "Green", shape = 17, size = 5) + 
      geom_point(data = nn[1,], aes(x = D1, y = D2),
                 colour = "red", pch = 16, size = 4)
    ggplotly(a)
    })

  output$plot2<-renderPlotly({
    DataFrame <- datos[, c(importantes.categoricas, importantes.numericas)]
    Disimilaridad <- daisy(DataFrame,  metric = "gower")
    
    # Hacemos MDS cl?sico en 2 dimensiones
    MDSc <- cmdscale(Disimilaridad, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE)
    
    # Sacamos las cpsas para LMDS
    Delta <- as.matrix(Disimilaridad)^2
    dmu <- colMeans(Delta)
    Lkg <- pseudoinverse(MDSc)
    Distancias <- daisy(rbind(seleccion, pronostico(), DataFrame),  metric = "gower")
    #Distancias <- daisy(rbind(seleccion, entrada, DataFrame),  metric = "gower")
    
    Delta.test <- t(as.matrix(Distancias)[1:2,-c(1,2)])^2
    Nuevos <- t(sapply(i <- 1:2, function(i) { -0.5*Lkg%*%(Delta.test[,i]-dmu) }))
    
    gg <- data.frame(D1 = MDSc[,1],D2 = MDSc[,2], c = Folder) # Puntos base
    nn <- data.frame(D1 = Nuevos[,1], D2 = Nuevos[,2]) # Puntos a proyectar
    
    a <- ggplot( gg, aes(x = D1, y = D2)) +
      geom_text(label = gg$c, colour = "black") +  
      xlab("Dimension 1") +
      ylab("Dimension 2") +
      ggtitle("Escalamiento multidimensional", subtitle = "Rojo:Manual Verde:Recomendacion") +
      geom_point(data = nn[2,], aes(x = D1, y = D2),
                 colour = "green", shape = 17, size = 5) + 
      geom_point(data = nn[1,], aes(x = D1, y = D2),
                 colour = "red", pch = 16, size = 4)
    ggplotly(a)
  })
  
  #########################################
  pronostico <- reactive({
    #variables numericas 
      if( as.numeric(input$Enfriamiento.i_TORElev4) != -1 )
      {
        entrada$Enfriamiento.i_TORElev4 <<- as.numeric(input$Enfriamiento.i_TORElev4)
        seleccion$Enfriamiento.i_TORElev4 <<- as.numeric(input$Enfriamiento.i_TORElev4)
        
        #vAriables continuas
        entrada$Enfriamiento.i_HSRElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
        entrada$Enfriamiento.i_TORElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev5']],entrada )$predicted
        entrada$Enfriamiento.i_HSRElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev5']],entrada  )$predicted
        entrada$dRuido2 <<- predict(na.action='na.omit',MODELOS[['dRuido2']],entrada )$predicted
        
        # variables categoricas
        entrada$Garantias.dCapExcitacion <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapExcitacion']],entrada, na.action='na.omit')$class
        entrada$Garantias.dCapEficReg <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapEficReg']],entrada)$class
        entrada$Garantias.dCapPerd <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapPerd']],entrada)$class
        entrada$Enfriamiento.i_ElevTempP3 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
        entrada$iDestino <<- predict(na.action='na.omit',MODELOS[['iDestino']],entrada)$class
          return(entrada)
      }
      if( as.numeric(input$Enfriamiento.i_HSRElev4)!= -1)
      {
        entrada$Enfriamiento.i_HSRElev4 <<- as.numeric(input$Enfriamiento.i_HSRElev4)
        seleccion$Enfriamiento.i_HSRElev4 <<- as.numeric(input$Enfriamiento.i_HSRElev4)
        
        #variables continuas
        entrada$Enfriamiento.i_TORElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
        entrada$Enfriamiento.i_TORElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
        entrada$Enfriamiento.i_HSRElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
        entrada$dRuido2 <<- predict(na.action='na.omit',MODELOS[['dRuido2']],entrada)$predicted
        # 
        # variables categoricas
        entrada$Garantias.dCapExcitacion <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapExcitacion']],entrada)$class
        entrada$Garantias.dCapEficReg <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapEficReg']],entrada)$class
        entrada$Garantias.dCapPerd <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapPerd']],entrada)$class
        entrada$Enfriamiento.i_ElevTempP3 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
        entrada$iDestino <<- predict(na.action='na.omit',MODELOS[['iDestino']],entrada)$class
        return(entrada)
      }
      if( as.numeric(input$Enfriamiento.i_TORElev5) != -1)
      {
        entrada$Enfriamiento.i_TORElev5 <<- as.numeric(input$Enfriamiento.i_TORElev5)
        seleccion$Enfriamiento.i_TORElev5 <<- as.numeric(input$Enfriamiento.i_TORElev5)
        
        # variables continuas
        entrada$Enfriamiento.i_TORElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
        entrada$Enfriamiento.i_HSRElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
        entrada$Enfriamiento.i_TORElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
        entrada$Enfriamiento.i_HSRElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
        entrada$dRuido2 <<- predict(na.action='na.omit',MODELOS[['dRuido2']],entrada)$predicted
        # 
        # variables categoricas
        entrada$Garantias.dCapExcitacion <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapExcitacion']],entrada)$class
        entrada$Garantias.dCapEficReg <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapEficReg']],entrada)$class
        entrada$Garantias.dCapPerd <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapPerd']],entrada)$class
        entrada$Enfriamiento.i_ElevTempP3 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
        entrada$iDestino <<- predict(na.action='na.omit',MODELOS[['iDestino']],entrada)$class
        return(entrada)
      }
      if( as.numeric(input$Enfriamiento.i_HSRElev5)!= -1 )
      {
        entrada$Enfriamiento.i_HSRElev5 <<- as.numeric(input$Enfriamiento.i_HSRElev5)
        seleccion$Enfriamiento.i_HSRElev5 <<- as.numeric(input$Enfriamiento.i_HSRElev5)
        
        
        #variables numericas
        entrada$Enfriamiento.i_TORElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
        entrada$Enfriamiento.i_HSRElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
        entrada$Enfriamiento.i_TORElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
        entrada$dRuido2 <<- predict(na.action='na.omit',MODELOS[['dRuido2']],entrada)$predicted
        # 
        # variables categoricas
        entrada$Garantias.dCapExcitacion <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapExcitacion']],entrada)$class
        entrada$Garantias.dCapEficReg <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapEficReg']],entrada)$class
        entrada$Garantias.dCapPerd <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapPerd']],entrada)$class
        entrada$Enfriamiento.i_ElevTempP3 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
        entrada$iDestino <<- predict(na.action='na.omit',MODELOS[['iDestino']],entrada)$class
        return(entrada)
      }
      if( as.numeric(input$dRuido2) != 0 )
      {
        entrada$dRuido2 <<- as.numeric(input$dRuido2)
        seleccion$dRuido2 <<- as.numeric(input$dRuido2)
        
        #variables numericas
        entrada$Enfriamiento.i_TORElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
        entrada$Enfriamiento.i_HSRElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
        entrada$Enfriamiento.i_TORElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
        entrada$Enfriamiento.i_HSRElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
        # 
        
        # variables categoricas
        entrada$Garantias.dCapExcitacion <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapExcitacion']],entrada)$class
        entrada$Garantias.dCapEficReg <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapEficReg']],entrada)$class
        entrada$Garantias.dCapPerd <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapPerd']],entrada)$class
        entrada$Enfriamiento.i_ElevTempP3 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
        entrada$iDestino <<- predict(na.action='na.omit',MODELOS[['iDestino']],entrada)$class
        return(entrada)
      }
      ###########segundo tab     # variables categoricas 
    if( input$Garantias.dCapExcitacion != '0' )
    {
      entrada$Garantias.dCapExcitacion <<- factor(input$Garantias.dCapExcitacion, levels = levels(entrada$Garantias.dCapExcitacion))
      seleccion$Garantias.dCapExcitacion <<- factor(input$Garantias.dCapExcitacion, levels = levels(entrada$Garantias.dCapExcitacion))
      
      #variables numericas
      entrada$Enfriamiento.i_TORElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
      entrada$Enfriamiento.i_HSRElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
      entrada$Enfriamiento.i_TORElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
      entrada$Enfriamiento.i_HSRElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
      entrada$dRuido2 <<- predict(na.action='na.omit',MODELOS[['dRuido2']],entrada )$predicted
      
      # 
      
      #variables categoricas   
      entrada$Garantias.dCapEficReg <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapEficReg']],entrada)$class
      entrada$Garantias.dCapPerd <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapPerd']],entrada)$class
      entrada$Enfriamiento.i_ElevTempP3 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
      entrada$iDestino <<- predict(na.action='na.omit',MODELOS[['iDestino']],entrada)$class
      
      return(entrada)
    }
    if( input$Garantias.dCapEficReg != '0' )
    {
      entrada$Garantias.dCapEficReg <<- factor(input$Garantias.dCapEficReg, levels = levels(entrada$Garantias.dCapEficReg))
      seleccion$Garantias.dCapEficReg <<- factor(input$Garantias.dCapEficReg, levels = levels(entrada$Garantias.dCapEficReg))
      
      #variables numericas
      entrada$Enfriamiento.i_TORElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
      entrada$Enfriamiento.i_HSRElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
      entrada$Enfriamiento.i_TORElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
      entrada$Enfriamiento.i_HSRElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
      entrada$dRuido2 <<- predict(na.action='na.omit',MODELOS[['dRuido2']],entrada )$predicted
      
      # 
      
      #variables categoricas   
      
      entrada$Garantias.dCapExcitacion <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapExcitacion']],entrada)$class
      entrada$Garantias.dCapPerd <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapPerd']],entrada)$class
      entrada$Enfriamiento.i_ElevTempP3 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
      entrada$iDestino <<- predict(na.action='na.omit',MODELOS[['iDestino']],entrada)$class
      return(entrada)
    }
    if( input$Garantias.dCapPerd != '0' )
    {
      entrada$Garantias.dCapPerd <<- factor(input$Garantias.dCapPerd, levels = levels(entrada$Garantias.dCapPerd))
      seleccion$Garantias.dCapPerd <<- factor(input$Garantias.dCapPerd, levels = levels(entrada$Garantias.dCapPerd))
      
      #variables numericas
      entrada$Enfriamiento.i_TORElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
      entrada$Enfriamiento.i_HSRElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
      entrada$Enfriamiento.i_TORElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
      entrada$Enfriamiento.i_HSRElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
      entrada$dRuido2 <<- predict(na.action='na.omit',MODELOS[['dRuido2']],entrada )$predicted
      
      # 
      
      #variables categoricas   
      
      entrada$Garantias.dCapExcitacion <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapExcitacion']],entrada)$class
      entrada$Garantias.dCapEficReg <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapEficReg']],entrada)$class
      entrada$Enfriamiento.i_ElevTempP3 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
      entrada$iDestino <<- predict(na.action='na.omit',MODELOS[['iDestino']],entrada)$class
      return(entrada)
    }
    if( input$Enfriamiento.i_ElevTempP3 != '-2' )
    {
      entrada$Enfriamiento.i_ElevTempP3 <<- factor(input$Enfriamiento.i_ElevTempP3, levels = levels(entrada$Enfriamiento.i_ElevTempP3))
      seleccion$Enfriamiento.i_ElevTempP3 <<- factor(input$Enfriamiento.i_ElevTempP3, levels = levels(entrada$Enfriamiento.i_ElevTempP3))
      
      #variables numericas
      entrada$Enfriamiento.i_TORElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
      entrada$Enfriamiento.i_HSRElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
      entrada$Enfriamiento.i_TORElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
      entrada$Enfriamiento.i_HSRElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
      entrada$dRuido2 <<- predict(na.action='na.omit',MODELOS[['dRuido2']],entrada )$predicted
      
      # 
      
      #variables categoricas   
      
      entrada$Garantias.dCapExcitacion <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapExcitacion']],entrada)$class
      entrada$Garantias.dCapEficReg <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapEficReg']],entrada)$class
      entrada$Garantias.dCapPerd <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapPerd']],entrada)$class
      entrada$iDestino <<- predict(na.action='na.omit',MODELOS[['iDestino']],entrada)$class
      return(entrada)
    }
    if( input$iDestino != '-2' )
    {
      entrada$iDestino <<- factor(input$iDestino, levels = levels(entrada$iDestino))
      seleccion$iDestino <<- factor(input$iDestino, levels = levels(entrada$iDestino))
      
            #variables numericas
      entrada$Enfriamiento.i_TORElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
      entrada$Enfriamiento.i_HSRElev4 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
      entrada$Enfriamiento.i_TORElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
      entrada$Enfriamiento.i_HSRElev5 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
      entrada$dRuido2 <<- predict(na.action='na.omit',MODELOS[['dRuido2']],entrada )$predicted
      
      # 
      
      # variables categoricas
      entrada$Garantias.dCapExcitacion <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapExcitacion']],entrada)$class
      entrada$Garantias.dCapEficReg <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapEficReg']],entrada)$class
      entrada$Garantias.dCapPerd <<- predict(na.action='na.omit',MODELOS[['Garantias.dCapPerd']],entrada)$class
      entrada$Enfriamiento.i_ElevTempP3 <<- predict(na.action='na.omit',MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
      return(entrada)
    }
  
     

      })






#variables numericas
  output$Enfriamiento.i_TORElev4Auto1 <- renderPrint({ as.numeric(pronostico()[1, 'Enfriamiento.i_TORElev4']) })
  output$Enfriamiento.i_HSRElev4Auto1 <- renderPrint({as.numeric(pronostico()[1, 'Enfriamiento.i_HSRElev4'] )})
  output$Enfriamiento.i_TORElev5Auto1 <- renderPrint({ as.numeric(pronostico()[, 'Enfriamiento.i_TORElev5']) })
  output$Enfriamiento.i_HSRElev5Auto1 <- renderPrint({as.numeric(pronostico()[1, 'Enfriamiento.i_HSRElev5'] )})
  output$dRuido2Auto1  <- renderPrint({ as.numeric( pronostico()[, 'dRuido2']) })
  
  output$Enfriamiento.i_TORElev4Auto2 <- renderPrint({ as.numeric(pronostico()[1, 'Enfriamiento.i_TORElev4']) })
  output$Enfriamiento.i_HSRElev4Auto2 <- renderPrint({as.numeric(pronostico()[1, 'Enfriamiento.i_HSRElev4'] )})
  output$Enfriamiento.i_TORElev5Auto2 <- renderPrint({ as.numeric(pronostico()[, 'Enfriamiento.i_TORElev5']) })
  output$Enfriamiento.i_HSRElev5Auto2 <- renderPrint({as.numeric(pronostico()[1, 'Enfriamiento.i_HSRElev5'] )})
  output$dRuido2Auto2 <- renderPrint({ as.numeric( pronostico()[, 'dRuido2']) })
  
#variables categoricas 
  output$Garantias.dCapExcitacionAuto1 <- renderPrint({ as.character( pronostico()[, 'Garantias.dCapExcitacion']) })
  output$Garantias.dCapEficRegAuto1 <- renderPrint({ as.character( pronostico()[, 'Garantias.dCapEficReg']) })
  output$Garantias.dCapPerdAuto1 <- renderPrint({ as.character( pronostico()[, 'Garantias.dCapPerd']) })
  output$Enfriamiento.i_ElevTempP3Auto1 <- renderPrint({ as.character( pronostico()[, 'Enfriamiento.i_ElevTempP3']) })
  output$iDestinoAuto1 <- renderPrint({ as.character( pronostico()[, 'iDestino']) })

  output$Garantias.dCapExcitacionAuto2 <- renderPrint({ as.character( pronostico()[, 'Garantias.dCapExcitacion']) })
  output$Garantias.dCapEficRegAuto2 <- renderPrint({ as.character( pronostico()[, 'Garantias.dCapEficReg']) })
  output$Garantias.dCapPerdAuto2 <- renderPrint({ as.character( pronostico()[, 'Garantias.dCapPerd']) })
  output$Enfriamiento.i_ElevTempP3Auto2 <- renderPrint({ as.character( pronostico()[, 'Enfriamiento.i_ElevTempP3']) })
  output$iDestinoAuto2 <- renderPrint({ as.character( pronostico()[, 'iDestino']) })
  
 

  #############################################################################


}
