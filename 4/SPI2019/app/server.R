####################################
#####    J. Antonio Garcia #########
#####   jose.ramirez@cimat.mx ######
####################################
library(plotly)
library(shiny)
library(ggplot2)
theme_set(theme_bw())
#########################################
# Construccion del backend              #
#########################################

####################################
server <- function(input, output) {
    #load('MODELOS.rdata')
    load('entrada.rdata')
    load('MODELOSRFRedox.rdata')
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
  pronostico <- reactive({
    #variables numericas 
      if( as.numeric(input$Enfriamiento.i_TORElev4) != -1 )
      {
        entrada$Enfriamiento.i_TORElev4 <<- as.numeric(input$Enfriamiento.i_TORElev4)
        #vAriables continuas
        entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
        entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
        entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada)$predicted
        
        # variables categoricas
        entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada)$class
        entrada$Garantias.dCapEficReg <<- predict(MODELOS[['Garantias.dCapEficReg']],entrada)$class
        entrada$Garantias.dCapPerd <<- predict(MODELOS[['Garantias.dCapPerd']],entrada)$class
        entrada$Enfriamiento.i_ElevTempP3 <<- predict(MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
        entrada$iDestino <<- predict(MODELOS[['iDestino']],entrada)$class
          return(entrada)
      }
      if( as.numeric(input$Enfriamiento.i_HSRElev4)!= -1)
      {
        entrada$Enfriamiento.i_HSRElev4 <<- as.numeric(input$Enfriamiento.i_HSRElev4)
        #variables continuas
        entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
        entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
        entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada)$predicted
        # 
        # variables categoricas
        entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada)$class
        entrada$Garantias.dCapEficReg <<- predict(MODELOS[['Garantias.dCapEficReg']],entrada)$class
        entrada$Garantias.dCapPerd <<- predict(MODELOS[['Garantias.dCapPerd']],entrada)$class
        entrada$Enfriamiento.i_ElevTempP3 <<- predict(MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
        entrada$iDestino <<- predict(MODELOS[['iDestino']],entrada)$class
        return(entrada)
      }
      if( as.numeric(input$Enfriamiento.i_TORElev5) != -1)
      {
        entrada$Enfriamiento.i_TORElev5 <<- as.numeric(input$Enfriamiento.i_TORElev5)
        
        # variables continuas
        entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
        entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
        entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
        entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada)$predicted
        # 
        # variables categoricas
        entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada)$class
        entrada$Garantias.dCapEficReg <<- predict(MODELOS[['Garantias.dCapEficReg']],entrada)$class
        entrada$Garantias.dCapPerd <<- predict(MODELOS[['Garantias.dCapPerd']],entrada)$class
        entrada$Enfriamiento.i_ElevTempP3 <<- predict(MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
        entrada$iDestino <<- predict(MODELOS[['iDestino']],entrada)$class
        return(entrada)
      }
      if( as.numeric(input$Enfriamiento.i_HSRElev5)!= -1 )
      {
        entrada$Enfriamiento.i_HSRElev5 <<- as.numeric(input$Enfriamiento.i_HSRElev5)
        
        #variables numericas
        entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
        entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
        entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada)$predicted
        # 
        # variables categoricas
        entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada)$class
        entrada$Garantias.dCapEficReg <<- predict(MODELOS[['Garantias.dCapEficReg']],entrada)$class
        entrada$Garantias.dCapPerd <<- predict(MODELOS[['Garantias.dCapPerd']],entrada)$class
        entrada$Enfriamiento.i_ElevTempP3 <<- predict(MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
        entrada$iDestino <<- predict(MODELOS[['iDestino']],entrada)$class
        return(entrada)
      }
      if( as.numeric(input$dRuido2) != 0 )
      {
        entrada$dRuido2 <<- as.numeric(input$dRuido2)
        #variables numericas
        entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
        entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
        entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
        # 
        
        # variables categoricas
        entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada)$class
        entrada$Garantias.dCapEficReg <<- predict(MODELOS[['Garantias.dCapEficReg']],entrada)$class
        entrada$Garantias.dCapPerd <<- predict(MODELOS[['Garantias.dCapPerd']],entrada)$class
        entrada$Enfriamiento.i_ElevTempP3 <<- predict(MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
        entrada$iDestino <<- predict(MODELOS[['iDestino']],entrada)$class
        return(entrada)
      }
      ###########segundo tab     # variables categoricas 
    if( input$Garantias.dCapExcitacion != '0' )
    {
      entrada$Garantias.dCapExcitacion <<- factor(input$Garantias.dCapExcitacion, levels = levels(entrada$Garantias.dCapExcitacion))
      #variables numericas
      entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
      entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
      entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
      entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
      entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada )$predicted
      
      # 
      
      #variables categoricas   
      entrada$Garantias.dCapEficReg <<- predict(MODELOS[['Garantias.dCapEficReg']],entrada)$class
      entrada$Garantias.dCapPerd <<- predict(MODELOS[['Garantias.dCapPerd']],entrada)$class
      entrada$Enfriamiento.i_ElevTempP3 <<- predict(MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
      entrada$iDestino <<- predict(MODELOS[['iDestino']],entrada)$class
      
      return(entrada)
    }
    if( input$Garantias.dCapEficReg != '0' )
    {
      entrada$Garantias.dCapEficReg <<- factor(input$Garantias.dCapEficReg, levels = levels(entrada$Garantias.dCapEficReg))
      #variables numericas
      entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
      entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
      entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
      entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
      entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada )$predicted
      
      # 
      
      #variables categoricas   
      
      entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada)$class
      entrada$Garantias.dCapPerd <<- predict(MODELOS[['Garantias.dCapPerd']],entrada)$class
      entrada$Enfriamiento.i_ElevTempP3 <<- predict(MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
      entrada$iDestino <<- predict(MODELOS[['iDestino']],entrada)$class
      return(entrada)
    }
    if( input$Garantias.dCapPerd != '0' )
    {
      entrada$Garantias.dCapPerd <<- factor(input$Garantias.dCapPerd, levels = levels(entrada$Garantias.dCapPerd))
      #variables numericas
      entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
      entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
      entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
      entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
      entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada )$predicted
      
      # 
      
      #variables categoricas   
      
      entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada)$class
      entrada$Garantias.dCapEficReg <<- predict(MODELOS[['Garantias.dCapEficReg']],entrada)$class
      entrada$Enfriamiento.i_ElevTempP3 <<- predict(MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
      entrada$iDestino <<- predict(MODELOS[['iDestino']],entrada)$class
      return(entrada)
    }
    if( input$Enfriamiento.i_ElevTempP3 != '-2' )
    {
      entrada$Enfriamiento.i_ElevTempP3 <<- factor(input$Enfriamiento.i_ElevTempP3, levels = levels(entrada$Enfriamiento.i_ElevTempP3))
      #variables numericas
      entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
      entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
      entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
      entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
      entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada )$predicted
      
      # 
      
      #variables categoricas   
      
      entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada)$class
      entrada$Garantias.dCapEficReg <<- predict(MODELOS[['Garantias.dCapEficReg']],entrada)$class
      entrada$Garantias.dCapPerd <<- predict(MODELOS[['Garantias.dCapPerd']],entrada)$class
      entrada$iDestino <<- predict(MODELOS[['iDestino']],entrada)$class
      return(entrada)
    }
    if( input$iDestino != '-2' )
    {
      entrada$iDestino <<- factor(input$iDestino, levels = levels(entrada$iDestino))
      #variables numericas
      entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )$predicted
      entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)$predicted
      entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)$predicted
      entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )$predicted
      entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada )$predicted
      
      # 
      
      # variables categoricas
      entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada)$class
      entrada$Garantias.dCapEficReg <<- predict(MODELOS[['Garantias.dCapEficReg']],entrada)$class
      entrada$Garantias.dCapPerd <<- predict(MODELOS[['Garantias.dCapPerd']],entrada)$class
      entrada$Enfriamiento.i_ElevTempP3 <<- predict(MODELOS[['Enfriamiento.i_ElevTempP3']],entrada)$class
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
