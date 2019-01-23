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
        entrada$Enfriamiento.i_TORElev4 <<- input$Enfriamiento.i_TORElev4
        #vriables continuas
        #entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)
         # entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        #  entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )
         # entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada)
        
        # variables categoricas
        #entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada, type='class' )
        
        
          #entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
          #entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
          #entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
          #entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      if( as.numeric(input$Enfriamiento.i_HSRElev4)!= -1)
      {
        entrada$Enfriamiento.i_HSRElev4 <<- input$Enfriamiento.i_HSRElev4
        #variables continuas
        # entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
        # entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        # entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )
        # entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada)
        # 
        # variables categoricas
        #entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada, type='class' )
        
        
        #  entrada$tTipoArreglo <<- predict(MODELOS[['tTipoArreglo']],entrada, type='class' )
         # entrada$tTipoOptimizador <<- predict(MODELOS[['tTipoOptimizador']],entrada, type='class' )
        #  entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
         # entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
        #  entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
        #  entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      if( as.numeric(input$Enfriamiento.i_TORElev5) != -1)
      {
        entrada$Enfriamiento.i_TORElev5 <<- input$Enfriamiento.i_TORElev5
        
        # variables continuas
        # entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
        # entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)
        # entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        # entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )
        # entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada)
        # 
        # variables categoricas
        #entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada, type='class' )
        
        
        #  entrada$iNumFases <<- predict(MODELOS[['iNumFases']],entrada, type='class' )
         # entrada$tTipoOptimizador <<- predict(MODELOS[['tTipoOptimizador']],entrada, type='class' )
        #  entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
        #  entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
        #  entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
        #  entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      if( as.numeric(input$Enfriamiento.i_HSRElev5)!= -1 )
      {
        entrada$Enfriamiento.i_HSRElev5 <<- input$Enfriamiento.i_HSRElev5
        
        #variables numericas
        # entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
        # entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)
        # entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        # entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada)
        # 
        # variables categoricas
        #entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada, type='class' )
        
        
         # entrada$tTipoOptimizador <<- input$tTipoOptimizador
          #entrada$tTipoAparato <<- predict(MODELOS[['tTipoAparato']],entrada, type='class' )
          #entrada$iNumFases <<- predict(MODELOS[['iNumFases']],entrada, type='class' )
          #entrada$tTipoArreglo <<- predict(MODELOS[['tTipoArreglo']],entrada, type='class' )
          #entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
          #entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
          #entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
          #entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      if( as.numeric(input$dRuido2) != 0 )
      {
        entrada$dRuido2 <<- input$dRuido2
        #variables numericas
        # entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
        # entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)
        # entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        # entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )
        # 
        
        # variables categoricas
        #entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada, type='class' )
        #entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada, type='class' )
      
          
        #entrada$tTipoAparato <<- predict(MODELOS[['tTipoAparato']],entrada, type='class' )
          #entrada$iNumFases <<- predict(MODELOS[['iNumFases']],entrada, type='class' )
          #entrada$tTipoArreglo <<- predict(MODELOS[['tTipoArreglo']],entrada, type='class' )
          #entrada$tTipoOptimizador <<- predict(MODELOS[['tTipoOptimizador']],entrada, type='class' )
          #entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
          #entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
          #entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      ###########segundo tab
    if( input$Garantias.dCapExcitacion != '0' )
    {
      entrada$Garantias.dCapExcitacion <<- input$Garantias.dCapExcitacion
      #variables numericas
      # entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
      # entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)
      # entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
      # entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )
      # 
      
      #variables categoricas   
      #entrada$Garantias.dCapExcitacion <<- predict(MODELOS[['Garantias.dCapExcitacion']],entrada, type='class' )
      
      #entrada$tTipoArreglo <<- predict(MODELOS[['tTipoArreglo']],entrada, type='class' )
      #entrada$tTipoOptimizador <<- predict(MODELOS[['tTipoOptimizador']],entrada, type='class' )
      #entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
      #entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
      #entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )
      
      return(entrada)
    }
    
  
     

      })






#variables numericas
  output$Enfriamiento.i_TORElev4Auto <- renderPrint({ as.numeric(pronostico()[1, 'Enfriamiento.i_TORElev4']) })
  output$Enfriamiento.i_HSRElev4Auto<- renderPrint({as.numeric(pronostico()[1, 'Enfriamiento.i_HSRElev4'] )})
  output$Enfriamiento.i_TORElev5Auto <- renderPrint({ as.numeric(pronostico()[, 'Enfriamiento.i_TORElev5']) })
  output$Enfriamiento.i_HSRElev5Auto<- renderPrint({as.numeric(pronostico()[1, 'Enfriamiento.i_HSRElev5'] )})
  output$dRuido2Auto <- renderPrint({ as.numeric( pronostico()[, 'dRuido2']) })
#variables categorcias 
  output$Garantias.dCapExcitacionAuto <- renderPrint({ as.character( pronostico()[, 'Garantias.dCapExcitacion']) })
  output$Garantias.dCapEficRegAuto <- renderPrint({ as.character( pronostico()[, 'Garantias.dCapEficReg']) })
  output$Garantias.dCapPerdAuto <- renderPrint({ as.character( pronostico()[, 'Garantias.dCapPerd']) })
  output$Enfriamiento.i_ElevTempP3Auto <- renderPrint({ as.character( pronostico()[, 'Enfriamiento.i_ElevTempP3']) })
  output$iDestinoAuto <- renderPrint({ as.character( pronostico()[, 'iDestino']) })
  

  #############################################################################


}
