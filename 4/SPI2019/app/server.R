####################################
#####    J. Antonio Garcia #########
#####   jose.ramirez@cimat.mx ######
####################################
library(plotly)
library(shiny)
library(ggplot2)
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
  output$Enfriamiento.i_TORElev4select <- renderPrint({ input$Enfriamiento.i_TORElev4 })
  output$Enfriamiento.i_HSRElev4select <- renderPrint({ input$Enfriamiento.i_HSRElev4 })
  output$Enfriamiento.i_TORElev5select <- renderPrint({ input$Enfriamiento.i_TORElev5 })
  output$Enfriamiento.i_HSRElev5select <- renderPrint({ input$Enfriamiento.i_HSRElev5 })
  output$dRuido2select <- renderPrint({ input$dRuido2 })
  
  #output$dVoltajeSistATselect <- renderPrint({ (input$dVoltajeSistAT) })
  #output$dVoltajeSistATInterfazselect <- renderPrint({ (input$dVoltajeSistATInterfaz) })
  #output$MSNMselect <- renderPrint({ (input$MSNM) })

  ############################################
  pronostico <- reactive({
    #variables numericas 
      if( as.numeric(input$Enfriamiento.i_TORElev4) != 0 )
      {
        entrada$Enfriamiento.i_TORElev4 <<- input$Enfriamiento.i_TORElev4
        #vriables continuas
        entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)
          entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
          entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )
          entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada)
          
          #entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
          #entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
          #entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
          #entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      if( as.numeric(input$Enfriamiento.i_HSRElev4)!=0)
      {
        entrada$Enfriamiento.i_HSRElev4 <<- input$Enfriamiento.i_HSRElev4
        #variables continuas
        entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )
        entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada)
        
        
        #  entrada$tTipoArreglo <<- predict(MODELOS[['tTipoArreglo']],entrada, type='class' )
         # entrada$tTipoOptimizador <<- predict(MODELOS[['tTipoOptimizador']],entrada, type='class' )
        #  entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
         # entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
        #  entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
        #  entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      if( as.numeric(input$Enfriamiento.i_TORElev5) != 0)
      {
        entrada$Enfriamiento.i_TORElev5 <<- input$Enfriamiento.i_TORElev5
        
        # variables continuas
        entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
        entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )
        entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada)
        
        
        #  entrada$iNumFases <<- predict(MODELOS[['iNumFases']],entrada, type='class' )
         # entrada$tTipoOptimizador <<- predict(MODELOS[['tTipoOptimizador']],entrada, type='class' )
        #  entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
        #  entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
        #  entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
        #  entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      if( as.numeric(input$Enfriamiento.i_HSRElev5)!=0 )
      {
        entrada$Enfriamiento.i_HSRElev5 <<- input$Enfriamiento.i_HSRElev5
        
        #variables numericas
        entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
        entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        entrada$dRuido2 <<- predict(MODELOS[['dRuido2']],entrada)
        
        
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
        entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
        entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        entrada$Enfriamiento.i_HSRElev5 <<- predict(MODELOS[['Enfriamiento.i_HSRElev5']],entrada )
        
        
         # entrada$bLlevaTerciario <<- input$bLlevaTerciario
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
     

      })






#variables numericas
  output$Enfriamiento.i_TORElev4Auto <- renderPrint({ as.character(pronostico()[1, 'Enfriamiento.i_TORElev4']) })
  output$Enfriamiento.i_HSRElev4Auto<- renderPrint({as.character(pronostico()[1, 'Enfriamiento.i_HSRElev4'] )})
  output$Enfriamiento.i_TORElev5Auto <- renderPrint({ as.character(pronostico()[, 'Enfriamiento.i_TORElev5']) })
  output$Enfriamiento.i_HSRElev5Auto<- renderPrint({as.character(pronostico()[1, 'Enfriamiento.i_HSRElev5'] )})
  output$dRuido2Auto <- renderPrint({ as.character( pronostico()[, 'dRuido2']) })
  
  #output$bLlevaTerciarioAuto <- renderPrint({ as.character(pronostico()[, 'bLlevaTerciario'] )})
  #output$dVoltajeSistATAuto <- renderPrint({ (pronostico()[, 'dVoltajeSistAT'] )})
  #output$dVoltajeSistATInterfazAuto <- renderPrint({ (pronostico()[, 'dVoltajeSistATInterfaz'] )})
  #output$MSNMAuto <- renderPrint({ as.character(pronostico()[, 'MSNM'] )})

  #############################################################################


}
