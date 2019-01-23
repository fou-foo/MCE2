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
    load('MODELOS.rdata')
    load('entrada.rdata')

  #seleccion de datos simulados
  output$Enfriamiento.i_TORElev4select <- renderPrint({ input$Enfriamiento.i_TORElev4 })
  output$Enfriamiento.i_HSRElev4select <- renderPrint({ input$Enfriamiento.i_HSRElev4 })
  output$Enfriamiento.i_TORElev5select <- renderPrint({ input$Enfriamiento.i_TORElev5 })

  
  output$tTipoArregloselect <- renderPrint({ input$tTipoArreglo })
  output$tTipoOptimizadorselect <- renderPrint({ input$tTipoOptimizador })
  output$dVoltajeSistATselect <- renderPrint({ (input$dVoltajeSistAT) })
  output$dVoltajeSistATInterfazselect <- renderPrint({ (input$dVoltajeSistATInterfaz) })
  output$MSNMselect <- renderPrint({ (input$MSNM) })

  ############################################
  pronostico <- reactive({
    #variables numericas 
      if( as.numeric(input$Enfriamiento.i_TORElev4) != 0 )
      {
        #vriables continuas
          entrada$Enfriamiento.i_TORElev4 <<- input$Enfriamiento.i_TORElev4
          entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
          
          
          entrada$tTipoArreglo <<- predict(MODELOS[['tTipoArreglo']],entrada, type='class' )
          entrada$tTipoOptimizador <<- predict(MODELOS[['tTipoOptimizador']],entrada, type='class' )
          entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
          entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
          entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
          entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      if( as.numeric(input$Enfriamiento.i_HSRElev4)!=0)
      {
        entrada$Enfriamiento.i_HSRElev4 <<- input$Enfriamiento.i_HSRElev4
        #variables continuas
        entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        
        
          entrada$tTipoArreglo <<- predict(MODELOS[['tTipoArreglo']],entrada, type='class' )
          entrada$tTipoOptimizador <<- predict(MODELOS[['tTipoOptimizador']],entrada, type='class' )
          entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
          entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
          entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
          entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      if( as.numeric(input$Enfriamiento.i_TORElev5) != 0)
      {
        entrada$Enfriamiento.i_TORElev5 <<- input$Enfriamiento.i_TORElev5
        
        # variables continuas
        entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
        entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        
        
          entrada$iNumFases <<- predict(MODELOS[['iNumFases']],entrada, type='class' )
          entrada$tTipoOptimizador <<- predict(MODELOS[['tTipoOptimizador']],entrada, type='class' )
          entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
          entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
          entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
          entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      if( input$tTipoOptimizador!='-2' )
      {
        #variables numericas
        entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
        entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        
        
          entrada$tTipoOptimizador <<- input$tTipoOptimizador
          entrada$tTipoAparato <<- predict(MODELOS[['tTipoAparato']],entrada, type='class' )
          entrada$iNumFases <<- predict(MODELOS[['iNumFases']],entrada, type='class' )
          entrada$tTipoArreglo <<- predict(MODELOS[['tTipoArreglo']],entrada, type='class' )
          entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
          entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
          entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
          entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      if( input$bLlevaTerciario!='-1' )
      {
        #variables numericas
        entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
        entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        
        
          entrada$bLlevaTerciario <<- input$bLlevaTerciario
          entrada$tTipoAparato <<- predict(MODELOS[['tTipoAparato']],entrada, type='class' )
          entrada$iNumFases <<- predict(MODELOS[['iNumFases']],entrada, type='class' )
          entrada$tTipoArreglo <<- predict(MODELOS[['tTipoArreglo']],entrada, type='class' )
          entrada$tTipoOptimizador <<- predict(MODELOS[['tTipoOptimizador']],entrada, type='class' )
          entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
          entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
          entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      ###########segundo tab
      if( as.numeric(input$dVoltajeSistAT)!=0)
      {
        #variables numericas
        entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
        entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        
        
        entrada$tTipoAparato <<- predict(MODELOS[['tTipoAparato']],entrada, type='class' )
          entrada$iNumFases <<- predict(MODELOS[['iNumFases']],entrada, type='class' )
          entrada$tTipoArreglo <<- predict(MODELOS[['tTipoArreglo']],entrada, type='class' )
          entrada$tTipoOptimizador <<- predict(MODELOS[['tTipoOptimizador']],entrada, type='class' )
          entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
          entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
          entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      if( as.numeric(input$dVoltajeSistATInterfaz)!=0)
      {
        #variables numericas
        entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
        entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        
        
        entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
          entrada$tTipoAparato <<- predict(MODELOS[['tTipoAparato']],entrada, type='class' )
          entrada$iNumFases <<- predict(MODELOS[['iNumFases']],entrada, type='class' )
          entrada$tTipoArreglo <<- predict(MODELOS[['tTipoArreglo']],entrada, type='class' )
          entrada$tTipoOptimizador <<- predict(MODELOS[['tTipoOptimizador']],entrada, type='class' )
          entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
          entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      if( input$MSNM!='0')
      {
        #variables numericas
        entrada$Enfriamiento.i_TORElev4 <<- predict(MODELOS[['Enfriamiento.i_TORElev4']],entrada )
        entrada$Enfriamiento.i_HSRElev4 <<- predict(MODELOS[['Enfriamiento.i_HSRElev4']],entrada)
        entrada$Enfriamiento.i_TORElev5 <<- predict(MODELOS[['Enfriamiento.i_TORElev5']],entrada)
        
        
        entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
          entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
          entrada$tTipoAparato <<- predict(MODELOS[['tTipoAparato']],entrada, type='class' )
          entrada$iNumFases <<- predict(MODELOS[['iNumFases']],entrada, type='class' )
          entrada$tTipoArreglo <<- predict(MODELOS[['tTipoArreglo']],entrada, type='class' )
          entrada$tTipoOptimizador <<- predict(MODELOS[['tTipoOptimizador']],entrada, type='class' )
          entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
          return(entrada)
      }

      })






#variables numericas
  output$Enfriamiento.i_TORElev4Auto <- renderPrint({ as.character(pronostico()[1, 'Enfriamiento.i_TORElev4']) })
  output$Enfriamiento.i_HSRElev4Auto<- renderPrint({as.character(pronostico()[1, 'Enfriamiento.i_HSRElev4'] )})
  output$Enfriamiento.i_TORElev5 <- renderPrint({ as.character(pronostico()[, 'Enfriamiento.i_TORElev5']) })

    output$tTipoOptimizadorAuto <- renderPrint({ as.character( pronostico()[, 'tTipoOptimizador']) })
  output$bLlevaTerciarioAuto <- renderPrint({ as.character(pronostico()[, 'bLlevaTerciario'] )})
  output$dVoltajeSistATAuto <- renderPrint({ (pronostico()[, 'dVoltajeSistAT'] )})
  output$dVoltajeSistATInterfazAuto <- renderPrint({ (pronostico()[, 'dVoltajeSistATInterfaz'] )})
  output$MSNMAuto <- renderPrint({ as.character(pronostico()[, 'MSNM'] )})

  #############################################################################


}
