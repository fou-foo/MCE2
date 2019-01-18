####################################
#####    J. Antonio Garcia #########
#####   jose.ramirez@cimat.mx ######
####     Eduardo Uresti Charre     #
####     Luis Daniel Mendoza       #
####################################
library(plotly)
library(MASS)
library(shiny)
library(knitr)
library(rmarkdown)
library(ggplot2)
library(lubridate)
library(leaflet)
library(shinyLP)
library(gapminder)
library(ggplot2)
library(shiny)
theme_set(theme_bw())
#########################################
# Construccion del backend              #
#########################################
library(plotly)

####################################
server <- function(input, output) {
    load('MODELOS.rdata')
    load('entrada.rdata')

  #seleccion de datos simulados
  output$tTipoAparatoselect <- renderPrint({ input$tTipoAparato })
  output$iNumFasesselect <- renderPrint({ input$iNumFases })
  output$iFrecuenciaselect <- renderPrint({ input$iFrecuencia })

  output$tTipoArregloselect <- renderPrint({ input$tTipoArreglo })
  output$tTipoOptimizadorselect <- renderPrint({ input$tTipoOptimizador })
  output$dVoltajeSistATselect <- renderPrint({ (input$dVoltajeSistAT) })
  output$dVoltajeSistATInterfazselect <- renderPrint({ (input$dVoltajeSistATInterfaz) })
  output$MSNMselect <- renderPrint({ (input$MSNM) })

  ############################################
  pronostico <- reactive({
      if( input$tTipoAparato!='0' )
      {
          entrada$tTipoAparato <<- input$tTipoAparato
          entrada$iNumFases <<- predict(MODELOS[['iNumFases']],entrada, type='class' )
          entrada$tTipoArreglo <<- predict(MODELOS[['tTipoArreglo']],entrada, type='class' )
          entrada$tTipoOptimizador <<- predict(MODELOS[['tTipoOptimizador']],entrada, type='class' )
          entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
          entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
          entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
          entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      if( input$iNumFases!='0')
      {
          entrada$iNumFases <<- input$iNumFases
          entrada$tTipoAparato <<- predict(MODELOS[['tTipoAparato']],entrada, type='class' )
          entrada$tTipoArreglo <<- predict(MODELOS[['tTipoArreglo']],entrada, type='class' )
          entrada$tTipoOptimizador <<- predict(MODELOS[['tTipoOptimizador']],entrada, type='class' )
          entrada$bLlevaTerciario <<- predict(MODELOS[['bLlevaTerciario']],entrada, type='class' )
          entrada$dVoltajeSistAT <<- predict(MODELOS[['dVoltajeSistAT']],entrada )
          entrada$dVoltajeSistATInterfaz <<- predict(MODELOS[['dVoltajeSistATInterfaz']],entrada )
          entrada$MSNM <<- predict(MODELOS[['MSNM']],entrada, type='class' )

          return(entrada)
      }
      if( input$tTipoArreglo!='0')
      {
          entrada$tTipoArreglo <<- input$tTipoArreglo
          entrada$tTipoAparato <<- predict(MODELOS[['tTipoAparato']],entrada, type='class' )
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
          entrada$dVoltajeSistAT <<- input$dVoltajeSistAT
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
          entrada$dVoltajeSistATInterfaz <<- input$dVoltajeSistATInterfaz
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
          entrada$MSNM <<- input$MSNM
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







  output$tTipoAparatoAuto <- renderPrint({ as.character(pronostico()[1, 'tTipoAparato']) })
  output$iNumFasesAuto <- renderPrint({as.character(pronostico()[1, 'iNumFases'] )})
  output$tTipoArregloAuto <- renderPrint({ as.character(pronostico()[, 'tTipoArreglo']) })
  output$tTipoOptimizadorAuto <- renderPrint({ as.character( pronostico()[, 'tTipoOptimizador']) })
  output$bLlevaTerciarioAuto <- renderPrint({ as.character(pronostico()[, 'bLlevaTerciario'] )})
  output$dVoltajeSistATAuto <- renderPrint({ (pronostico()[, 'dVoltajeSistAT'] )})
  output$dVoltajeSistATInterfazAuto <- renderPrint({ (pronostico()[, 'dVoltajeSistATInterfaz'] )})
  output$MSNMAuto <- renderPrint({ as.character(pronostico()[, 'MSNM'] )})

  #############################################################################


}
