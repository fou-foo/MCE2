####################################
#####    J. Antonio Garcia #########
#####   jose.ramirez@cimat.mx ######
####################################
library(shinydashboard)
library(shiny)
library(plotly)
library(ggplot2)
library(shiny)
load('entrada.rdata')
theme_set(theme_bw())
#########################################
# Construccion de la UI                 #
#########################################
sidebar <- dashboardSidebar(
  #comenzamos con el menu
  sidebarMenu(
    menuItem("General", icon = icon("database"), tabName = "General")#,
    #menuItem("Otros", icon = icon("th"), tabName = "Otros")#,
  )
)
#cramos varias tabs
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "General",
            box(   width=6, h1('Seleccion del operador'),
                   selectInput("tTipoArreglo", label = h4("tTipoArreglo"),
                               choices = list("NULL" = 0, "3" = 3, "4" = 4,
                                              '5'=5, '7'=7, '10'=10, '11'= 11, '12'=12,
                                              '14'=14, '16'=16, '17'=17, '18'=18,
                                              '19'=19),
                               selected = 0),
                   fluidRow(column(3, verbatimTextOutput("tTipoArregloselect"))),
            ),
        box(   width=6, h1('Sugerencia automatica basada en datos'),
               h4("tTipoAparato"),
            fluidRow(column(6, verbatimTextOutput("tTipoAparatoAuto"))),
             h4("iNumFases"),
             fluidRow(column(6, verbatimTextOutput("iNumFasesAuto"))),
             h4("tTipoArreglo"),
             fluidRow(column(6, verbatimTextOutput("tTipoArregloAuto"))),
             h4("tTipoOptimizador"),
             fluidRow(column(6, verbatimTextOutput("tTipoOptimizadorAuto"))),
             h4("bLlevaTerciario"),
            fluidRow(column(6, verbatimTextOutput("bLlevaTerciarioAuto")))
            )),


    tabItem(tabName = "Otros",
            box(   width=6, h1('Seleccion del operador'),
                   sliderInput("dVoltajeSistAT", label = h4("dVoltajeSistAT"), min = 0,
                               max = 500, value =0),
                   fluidRow(column(6, verbatimTextOutput("dVoltajeSistATselect"))),
                   sliderInput("dVoltajeSistATInterfaz",
                               label = h4("dVoltajeSistATInterfaz"), min = 0,
                               max = 500, value =0),
                   fluidRow(column(6, verbatimTextOutput("dVoltajeSistATInterfazselect"))),

                   selectInput("MSNM", label = h4("MSNM"),
                               choices = list("NULL" = 0, "1000" = 1000, "1100" = 1100,
                                              '1200'=1200, '1226'=1226,
                                              '1250'=1250, '1300'= 1300, '1400'=1400,
                                              '1500'=1500, '1524'=1524, '1650'=1650,
                                              '1800'=1800, '2000'=2000, '2050'=2050,
                                              '2069'=2069, '2200'=2200, '2300'=2300,
                                              '2350'=2350, '2438'=2438, '2500'=2500),
                               selected = 0),
                   fluidRow(column(3, verbatimTextOutput("MSNMselect")))


            ),
            box(width=6, h1('Sugerencia automatica basada en datos'),
                h4("dVoltajeSistAT"),
                fluidRow(column(6, verbatimTextOutput("dVoltajeSistATAuto"))),
                h4("dVoltajeSistATInterfaz"),
                fluidRow(column(6, verbatimTextOutput("dVoltajeSistATInterfazAuto"))),
                h4("MSNM"),
                fluidRow(column(6, verbatimTextOutput("MSNMAuto")))



    )
  )))




# Put them together into a dashboardPage
dashboardPage(skin = "red",
  dashboardHeader(title = "PROLEC-SPI2019"),
  sidebar,
  body
)