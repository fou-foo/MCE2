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
                   #variables continuas
                   sliderInput("Enfriamiento.i_TORElev4", label = h3("Enfriamiento.i_TORElev4"),
                               min = 0, max = 85, value = 0),
                   fluidRow(column(3, verbatimTextOutput("Enfriamiento.i_TORElev4select"))),
                   
                   sliderInput("Enfriamiento.i_HSRElev4", label = h3("Enfriamiento.i_HSRElev4"),
                               min = 0, max = 65, value = 0),
                   fluidRow(column(3, verbatimTextOutput("Enfriamiento.i_TORElev4select"))),
                   
                   sliderInput("Enfriamiento.i_TORElev5", label = h3("Enfriamiento.i_TORElev5"),
                               min = 0, max = 65, value = 0),
                   fluidRow(column(3, verbatimTextOutput("Enfriamiento.i_TORElev5select"))),
                   
            ),
        box(   width=6, h1('Sugerencia automatica basada en datos'),
               #variables numericas
               h4("Enfriamiento.i_TORElev4"),
            fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_TORElev4Auto"))),
            
            h4("Enfriamiento.i_HSRElev4"),
            fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_HSRElev4Auto"))),
            
            
            h4("Enfriamiento.i_TORElev5"),
            fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_TORElev5Auto"))),
            
            
            h4("Enfriamiento.i_TORElev4"),
            fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_TORElev4"))),
            
            
            h4("Enfriamiento.i_TORElev4"),
            fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_TORElev4"))),
            
            
            h4("Enfriamiento.i_TORElev4"),
            fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_TORElev4"))),
            
            #variables categoricas 
            )),


    tabItem(tabName = "Otros",
            box(   width=6, h1('Seleccion del operador')
                 


            ),
            box(width=6, h1('Sugerencia automatica basada en datos')
                



    )
  )))




# Put them together into a dashboardPage
dashboardPage(skin = "red",
  dashboardHeader(title = "PROLEC-SPI2019"),
  sidebar,
  body
)