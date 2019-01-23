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
    menuItem("General", icon = icon("database"), tabName = "General"),
    menuItem("Otros", icon = icon("th"), tabName = "Otros")#,
  )
)
#cramos varias tabs
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "General",h1('Seleccion del operador'),
            box(   width=6, 
                   #variables continuas
                   selectInput("Enfriamiento.i_TORElev4", label = h4("Enfriamiento.i_TORElev4"), 
                               choices = list("NULL" = -1, "0" = 0, "53" = 53, "55" = 55, 
                                              "65" = 65), 
                               selected = -1),
                   fluidRow(column(3, verbatimTextOutput("Enfriamiento.i_TORElev4select"))),
                   
                   selectInput("Enfriamiento.i_HSRElev4", label = h4("Enfriamiento.i_HSRElev4"), 
                               choices = list("NULL" = -1, "0" = 0, "58" = 58, "70" = 70, 
                                              "80" = 80), 
                               selected = -1),
                   fluidRow(column(3, verbatimTextOutput("Enfriamiento.i_HSRElev4select"))),
                   
                   selectInput("Enfriamiento.i_TORElev5", label = h4("Enfriamiento.i_TORElev5"), 
                               choices = list("NULL" = -1, "0" = 0, "65" = 65), 
                               selected = -1),
                   fluidRow(column(3, verbatimTextOutput("Enfriamiento.i_TORElev5select"))),
                   
                   selectInput("Enfriamiento.i_HSRElev5", label = h4("Enfriamiento.i_HSRElev5"), 
                               choices = list("NULL" = -1, "0" = 0, "80" = 80), 
                               selected = -1),
                   fluidRow(column(3, verbatimTextOutput("Enfriamiento.i_HSRElev5select"))),
                   
                   sliderInput("dRuido2", label = h3("dRuido2"),
                               min = 0, max = 88, value = 0),
                   fluidRow(column(3, verbatimTextOutput("dRuido2select")))
                   
                   
            ),
        box(   width=6, h1('Sugerencia automatica basada en datos'),
               #variables numericas
               h4("Enfriamiento.i_TORElev4"),
            fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_TORElev4Auto"))),
            
            h4("Enfriamiento.i_HSRElev4"),
            fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_HSRElev4Auto"))),
            
            
            h4("Enfriamiento.i_TORElev5"),
            fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_TORElev5Auto"))),
            
            
            h4("Enfriamiento.i_HSRElev5"),
            fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_HSRElev5Auto"))),
            
            
            h4("dRuido2"),
            fluidRow(column(6, verbatimTextOutput("dRuido2Auto")))
            #variables categoricas 
            )),


    tabItem(tabName = "Otros",
            column(4,
                   
                   # Copy the line below to make a slider bar 
                   sliderInput("slider1", label = h3("Slider"), min = 0, 
                               max = 100, value = 50)
            ),
            fluidRow(
              column(4, verbatimTextOutput("value")))
            
                



    )
    
  )
  )




# Put them together into a dashboardPage
dashboardPage(skin = "red",
  dashboardHeader(title = "PROLEC-CIMAT"),
  sidebar,
  body
)