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
    tabItem(tabName = "General",
            box(   width=6, h1('Seleccion del operador'),
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
            fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_TORElev4Auto1"))),
            
            h4("Enfriamiento.i_HSRElev4"),
            fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_HSRElev4Auto1"))),
            
            
            h4("Enfriamiento.i_TORElev5"),
            fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_TORElev5Auto1"))),
            
            
            h4("Enfriamiento.i_HSRElev5"),
            fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_HSRElev5Auto1"))),
            
            
            h4("dRuido2"),
            fluidRow(column(6, verbatimTextOutput("dRuido2Auto1"))),
            
            
            ####################################################
            h4("Garantias.dCapExcitacion"),
            fluidRow(column(6, verbatimTextOutput("Garantias.dCapExcitacionAuto1"))),
            
            h4("Garantias.dCapEficReg"),
            fluidRow(column(6, verbatimTextOutput("Garantias.dCapEficRegAuto1"))),
            
            h4("Garantias.dCapPerd"),
            fluidRow(column(6, verbatimTextOutput("Garantias.dCapPerdAuto1"))),
            
            h4("Enfriamiento.i_ElevTempP3"),
            fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_ElevTempP3Auto1"))),
            
            h4("iDestino"),
            fluidRow(column(6, verbatimTextOutput("iDestinoAuto1")))
            
            )),


    tabItem(tabName = "Otros",
            #variables categoricas 
            box(   width=6, h1('Seleccion del operador'),
                   #variables continuas
                   selectInput("Garantias.dCapExcitacion", label = h4("Garantias.dCapExcitacion"), 
                               choices = list("NULL" = 0, "10"=10,    "100"= 100,   "108"=108,   "110"=110,   "12"=12,    "120"=120,   "13.2"=13.2,  "13.3"=13.3,
                                              "133.5"=133.5, "135"=135,   "14"=14,   "15"=15,    "150"=150,   "16"=16,    "16.5"=16.5,
                                              "16.8"=16.8,  "173"=173,   "18"=18,    "19"=19,    "20"=20,    "22"=22,    "22.4"=22.4, 
                                              "24"=24,    "240"=240,   "25"=25,    "27"=27,    "28"=28, 
                                              "30"=30,    "300"=300,   "33"=33,    "33.6"=33.6,  "35.5"=35.5,  
                                              "36"=36, "37.6"=37.6,  "38"=38,    "39"=39,    "40"=40,    "42"=42,    "45"=45,
                                              "47.22"=47.22, "48"=48,    "49"=49,    "5"=5,     "50"=50,
                                              "50.4"=50.4,  "51"=51,    "55"=55,    "57"=57,    "57.6"=57.6,  "57.8"=57.8,
                                              "60"=60,    "66"=66,    "7.5"=7.5,   "70"=70,    "72"=72,
                                              "75"=75,    "80"=80,    "81"=81,    "86"=86,    "87"=87,    "88"=88,
                                              "90"=90,    "95"=95,    "99"=99 ), selected = 0),
                   fluidRow(column(3, verbatimTextOutput("Garantias.dCapExcitacionselect"))),
                   
                   selectInput("Garantias.dCapEficReg", label = h4("Garantias.dCapEficReg"), 
                               choices = list("NULL" = 0, "10"=10,    "100"= 100,   "108"=108,   "110"=110,   "12"=12,    "120"=120,   "13.2"=13.2,  "13.3"=13.3,
                                              "133.5"=133.5, "135"=135,   "14"=14,   "15"=15,    "150"=150,   "16"=16,    "16.5"=16.5,
                                              "16.8"=16.8,  "173"=173,   "18"=18,    "19"=19,    "20"=20,    "22"=22,    "22.4"=22.4, 
                                              "24"=24,    "25"=25,     "27"=27,    "28"=28, 
                                              "30"=30,    "300"=300,   "33"=33,    "33.6"=33.6,  "35.5"=35.5,  
                                              "36"=36, "37.6"=37.6,  "38"=38,    "39"=39,    "40"=40, "400"=400,   "42"=42,    "45"=45,
                                              "47.22"=47.22, "48"=48,    "49"=49,    "5"=5,     "50"=50,
                                              "50.4"=50.4,  "51"=51,    "55"=55,    "57"=57,    "57.6"=57.6,  "57.8"=57.8,
                                              "60"=60,    "66"=66,    "7.5"=7.5,   "70"=70,    "72"=72,
                                              "75"=75,    "80"=80,    "81"=81,    "86"=86,    "87"=87,    "88"=88,
                                              "90"=90,    "95"=95,    "99"=99 ), selected = 0),
                   fluidRow(column(3, verbatimTextOutput("Garantias.dCapEficRegselect"))),
                   
                   selectInput("Garantias.dCapPerd", label = h4("Garantias.dCapPerd"), 
                               choices = list("NULL" = 0, "10"=10,    "100"= 100,   "108"=108,   "110"=110,   "12"=12,    "120"=120,   "13.2"=13.2,  "13.3"=13.3,
                                              "133.5"=133.5, "135"=135,   "14"=14,   "15"=15,    "150"=150,   "16"=16,    "16.5"=16.5,
                                              "16.8"=16.8,  "173"=173,   "18"=18,    "19"=19,    "20"=20,    "22"=22,    "22.4"=22.4, 
                                              "24"=24,    "240"=240,  "25"=25,     "27"=27,    "28"=28, 
                                              "30"=30,    "300"=300,   "33"=33,    "33.6"=33.6,  "35.5"=35.5,  
                                              "36"=36, "37.6"=37.6,  "38"=38,    "39"=39,    "40"=40,    "42"=42,    "45"=45,
                                              "47.22"=47.22, "48"=48,    "49"=49,    "5"=5,     "50"=50,
                                              "50.4"=50.4,  "51"=51,    "55"=55,    "57"=57,    "57.6"=57.6,  "57.8"=57.8,
                                              "60"=60,    "66"=66,    "7.5"=7.5,   "70"=70,    "72"=72,
                                              "75"=75,    "80"=80,    "81"=81,    "86"=86,    "87"=87,    "88"=88,
                                              "90"=90,    "95"=95,    "99"=99 ), selected = 0),
                   fluidRow(column(3, verbatimTextOutput("Garantias.dCapPerdselect"))),
                   
                   selectInput("Enfriamiento.i_ElevTempP3", label = h4("Enfriamiento.i_ElevTempP3"), 
                               choices = list("NULL" = -2, "-1"=-1,    "45"= 45,   "55"=55,   "56"=56,   "60"=60,    "65"=65 )
                               , selected = -2),
                   fluidRow(column(3, verbatimTextOutput("Enfriamiento.i_ElevTempP3select"))),
                   
                   selectInput("iDestino", label = h4("iDestino"), 
                               choices = list("NULL" = -2, "-1"=-1,    "0"= 0,   "1"=1,   "10"=10,   "12"=12,
                                              "14"=14,   "16"=16,  "19"=19,
                                              "22"=22, "23"=23,   "29"=29,   "3"=3,    "4"=4,   "40"=40,
                                              "43"=43,
                                              "54"=54, '57'=57, '7'=7 ), selected = -2),
                   fluidRow(column(3, verbatimTextOutput("iDestinoselect")))
                   
                  
                   
                   
            ),
            box(   width=6, h1('Sugerencia automatica basada en datos'),
                   #variables numericas
                   h4("Enfriamiento.i_TORElev4"),
                   fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_TORElev4Auto2"))),
                   
                   h4("Enfriamiento.i_HSRElev4"),
                   fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_HSRElev4Auto2"))),
                   
                   
                   h4("Enfriamiento.i_TORElev5"),
                   fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_TORElev5Auto2"))),
                   
                   
                   h4("Enfriamiento.i_HSRElev5"),
                   fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_HSRElev5Auto2"))),
                   
                   
                   h4("dRuido2"),
                   fluidRow(column(6, verbatimTextOutput("dRuido2Auto2"))),
                   
                   
                   ####################################################
                   h4("Garantias.dCapExcitacion"),
                   fluidRow(column(6, verbatimTextOutput("Garantias.dCapExcitacionAuto2"))),
                   
                   h4("Garantias.dCapEficReg"),
                   fluidRow(column(6, verbatimTextOutput("Garantias.dCapEficRegAuto2"))),
                   
                   h4("Garantias.dCapPerd"),
                   fluidRow(column(6, verbatimTextOutput("Garantias.dCapPerdAuto2"))),
                   
                   h4("Enfriamiento.i_ElevTempP3"),
                   fluidRow(column(6, verbatimTextOutput("Enfriamiento.i_ElevTempP3Auto2"))),
                   
                   h4("iDestino"),
                   fluidRow(column(6, verbatimTextOutput("iDestinoAuto2")))
                   
                      
                   
                  
            ))
  )
)



  
    






# Put them together into a dashboardPage
dashboardPage(skin = "red",
  dashboardHeader(title = "PROLEC-CIMAT"),
  sidebar,
  body
)