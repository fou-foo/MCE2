####################################
#####    J. Antonio Garcia #########
#####   jose.ramirez@cimat.mx ######
####################################
library(shinydashboard)
library(shiny)
library(plotly)
library(knitr)
library(rmarkdown)
#########################################
# Construccion de la UI                 #
#########################################
sidebar <- dashboardSidebar(
  #comenzamos con el menu
  sidebarMenu(
    menuItem("CIMAT", tabName = "CIMAT", icon = icon("dashboard")),
    menuItem("DWD", icon = icon("th"), tabName = "DWD",
             badgeLabel = "nuevo", badgeColor = "green")

  )
)
#cramos varias tabs
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "CIMAT",
            h2('Distance Weighted Discrimination (DWD),'),
            h3(' la maldicion de la dimensionalidad')
    ),

    tabItem(tabName = "DWD",
            h2('Geo de DWD'),
      fluidRow(
            box(  title = '',
                #  background = "light-blue",
             # background = "green",
             # solidHeader = TRUE,
              plotlyOutput("puntos", height = 230)
            ),
            box(
              title = "",
             # background = "light-blue", solidHeader = TRUE,
              plotlyOutput("Bayes", height = 230)
            )
          ),
            fluidRow(
            box(
              #title = "Proyección sobre la dirección MDP",
               #solidHeader = TRUE, background = 'light-blue',
              plotlyOutput("DWD", height = 230)
              ),
            fluidRow(
            box(
              title = "Que dimension ?",
              "Tamaño de muestra fijo 20", br(),
              sliderInput("d", "d", min = 2, max = 1000, step = 5, value = 2)
            ))
          ),
      fluidRow(
          box(width = 12,h3("Los datos chaparros (HDLSS) tienden asin (d al inf y n fijo) a tener una
                            geometria  rigida"),
               h5("Cuando d >>n los datos consisten en un subespacio n dimensional y la idea de trabajar en este espacio es IMPRACTICA"),
                h6("Las nuevas observaciones se espera que aparezcan fuera de este subespacio"),
            h6('En el contexto de microarreglos el interes recae solo en algunos genes y esta atencion es mas dificil de antener solo con algunas comb. lin. (i.e. cualquier base degerado por los datos) de los genes considerados')
          ))
    )

    )
  )

# Put them together into a dashboardPage
dashboardPage(skin = "purple",
  dashboardHeader(title = "CIMAT Monterrey"),
  sidebar,
  body
)