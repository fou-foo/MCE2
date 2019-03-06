####################################
#####    J. Antonio Garcia #########
#####   jose.ramirez@cimat.mx ######
####################################
library(shinydashboard)
library(shiny)
library(plotly)
library(knitr)
library(rmarkdown)
####################################################
# Construccion de la UI                            #
# esto solo es la apariencia, no el funcionamiento #
####################################################

#NOTA: el objeto 'sidebar' tambien es un 'Closure' definido en el package 'shinydaShboard'
sidebar <- dashboardSidebar(
  #comenzamos con el menu que tiene el nombre y las villetas del lado derecho

  sidebarMenu(
    # se define un 'menuItem' por cada villeta, los parametros definen el dibujo
    # que se desplegara del lado izquierdo del 'dashboard'
    menuItem("CIMAT", tabName = "CIMAT", icon = icon("dashboard")),
    menuItem("DWD", icon = icon("th"), tabName = "DWD",
             badgeLabel = "nuevo", badgeColor = "green")
            )
    ) # aqui termina la 'Closure' que construye los tabs del lado izquierdo del dashboard

#cramos varias tabs
# NOTA: todo el contenido va dentro de un 'body' que tambien es una closure
body <- dashboardBody(
  # 'menuItems' construye la villeta
  # 'tabItems' es el complemento de 'menuItem'
  tabItems(
    # asu vez cada villeta contiene elementos
    tabItem(tabName = "CIMAT", #nombre de la tab que debe de corresponder con el nombre en 'menuItem'
            h2('Distance Weighted Discrimination (DWD)'), #mensajes de reyeno
            h3(' la maldicion de la dimensionalidad')
            ),
     # esta es otra villeta contiene una grafica de scatterplot
    tabItem(tabName = "DWD", #nombre del tab que se corresponde con la el nombre en 'menuItem'
            h2('Geometria de DWD'), #un feliz comentario
    fluidRow( #esta linea define el 'layout' define una linea (row) del ancho de toda la 'hoja' o pantalla en que se visualiza el dashboard
          # objeto box es una caja, la podemos reyenar de algun color con parametros extras
          box(  title = 'Datos simulados',
                  background = "light-blue",
                  solidHeader = TRUE,
              plotlyOutput("puntos", height = 230)) , #este es el objeto importante de este 'box')
          # otra vaja dentro del mismo row
          box(title = "Frontera opt.Bayes",
              background = "green", solidHeader = TRUE,
              plotlyOutput("Bayes", height = 230))
    ), # fin de row
    fluidRow(box( title = 'Direccion MDP',
              plotlyOutput("DWD", height = 230)),
    fluidRow(box( title = "Que dimension ?",
              h3("N fijo a 20"), br(),
              sliderInput("d", "d", min = 2, max = 1000, step = 5, value = 2) #este objeto define la barrita de slider su id es 'd' se forma en la lista 'input'
            )) ),
      fluidRow( box(width = 12,
                        h3("Los datos chaparros (HDLSS) tienden asin (d al inf y n fijo) a tener una
                            geometria  rigida"),
                        h5("Cuando d >>n los datos consisten en un subespacio n dimensional y la idea de trabajar en este espacio es IMPRACTICA"),
                        h6("Las nuevas observaciones se espera que aparezcan fuera de este subespacio"),
                        h6('En el contexto de microarreglos el interes recae solo en algunos genes y esta atencion es mas dificil de antener solo con algunas comb. lin. (i.e. cualquier base degerado por los datos) de los genes considerados')
                    ))
    )

    )# se termina este tab
  )#fin de body

# Esto inicializa el dashboard con todos lo que construimos
dashboardPage(skin = "purple",
  dashboardHeader(title = "CIMAT Monterrey"),
  sidebar,
  body
)