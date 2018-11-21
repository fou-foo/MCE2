library(shiny)
library(shinythemes)



shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  h1("Corrector de palabras "),
  
  mainPanel(
    tabsetPanel(
      id = 'app',
      tabPanel("Correcion",
               hr(),
               textInput("input", label = h3('Typing here'), value = 'ecuacion de calom, densidaf, varivle alaetoria'),
               column(8, h4('escribiste')), 
               column(8, wellPanel(
                 verbatimTextOutput("entrada") )),
               hr(),
               column(8, h4('la mejor correcion es:')),
                column(8, wellPanel(
                 verbatimTextOutput("predict") )) )
      
    )  ) )
)
