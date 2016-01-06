# library(shiny)
# setwd("D:\\_GIT_\\Capston_Project")
# runApp() 
# shinyapps::deployApp('D:\\_GIT_\\Capston_Project')

library(shiny)

shinyUI(pageWithSidebar(
   headerPanel(HTML('Capston Project <h3>Eric VACHON - January 2016')),
   sidebarPanel( #width = 12,
      tabPanel("Common",
      h4("hello"))),
   mainPanel(#width = 12,
      tabsetPanel(
        tabPanel("PredictOWords"  , textOutput("selected"), plotOutput("ageHist"))
        ,tabPanel("Documentation"  , includeHTML("./www/documentation.html"))
        ))
))
