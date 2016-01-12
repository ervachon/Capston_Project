# devtools::install_github('rstudio/rsconnect')
# library(rsconnect)

# library(shiny)
# setwd("D:\\_GIT_\\Capston_Project")
# runApp() 
# shinyapps::deployApp('D:\\_GIT_\\Capston_Project')

library(shiny)

shinyUI(pageWithSidebar(
   headerPanel(HTML('Capston Project : PredictORama <h3>Eric VACHON - January 2016')),
   sidebarPanel( #width = 12,
      tabPanel("Common",
      radioButtons("corpus",
                   label =h4("Choice your corpus"),
                   choices = list("With Stop Words"= 1,
                                  "Without Stop Words" = 2),
                   selected = 1
                   ),
      img(src="logo.jpg", width = 253, height = 117),
      br()
      )),
   mainPanel(#width = 12,
      tabsetPanel(
        tabPanel("PredictORama", 
                 textInput("sentence", label = h4("Enter your sentence bellow:"), 
                           value='',width = '100%'), 
                 #verbatimTextOutput("triGram"),
                 #verbatimTextOutput("predict"),
                 fluidRow(#column(width=3,actionButton("res1", label="Top 1",width = '100%')),
                          uiOutput("button_1_4")
                          ),br(),
                 fluidRow(#column(width=3,actionButton("res5", label="Top 5",width = '100%')),
                          uiOutput("button_5_8")
                         )
                 )
        ,tabPanel("Documentation"  , includeHTML("./www/documentation.html"))
        ,tabPanel("Milestone Report"  , includeHTML("./www/index.html"))
        ))
))
