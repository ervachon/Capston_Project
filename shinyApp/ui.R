# devtools::install_github('rstudio/rsconnect')
# library(rsconnect)
# devtools::install_github("rstudio/shinyapps")
# library(shiny)
# library(shinyapps)

#setwd("D:\\_GIT_\\Capston_Project\\shinyApp")
#runApp() 
#Sys.setlocale("LC_ALL","English")
#shinyapps::deployApp('D:\\_GIT_\\Capston_Project\\shinyApp',appName = "Capston_Project")

  
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(RCurl))

shinyUI(pageWithSidebar(
   headerPanel(HTML('Capston Project : PredictORama <h3>Eric VACHON - January 2016')),
   sidebarPanel( #width = 12,
      tabPanel("Common",
      radioButtons("corpus",
                   label = h4("Choice your corpus"),
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
                 
                 useShinyjs(),
                 tags$head(tags$style("#myText{color: red}")),
                 #h4(textOutput("myText")), 
                 h4(div(id = "myText", "Wait CORPUS is loading !!!")),
        
                 textInput("sentence", label = h4("Enter your sentence bellow:"), 
                           value='',width = '100%'), 
#                 verbatimTextOutput("triGram"),
#                 verbatimTextOutput("predict"),
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
