# devtools::install_github('rstudio/rsconnect')
# library(rsconnect)
# devtools::install_github("rstudio/shinyapps")
# library(shiny)
# library(shinyapps)

# setwd("D:\\_GIT_\\Capston_Project\\shinyApp")
# runApp() 
# Sys.setlocale("LC_ALL","English")
# shinyapps::deployApp('D:\\_GIT_\\Capston_Project\\shinyApp',appName = "Capston_Project")

#suppressPackageStartupMessages(library(rJava))
#suppressPackageStartupMessages(library(NLP))
#suppressPackageStartupMessages(library(RWeka))
#suppressPackageStartupMessages(library(data.table))
#suppressPackageStartupMessages(library(slam))

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(RCurl))

shinyUI(pageWithSidebar(
   headerPanel(HTML('Capston Project : PredictORama <h3>Eric VACHON - January 2016')),
   sidebarPanel( width = 4,
      radioButtons("corpus",
                   label = h4("Choice your corpus"),
                   choices = list("With Stop Words"= 1,
                                  "Without Stop Words" = 2),
                   selected = 1
                   ),
      a(href="https://class.coursera.org/dsscapstone-006",img(src="logo.jpg", width = 253, height = 117)),
      br(),
      a(href="https://github.com/ervachon/Capston_Project",img(src="github.png", width = 253, height = 104))
      #(href="mailto:eric.vachon.pro@orange.fr?&subject=Capton Project Contact",img(src="mail.jpg"))
   ),
   mainPanel(#width = 12,
      tabsetPanel(
        tabPanel(strong("PredictORama"), 
                 
                 useShinyjs(),
                 tags$head(tags$style("#myText{color: red}")),
                 #h4(textOutput("myText")), 
                 h4(div(id = "myText", "Wait CORPUS is loading !!!")),
        
                 textInput("sentence", label = h4("Enter your sentence bellow:"), 
                           value='',width = '100%'), 
                 #verbatimTextOutput("triGram"),
                 #verbatimTextOutput("predict"),
                 #fluidRow(uiOutput("button_1_4")),
                 fluidRow(column(3,uiOutput("button_1")),
                          column(3,uiOutput("button_2")),
                          column(3,uiOutput("button_3")),
                          column(3,uiOutput("button_4"))),
                 br(),
                 #fluidRow(uiOutput("button_5_8"))
                 fluidRow(column(3,uiOutput("button_5")),
                          column(3,uiOutput("button_6")),
                          column(3,uiOutput("button_7")),
                          column(3,uiOutput("button_8")))
               )
        ,tabPanel("Documentation"  , includeHTML("./www/documentation.html"))
        #,tabPanel("Milestone Report"  , includeHTML("http://rpubs.com/ervachon/139299"))
        ))
    )
)

