# library(shiny)
# setwd("D:\\_MOOC_\\git\\Developing_Data_Products")
# runApp() 
# shinyapps::deployApp('D:\\_MOOC_\\git\\Developing_Data_Products')

library(shiny)

shinyUI(pageWithSidebar(
   headerPanel(HTML('Developing Data Products Project<h3>Eric VACHON - October 2015')),
   sidebarPanel( #width = 12,
    tabsetPanel(
      tabPanel("Common",
      selectInput("data", 
                label = "Choose the course data",
                choices = list("Portugues","Math"),
                selected = "Portugues"
      ),
      br(),
      h4("Select colors of Histogram"),
        fluidRow(
          column(4,radioButtons("col1", label = h4("Female"),
                                 choices = list("red"   = 1, "orange" = 2, "blue"   = 3,
                                                "green" = 4, "brown"  = 5, "yellow" = 6,
                                                "pink"  = 7), selected = 1)),
          column(4,radioButtons("col2", label = h4("Male"),
                                 choices = list("red"   = 1, "orange" = 2, "blue"   = 3,
                                                "green" = 4, "brown"  = 5, "yellow" = 6,
                                                "pink"  = 7), selected = 3))
      ),
      
      textOutput("col1Sel"),
      textOutput("col2Sel"),
      br(),
      
      p("The data source : "),
      a("UC Irvine Machine Learning Repository", 
        href="https://archive.ics.uci.edu/ml/datasets/Student+Performance"),
      br(),
      a("(Download source)", 
         href="https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip")
   ),
   tabPanel("features selection",
            br(),
            actionButton("updSelFeat", "Update the selected features"),
            fluidRow(
              column(4, checkboxGroupInput("featuresSel1", 
                               label = h3("Select"), 
                               choices = list("school" = 1,"sex" = 2,"age" = 3,"address" = 4,"famsize" = 5,
                                              "Pstatus" = 6,"Medu" = 7,"Fedu" = 8,"Mjob" = 9,"Fjob" = 10,
                                              "reason" = 11,"guardian" = 12,"traveltime" = 13,"studytime" = 14,
                                              "failures" = 15,"schoolsup" = 16), selected = c(1:16)
                               )
              ),
              column(4, checkboxGroupInput("featuresSel2", 
                                            label = h3("features"), 
                                            choices = list("famsup" = 17,"paid" = 18,
                                                           "activities" = 19,"nursery" = 20,"higher" = 21,"internet" = 22,
                                                           "romantic" = 23,"famrel" = 24,"freetime" = 25,"goout" = 26,
                                                           "Dalc" = 27,"Walc" = 28,"health" = 29,"absences" = 30,"G1" = 31,
                                                           "G2" = 32), selected = c(17:32)
              )
             )
            )
    ))),
   mainPanel(#width = 12,
      tabsetPanel(
        tabPanel("Documentation"              , includeHTML("./www/documentation.html")),
        tabPanel("Histograms"                 , textOutput("selected"), plotOutput("ageHist")),
        tabPanel("Prediction"                 , textOutput("selFormula"), textOutput("featSel"), plotOutput("pred")),
        tabPanel("Browse data"                , textOutput("showed"), dataTableOutput(outputId="theDataSelected")),
        tabPanel("Source and data information", includeHTML("./www/source.html")),
        tabPanel("Map of the University"      , h4("University of Minho, Guimarães, Portugal, (41.5608°N 8.3968°W)"), htmlOutput("univ"))
        )
      )
   )
)
