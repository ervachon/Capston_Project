
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(RCurl))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(googleVis))

theURL <-"https://archive.ics.uci.edu/ml/datasets/Student+Performance"
  
theFormula <- function(theSelected) {
  f <- returnFeaturesSelected(theSelected)
  return( f )
}

loadCSV <- function(URL) {
  csv <- tryCatch( {read.csv(text=getURL(URL,ssl.verifypeer=0L, followlocation=1L),sep=';')},
                          error=function(cond) {return(read.csv(URL,sep=';'))})    
  return(csv)
}

SelectStudentData <- function(theData){
  if (theData == "Portugues") {
    studentData     <- loadCSV("https://raw.githubusercontent.com/ervachon/Developing_Data_Products/gh-pages/student/student-por.csv")   
  } else {
    studentData     <- loadCSV("https://raw.githubusercontent.com/ervachon/Developing_Data_Products/gh-pages/student/student-mat.csv")   
  }
  
  studentData$ageLevel <- as.factor(studentData$age)
  return(studentData)
  
}

theColors <- function(indice){
  col <- array(data <- c("red", "orange", "blue", "green", "brown", "yellow", "pink"))
  return(col[as.integer(indice)])
} 

myPredict <- function(myData,theSelectedFeatures){
  set.seed(12345)
  inTrain           <- createDataPartition(y=myData$G3, p=.8, list = FALSE)
  ourTrain          <- myData[inTrain,]
  ourTest           <- myData[-inTrain,]
  modelFit          <- train(as.formula(theFormula(theSelectedFeatures)), method = "glm",data=ourTrain)
  testPred          <- predict(modelFit,ourTest)
  results           <- cbind(ourTest$G3,testPred)
  colnames(results) <- c("Reference", "Prediction")
  return(as.data.frame(results))
}

returnFeaturesSelected <- function(theSelected) {
  formula <- ""
  if (1 %in% theSelected) { formula <- paste(formula," + " ,"school")}
  if (3 %in% theSelected) { formula <- paste(formula," + " ,"age")}
  if (4 %in% theSelected) { formula <- paste(formula," + " ,"address")}
  if (5 %in% theSelected) { formula <- paste(formula," + " ,"famsize")}
  if (6 %in% theSelected) { formula <- paste(formula," + " ,"Pstatus")}
  if (7 %in% theSelected) { formula <- paste(formula," + " ,"Medu")}
  if (8 %in% theSelected) { formula <- paste(formula," + " ,"Fedu")}
  if (9 %in% theSelected) { formula <- paste(formula," + " ,"Mjob")}
  if (10 %in% theSelected) { formula <- paste(formula," + " ,"Fjob")}
  if (11 %in% theSelected) { formula <- paste(formula," + " ,"reason")}
  if (12 %in% theSelected) { formula <- paste(formula," + " ,"guardian")}
  if (13 %in% theSelected) { formula <- paste(formula," + " ,"traveltime")}
  if (14 %in% theSelected) { formula <- paste(formula," + " ,"studytime")}
  if (15 %in% theSelected) { formula <- paste(formula," + " ,"failures")}
  if (16 %in% theSelected) { formula <- paste(formula," + " ,"schoolsup")}
  if (17 %in% theSelected) { formula <- paste(formula," + " ,"famsup")}
  if (18 %in% theSelected) { formula <- paste(formula," + " ,"paid")}
  if (19 %in% theSelected) { formula <- paste(formula," + " ,"activities")}
  if (20 %in% theSelected) { formula <- paste(formula," + " ,"nursery")}
  if (21 %in% theSelected) { formula <- paste(formula," + " ,"higher")}
  if (22 %in% theSelected) { formula <- paste(formula," + " ,"internet")}
  if (23 %in% theSelected) { formula <- paste(formula," + " ,"romantic")}
  if (24 %in% theSelected) { formula <- paste(formula," + " ,"famrel")}
  if (25 %in% theSelected) { formula <- paste(formula," + " ,"freetime")}
  if (26 %in% theSelected) { formula <- paste(formula," + " ,"goout")}
  if (27 %in% theSelected) { formula <- paste(formula," + " ,"Dalc")}
  if (28 %in% theSelected) { formula <- paste(formula," + " ,"Walc")}
  if (29 %in% theSelected) { formula <- paste(formula," + " ,"health")}
  if (30 %in% theSelected) { formula <- paste(formula," + " ,"absences")}
  if (31 %in% theSelected) { formula <- paste(formula," + " ,"G1")}
  if (32 %in% theSelected) { formula <- paste(formula," + " ,"G2")}
  
  return(paste("G3 ~ ",substring(formula,4)))
}

shinyServer(
  function(input, output) {    
    set.seed(12345)
        
    myData <- reactive({SelectStudentData(input$data)})
    
    output$selFormula      <- renderText({paste("The formula use in the generalized linear model (method='glm', Caret package) with the data ", input$data," is :")})
    output$selected        <- renderText({paste("You have selected : ", input$data)})
    output$showed          <- renderText({paste("Data : ", input$data)})
    output$theDataSelected <- renderDataTable({data <- myData()})
    output$ageHist         <- renderPlot(ggplot(myData(), aes(x=ageLevel,fill=sex))
                                            + geom_bar(binwidth=.5,position="dodge")
                                            + facet_grid(school ~ . )
                                            + xlab ("Age of the students")
                                            + scale_fill_manual(values=c( theColors(input$col1), theColors(input$col2)))
                                         )
    
    myPred      <- reactive({myPredict(myData(),
                                       if (input$updSelFeat==0)
                                          {c(1:32)}
                                       else
                                          {isolate ({c(input$featuresSel1,input$featuresSel2)})}
                                      )
                            })
    output$pred <- renderPlot( ggplot(myPred(), aes(x = Reference, y = Prediction)) 
                                   + geom_point(color='blue')
                                   + geom_abline(intercept=0,slope=1,colour='red') 
                                   + geom_smooth(color = 'green')
                             )
    output$featSel <- renderText({
      input$updSelFeat
      isolate({returnFeaturesSelected(c(input$featuresSel1,input$featuresSel2))})
      })
    
    dfUniv      <- reactive({data.frame(longLat=c("41.5608:-8.3968"),Tip=c("University of Minho, Guimarães, Portugal"))})
    output$univ <- renderGvis({gvisMap(dfUniv(), "longLat" , "Tip", options=list(showTip=TRUE,showLine=TRUE, 
                                                                           enableScrollWheel=TRUE,
                                                                           mapType='normal', useMapTypeControl=TRUE,
                                                                           zoomLevel=15,width=400, height=400))})
  }
)