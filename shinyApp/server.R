#load(url("https://github.com//ervachon//Capston_Project//raw//gh-pages//data.shiny//wFinal_StopWords.RData"))
#load(url("https://github.com//ervachon//Capston_Project//raw//gh-pages//data.shiny//wFinal_noStopWords.RData"))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(rJava))
suppressPackageStartupMessages(library(NLP))
suppressPackageStartupMessages(library(RWeka))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(slam))

nbGram <- 4
nbRes <- 8
nbResAnalyse <- nbRes

SelectData <- function(clean){
  if (clean == TRUE) {
    withProgress(message = paste('##################################',
                                 'LOAD NO STOPWORD CORPUS',
                                 '##################################'), 
                 value = 0,
               { data <- readRDS('./www/wFinal_noStopWords.RDs') 
                 #incProgress(1,detail = "########## OK ##########")
                })
  } else {
    withProgress(message = paste('################################## ',
                                 'LOAD STOPWORD CORPUS',
                                 '################################## '), 
                 value = 0,
               { data <- readRDS('./www/wFinal_StopWords.RDs') 
                 #incProgress(1,detail = "########## OK ##########")
               })
  }
  return(data)
}

shinyServer(
  function(input, output,session) {    
    returnSentenceNGramMax <- function(laPhrase,clean) {
      return(returnSentence(laPhrase,nbGram-1,clean))
    }
    
    returnSentence <- function(laPhrase,n,clean) {
      #clean the sentence and take the n last words  
      laPhrase <- iconv(laPhrase, "latin1", "ASCII", sub=" ");
      laPhrase <- gsub("[^[:alpha:][:space:][:punct:]]", "", laPhrase)
      
      theSentence <- VCorpus(VectorSource(laPhrase))
      theSentence <- tm_map(theSentence, content_transformer(tolower))
      theSentence <- tm_map(theSentence, removePunctuation)
      theSentence <- tm_map(theSentence, removeNumbers)
      theSentence <- tm_map(theSentence, stripWhitespace)
      
      if (clean==TRUE){
        theSentence <- tm_map(theSentence, function(x)removeWords(x,stopwords()))
      }
      
      theSentence <- tm_map(theSentence, stripWhitespace)
      
      # here we take only the n last words
      tmp <- trimws(as.character(theSentence[[1]]))
      tmp <- strsplit(trimws(tmp), " ")
      
      #split avec espace les n derniers
      tmp <- tail(tmp[[1]],n)
      tmp <- paste(tmp, collapse = ' ')
      
      return(tmp)
    }
    
    returnFirstWords <- function(listWords){
      strTmp <- strsplit(trimws(listWords), " ")[[1]]
      return(paste(strTmp[1:(length(strTmp)-1)], collapse = ' '))
    }
    
    returnLastWord <- function(listWords){
      strTmp <- strsplit(trimws(listWords), " ")[[1]]
      return(strTmp[length(strTmp)])
    }
    
    returnNbGram <- function(word){
      return(length(strsplit(word," ")[[1]]))
    }
    
    predictMLE <- function(theSentence){
      res <- c()
      tmp <- returnListNGramOfNgramLessOne(theSentence)
      if (is.null(tmp)!=TRUE){
        for (i in 1:(min(nbRes,nrow(tmp)))){
          res <- c(res,returnLastWord(tmp[i,]$terms))
        }
      }
      return(res)
    }
    
    predictBackOFF <- function(theSentence){
      tmp <- strsplit(trimws(theSentence), " ")
      listSentence <- vector(mode="list", length=nbGram-1)
      for (i in 1:(nbGram-1)){
        listSentence[[i]] <- paste(tmp[[1]][i:(nbGram-1)], collapse = ' ')
      }
      res <- predictMLE(theSentence[1])
      i <- 2
      while(length(res)<=nbRes & i < nbGram ){
        resTmp <- predictMLE(listSentence[[i]])
        if (length(resTmp)>0){
          for (j in 1:length(resTmp)){
            if ((resTmp[j] %in% res)==FALSE){res <- c(res,resTmp[j])}
          }
        }
        i<-i+1
      }
      
      i<-1
      while (length(res) < nbRes){
        if ((wFinal()[[1]][[1]]$terms[i] %in% res)== FALSE){
          res <- c(res,wFinal()[[1]][[1]]$terms[i])}
        i <- i + 1
      }
      
      if (theSentence==""){res <- c('-','-' ,'-', '-','-','-','-','-')}
      
      return(res[1:(min(nbRes,length(res)))])
    }
    returnFrequenceNgram <- function(theNGram){
      n <- returnNbGram(theNGram)
      return (wFinal()[[n]][[1]]$freq[match(theNGram, wFinal()[[n]][[1]]$terms[])])
    }
    
    returnCountNgramCorpus <- function(theNGram){
      return (wFinal()[[n]][[1]]$freq[match(theNGram, wFinal()[[n]][[1]]$terms[])])
    }
    
    returnListNGramOfNgramLessOne <- function(theNgram){
      n <- returnNbGram(theNgram)
      listTmp <- wFinal()[[n+1]][[1]][grep(paste("^",theNgram," ",sep=""),
                                         wFinal()[[n+1]][[1]]$terms), ] 
      tmp <- c()
      if (nrow(listTmp)>0){tmp <-listTmp[1:min(nbResAnalyse,nrow(listTmp)),]}
      return(tmp)
    }
    
    wFinal <- reactive({SelectData(input$corpus == "2")})
    
    output$predict <- renderText({
          a <- predictBackOFF(returnSentenceNGramMax(input$sentence, input$corpus == "2"))
          res$list<<-a
          return(a)
      })
    
    output$triGram <- renderText({returnSentenceNGramMax(input$sentence, input$corpus == "2")})

    output$button_1_4 <- renderUI({
      lapply(1:4, function(i) {
        actionButton(inputId = paste0("b_", i), 
                     label   = res$list[i],
                     width = '24%')
      })
    })

    output$button_5_8 <- renderUI({
      lapply(5:8, function(i) {
        actionButton(inputId = paste0("b_", i), 
                     label   = res$list[i],
                     width = '24%')
      })
    })
    
    observeEvent(input[[paste("b_",1,sep="")]], {updateTextInput(session, "sentence", value = paste(input[["sentence"]],res$list[1]))})
    observeEvent(input[[paste("b_",2,sep="")]], {updateTextInput(session, "sentence", value = paste(input[["sentence"]],res$list[2]))})
    observeEvent(input[[paste("b_",3,sep="")]], {updateTextInput(session, "sentence", value = paste(input[["sentence"]],res$list[3]))})
    observeEvent(input[[paste("b_",4,sep="")]], {updateTextInput(session, "sentence", value = paste(input[["sentence"]],res$list[4]))})
    observeEvent(input[[paste("b_",5,sep="")]], {updateTextInput(session, "sentence", value = paste(input[["sentence"]],res$list[5]))})
    observeEvent(input[[paste("b_",6,sep="")]], {updateTextInput(session, "sentence", value = paste(input[["sentence"]],res$list[6]))})
    observeEvent(input[[paste("b_",7,sep="")]], {updateTextInput(session, "sentence", value = paste(input[["sentence"]],res$list[7]))})
    observeEvent(input[[paste("b_",8,sep="")]], {updateTextInput(session, "sentence", value = paste(input[["sentence"]],res$list[8]))})
  
    
    observe({res$list<-predictBackOFF(returnSentenceNGramMax(input$sentence,
                                                             input$corpus == "2")) })
    
    res <- reactiveValues(list=c('-','-' ,'-', '-','-','-','-','-'))

})

#can i know just
#avec : i know just   : how/the/what/in/a/to/like
#sans : can know just : case/dont/like/much/want/one/thought
