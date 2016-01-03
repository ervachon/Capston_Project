############################################
# setwd('D:\\_GIT_\\Capston_Project\\')
# source('PredictORama_10_predict.R')
############################################

# scriptName
scriptName = "PredictORama_10_predict.R"
# load comvmon script
source("PredictORama_00_common.R")

######################################################################################

returnSentence <- function(laPhrase,n) {
  #clean the sentence and take the n last words  
  laPhrase <- iconv(laPhrase, "latin1", "ASCII", sub=" ");
  laPhrase <- gsub("[^[:alpha:][:space:][:punct:]]", "", laPhrase)
  
  theSentence <- VCorpus(VectorSource(laPhrase))
  theSentence <- tm_map(theSentence, content_transformer(tolower))
  theSentence <- tm_map(theSentence, removePunctuation)
  theSentence <- tm_map(theSentence, removeNumbers)
  theSentence <- tm_map(theSentence, stripWhitespace)
  
  # attention not always
  if (cleanStopWord==TRUE){
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

returnLastWord <- function(listWords){
  strTmp <- strsplit(trimws(listWords), " ")[[1]]
  return(strTmp[length(strTmp)])
}

######################################################################################
# wFinal[[n]][[1]][grep(paste("^",tmp," ",sep=""), wFinal[[n]][[1]]$terms), ] [1:3,]  
# wFinal[[nGram]][[1]]$freq[1]
# wFinal[[nGram]][[1]]$terms[1]
# wFinal[[nGram]][[2]]$freq[1]
# wFinal[[nGram]][[2]]$freqCount[1]

predictFreqNGram <- function(theSentence,nGram){
  res <- c()
  tmp <- wFinal[[nGram]][[1]][grep(paste("^",theSentence," ",sep=""), wFinal[[nGram]][[1]]$terms), ] 
  if (nrow(tmp)>0) {
    for (i in 1:(min(nbRes,nrow(tmp)))){
       res <- c(res,returnLastWord(tmp[i,]$terms))
    }
  }
  return(res)
}

predictFreq <- function(theSentence){
  tmp <- strsplit(trimws(theSentence), " ")
  theSentence3 <- paste(tmp[[1]][1:3], collapse = ' ')
  theSentence2 <- paste(tmp[[1]][2:3], collapse = ' ')
  theSentence1 <- paste(tmp[[1]][3:3], collapse = ' ')
  
  res <- predictFreqNGram(theSentence3,4)
  if (is.null(res)==TRUE) { res <- predictFreqNGram(theSentence2,3)}
  if (is.null(res)==TRUE) { res <- predictFreqNGram(theSentence1,2)}
  if (is.null(res)==TRUE) { res <- NA}
  return(res)
}

