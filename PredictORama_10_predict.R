############################################
# setwd('D:\\_GIT_\\Capston_Project\\')
# source('PredictORama_10_predict.R')
############################################

# scriptName
scriptName = "PredictORama_10_predict.R"
# load common script
source("PredictORama_00_common.R")

#load(paste(directoryDataFinal,"wFinal_",paste(minOcc, collapse = ''),".RData",sep=""))  

######################################################################################
returnSentenceNGramMax <- function(laPhrase) {
   return(returnSentence(laPhrase,nbGram-1))
}

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

returnFrequenceNgram <- function(theNGram){
  n <- returnNbGram(theNGram)
  return (wFinal[[n]][[1]]$freq[match(theNGram, wFinal[[n]][[1]]$terms[])])
}

returnCountNgramCorpus <- function(theNGram){
  return (wFinal[[n]][[1]]$freq[match(theNGram, wFinal[[n]][[1]]$terms[])])
}

returnListNGramOfNgramLessOne <- function(theNgram){
  n <- returnNbGram(theNgram)
  listTmp <- wFinal[[n+1]][[1]][grep(paste("^",theNgram," ",sep=""),
                                     wFinal[[n+1]][[1]]$terms), ] 
  tmp <- c()
  if (nrow(listTmp)>0){tmp <-listTmp[1:min(nbResAnalyse,nrow(listTmp)),]}
  return(tmp)
}

####################################################################################################

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
  #while(is.null(res)==TRUE & i < nbGram ){
  while(length(res)<=nbRes & i < nbGram ){
    resTmp <- predictMLE(listSentence[[i]])
    if (length(resTmp)>0){
      for (j in 1:length(resTmp)){
         if ((resTmp[j] %in% res)==FALSE){res <- c(res,resTmp[j])}
      }
    }
    i<-i+1
  }
  return(res[1:(min(nbRes,length(res)))])
}

####################################################################################################

predictListGT <- function(listNgram){
   listTmp <- data.frame(e=character(),w=character(),a=character(),p=numeric())
   for (i in 1:(min(nrow(listNgram),nbResAnalyse))){
      w <- returnFirstWords(listNgram$terms[i])
      pw <- returnFrequenceNgram(w)
      p <- listNgram$freq[i]/pw
      listTmp <- rbind(listTmp,data.frame(listNgram$terms[i],w,a,p) )
      print(paste(listNgram$terms[i],w,listNgram$freq[i],pw,p,sep="/"))
      n <- returnNbGram(listNgram$terms[i])
      c <- (listNgram$freq[i]+1) * (1/1)
   }
   # trier la list 
   return(listTmp)
}

predictGT <- function(theSentence){
  tmp <- strsplit(trimws(theSentence), " ")
  listSentence <- vector(mode="list", length=nbGram-1)
  for (i in 1:(nbGram-1)){
    listSentence[[i]] <- paste(tmp[[1]][i:(nbGram-1)], collapse = ' ')
  }
  res <- predictListGT(theSentence[1])
  #i <- 2
  #while(is.null(res)==TRUE & i < nbGram ){
  #  res <- predictListGT(listSentence[[i]])
  #  i<-i+1
  #}
  #if (is.null(res)==TRUE) { res <- NA}
  return(res)
}

######################################################################################
# wFinal[[n]][[1]][grep(paste("^",tmp," ",listSentence <- vector(mode="list", length=nbGram-1)sep=""), wFinal[[n]][[1]]$terms), ] [1:3,]  
# wFinal[[nGram]][[1]]$freq[1]
# wFinal[[nGram]][[1]]$terms[1]
# wFinal[[nGram]][[2]]$freq[1]
# wFinal[[nGram]][[2]]$freqCount[1]

# freq de this dans 1gram
# wFinal[[nGram]][[1]]$freq[match("this", wFinal[[1]][[1]]$terms[])]
