############################################
# setwd('D:\\_GIT_\\Capston_Project\\')
# source('PredictORama_10_predict.R')
############################################

# scriptName
scriptName = "PredictORama_10_predict.R"
# load comvmon script
source("PredictORama_00_common.R")

# function ###############################################################################
# wFinal[[n]][[1]][grep(paste("^",tmp," ",sep=""), wFinal[[n]][[1]]$terms), ] [1:3,]  
# wFinal[[nGram]][[1]]$freq[1]
# wFinal[[nGram]][[1]]$terms[1]
# wFinal[[nGram]][[2]]$freq[1]
# wFinal[[nGram]][[2]]$freqCount[1]

predictFreqNGram <- function(theSentence,nGram){
  res <- c()
  tmp <- wFinal[[nGram]][[1]][grep(paste("^",theSentence," ",sep=""), wFinal[[nGram]][[1]]$terms), ] 
  
  for (i in 1:(min(3,nrow(tmp)))){
    res <- c(res,tmp[i,]$terms)
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
  if (is.null(res)==TRUE) { res <- c("PLOUF")}
  return(res)
}

predictFreqN <- function(theSentence,n){
  res <- predictFreq(theSentence)
  return(res[n])
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


# Main ###############################################################################

tStart <-Sys.time()
print(paste(tStart,"> DEBUT",scriptName))
print("---------------------------------------------------")
#load wFinal
load(paste(directoryData,"wFinal_",paste(minOcc, collapse = ''),".RData",sep=""))  

quizz <- c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
           "You're the reason why I smile everyday. Can you follow me please? It would mean the",
           "Hey sunshine, can you follow me and make me the",
           "Very early observations on the Bills game: Offense still struggling but the",
           "Go on a romantic date at the",
           "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
           "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
           "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
           "Be grateful for the good times and keep the faith during the",
           "If this isn't the cutest thing you've ever seen, then you must be")

for (i in 1:length(quizz)){
#for (i in 1:1){
   cleanSentence <- returnSentence(quizz[i],nbGram)
   listRes <- predictFreqN(cleanSentence,1)
   print(listRes)
}
print("---------------------------------------------------")

#setkeyv ???

# end main ###########################################################################
tEnd <-Sys.time()
print(paste(tEnd,"end",sep=" > ",scriptName))
print(tEnd-tStart)
