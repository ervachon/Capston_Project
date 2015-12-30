#setwd('D:\\_MOOC_\\git\\Capston_Project\\')
#source('predict.R')

library(rJava)
library(NLP)
library(RWeka)
library(tm)
library(data.table)
library(wordcloud)
library(slam) #slam::row_sums

predict <- function(laPhrase,n) {
  #clean the sentence and take the n last words  
  # splitStr = strsplit(trimws(laPhrase)," ")
  laPhrase <- iconv(laPhrase, "latin1", "ASCII", sub=" ");
  laPhrase <- gsub("[^[:alpha:][:space:][:punct:]]", "", laPhrase)
  
  theSentence <- VCorpus(VectorSource(laPhrase))
  theSentence <- tm_map(theSentence, content_transformer(tolower))
  theSentence <- tm_map(theSentence, removePunctuation)
  theSentence <- tm_map(theSentence, removeNumbers)
  theSentence <- tm_map(theSentence, stripWhitespace)
  theSentence <- tm_map(theSentence, function(x)removeWords(x,stopwords()))
  theSentence <- tm_map(theSentence, stripWhitespace)
  
  # here we take only the 3 last words
  tmp<-trimws(as.character(theSentence[[1]]))
  tmp=strsplit(trimws(tmp), " ")
  
  #split avec espace les n derniers
  tmp <- tail(tmp[[1]],n)
  tmp <- paste(tmp, collapse = ' ')
  #a<-"me about this"
  #wF[grep(paste("^",a," ",sep=""), wF$terms), ] [1:10,]
  #nrow(wF[wF$freq>1,])
  tmp
}

directory <- 'D:\\_MOOC_\\git\\Capston_Project\\'
dataDirectory <- paste(directory,'data\\', sep='')
nbGram <- 4

wListe <- vector(mode="list", length=length(listFiles))
for (i in 1:nbGram){
  load(paste(dataDirectory,"w",i,".RData",sep=""))  
  wListe[i] <- wF
  rm(wF)
}

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

# strsplit(trimws(laPhrase)," ")
predict(quizz(0))




#setkeyv ???

