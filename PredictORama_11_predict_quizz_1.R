############################################
# setwd('D:\\_GIT_\\Capston_Project\\')
# source('PredictORama_11_predict_quizz_1.R')
############################################

# scriptName
scriptName = "PredictORama_11_predict_quizz_1.R"
# load predict script
source("PredictORama_10_predict.R")

# Main ###############################################################################

tStart <-Sys.time()
print(paste(tStart,"> DEBUT",scriptName))
print("---------------------------------------------------")

load(paste(directoryDataFinal,"wFinal_",paste(minOcc, collapse = ''),".RData",sep=""))  
#load(url("https://github.com//ervachon//Capston_Project//raw//gh-pages//data.shiny//wFinal_StopWords.RData"))

quizz <- c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
           "You're the reason why I smile everyday. Can you follow me please? It would mean the",
           "Hey sunshine, can you follow me and make me the",
           "Very early observations on the Bills game: Offense still struggling but the",
           "Go on a romantic date at the",
           "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
           "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
           "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
           "Be grateful for the good times and keep the faith during the",
           "If this isn't the cutest thing you've ever seen, then you must be",
           "can i know just")

for (i in 1:length(quizz)){
  #for (i in 1:1){
  cleanSentence  <- returnSentenceNGramMax(quizz[i])
  theResListMLE  <- predictMLE(cleanSentence)
  print(paste(cleanSentence,'=',paste(theResListMLE, collapse = '/'),sep=""))
  
  theResListBO   <- predictBackOFF(cleanSentence)
  print(paste(cleanSentence,'=',paste(theResListBO, collapse = '/'),sep=""))
  print("---------------------------------------------------")
}



# end main ###########################################################################
tEnd <-Sys.time()
print(paste(tEnd,"end",sep=" > ",scriptName))
print(tEnd-tStart)
