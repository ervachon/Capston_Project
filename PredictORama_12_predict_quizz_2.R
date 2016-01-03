############################################
# setwd('D:\\_GIT_\\Capston_Project\\')
# source('PredictORama_12_predict_quizz_2.R')
############################################

# scriptName
scriptName = "PredictORama_12_predict_quizz_2.R"
# load predict script
source("PredictORama_10_predict.R")

# Main ###############################################################################

tStart <-Sys.time()
print(paste(tStart,"> DEBUT",scriptName))
print("---------------------------------------------------")

load(paste(directoryDataFinal,"wFinal_",paste(minOcc, collapse = ''),".RData",sep=""))  

quizz <- c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
           "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
           "I'd give anything to see arctic monkeys this",
           "Talking to your mom has the same effect as a hug and helps reduce your",
           "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
           "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
           "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
           "Every inch of you is perfect from the bottom to the",
           "I'm thankful my childhood was filled with imagination and bruises from playing",
           "I like how the same people are in almost all of Adam Sandler's")

for (i in 1:length(quizz)){
  #for (i in 1:1){
  cleanSentence <- returnSentence(quizz[i],nbGram-1)
  #listRes <- predictFreq(cleanSentence)
  theCompletRes <- predictFreqN(cleanSentence,1)
  theRes        <- strsplit(trimws(theCompletRes), " ")[[1]]
  print(paste(cleanSentence,theCompletRes,theRes[length(theRes)],sep=" > "))
}
print("---------------------------------------------------")

# end main ###########################################################################
tEnd <-Sys.time()
print(paste(tEnd,"end",sep=" > ",scriptName))
print(tEnd-tStart)
