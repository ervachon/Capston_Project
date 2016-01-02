############################################
#setwd('D:\\_GIT_\\Capston_Project\\')
#source('PredictORama_01_corpus.R')
############################################

# scriptName
scriptName <- "PredictORama_01_corpus.R"
# load comvmon script
source("PredictORama_00_common.R")

tStart <-Sys.time()
print(paste(tStart,"> DEBUT :",scriptName))

# Main ##############################################################################

# list of txt files in directoryRawData
listFiles <- returnListeFiles(directoryRawData)

#for (i in 1:length(listFiles)){
tmp <- foreach (i=1:length(listFiles), .packages=c('tm','RWeka','slam','data.table')) %dopar% { 
  print(paste(Sys.time(),listFiles[i],sep=" > "))
  myFile <- paste(directoryRawData,listFiles[i],sep="")
  f <- readLines(myFile, n = -1L, ok = TRUE, warn = FALSE,  skipNul = FALSE)
  
  if (sample != 1) {f <- sample(f,length(f)*sample)}
  
  # Remove non print data etc
  f <- iconv(f, "latin1", "ASCII", sub=" ");
  f <- gsub("[^[:alpha:][:space:][:punct:]]", "", f);
  
  print(paste(Sys.time(),"VCorpus ",sep=" > "))
  myCorpus <- VCorpus(VectorSource(f))
  rm(f)
  
  print(paste(Sys.time(),"tm_map_lower",sep=" > "))
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  
  print(paste(Sys.time(),"tm_map_puntruation",sep=" > "))
  myCorpus <- tm_map(myCorpus, removePunctuation)
  
  print(paste(Sys.time(),"tm_map_number",sep=" > "))
  myCorpus <- tm_map(myCorpus, removeNumbers)
  
  print(paste(Sys.time(),"tm_map_rm_words",sep=" > "))
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("english")) 
  
  print(paste(Sys.time(),"tm_map_whitespace",sep=" > "))
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  
  save(myCorpus, file=paste(directoryDataTmp,"corpus_",listFiles[i],".RData", sep=""))
  rm(myCorpus)
  
}

stopCluster(myCluster)

# end main ##########################################################################
tEnd <-Sys.time()
print(paste(tEnd,"end",sep=" > ",scriptName))
print(tEnd-tStart)

