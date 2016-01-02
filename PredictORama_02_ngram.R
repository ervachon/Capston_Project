############################################
#setwd('D:\\_GIT_\\Capston_Project\\')
#source('PredictORama_02_ngram.R')
############################################

# scriptName
scriptName <- "PredictORama_02_ngram.R"
# load comvmon script
source("PredictORama_00_common.R")

tStart <-Sys.time()
print(paste(tStart,"> DEBUT :",scriptName))

# Main ##############################################################################

# list of txt files in directoryRawData
listFiles <- returnListeFiles(directoryRawData)

myCluster <- makeCluster(no_cores)
registerDoParallel(myCluster)

tmp <- foreach (i=1:length(listFiles), .packages=c('tm','RWeka','slam','data.table')) %dopar% { 
  
  load(paste(directoryDataTmp,"corpus_",listFiles[i],".RData", sep=""))
  
  #ngramification
  for (j in 1:nbGram){ 
    for (k in seq(0, length(myCorpus), by=nbLinesLoad)) {
      t  <- nGramMatrix(myCorpus[k:min(k+nbLinesLoad,length(myCorpus))],j)
      freq <- sort(row_sums(t), decreasing=TRUE)
      w <- data.table(terms=names(freq), freq=freq)
      save(w, file=paste(directoryDataTmp,"w",j,"_", format(k, scientific = F ) , "_", listFiles[i],".RData", sep=""))
      rm(w)
      gc()
    }
  }
}

stopCluster(myCluster)

# end main ##########################################################################

tEnd <-Sys.time()
print(paste(tEnd,"end",sep=" > ",scriptName))
print(tEnd-tStart)

