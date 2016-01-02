############################################
#setwd('D:\\_GIT_\\Capston_Project\\')
#source('PredictORama_04_ngram_count.R')
############################################

# scriptName
scriptName <- "PredictORama_04_ngram_count.R"
# load comvmon script
source("PredictORama_00_common.R")

tStart <-Sys.time()
print(paste(tStart,"> DEBUT :",scriptName))

# Main ##############################################################################

myCluster <- makeCluster(no_cores)
registerDoParallel(myCluster)
tmp <- foreach (i=1:nbGram, .packages=c('tm','RWeka','slam','data.table')) %dopar% { 
   load(paste(directoryData,"w",i,".RData", sep=""))
   freqCount<-data.frame(table(wF$freq))
   colnames(freqCount) <- c('freqCount','freq')
   save(freqCount, file=paste(directoryData,"w",i, "_freqCount.RData", sep=""))
   rm(freqCount)
   rm(wF)
   gc()
}
stopCluster(myCluster)

# end main ##########################################################################

tEnd <-Sys.time()
print(paste(tEnd,"end",sep=" > ",scriptName))
print(tEnd-tStart)
