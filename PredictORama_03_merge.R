############################################
#setwd('D:\\_GIT_\\Capston_Project\\')
#source('PredictORama_03_merge.R')
############################################

# scriptName
scriptName <- "PredictORama_03_merge.R"
# load comvmon script
source("PredictORama_00_common.R")

tStart <-Sys.time()
print(paste(tStart,"> DEBUT :",scriptName))

# Main ###############################################################################

myCluster <- makeCluster(no_cores)
registerDoParallel(myCluster)

tmp <- foreach (i=1:nbGram, .packages=c('tm','RWeka','slam','data.table')) %dopar% { 
#for (i in 1:nbGram){
  print(paste(Sys.time(),i,sep=" > "))
  wF <- data.frame()
  files <- list.files(pattern = paste('^w',i,'.*RData$',sep=''),path=directoryDataTmp)
  for (j in files){
    load(paste(directoryDataTmp,j,sep=''))
    print(paste('>',j))
    w  <- as.data.frame(w)
    wF <- mergeW(wF,w)
    rm(w)
    gc()
  }
  wF <- wF[order(-wF$freq,wF$terms),]
  rm(wF)
  gc()
} 

stopCluster(myCluster)

# end main ###########################################################################

tEnd <-Sys.time()
print(paste(tEnd,"end",sep=" > ",scriptName))
print(tEnd-tStart)

