############################################
# setwd('D:\\_GIT_\\Capston_Project\\')
# source('PredictORama_07_createFinaleStructure.R')
############################################

# scriptName
scriptName <- "PredictORama_07_createFinaleStructure.R"
# load comvmon script
source("PredictORama_00_common.R")

tStart <-Sys.time()
print(paste(tStart,"> DEBUT :",scriptName))

# Main ##############################################################################

wFinal <- vector(mode="list", length=nbGram)
for (i in 1:nbGram){
  wTmp <- vector(mode="list", length=2)
  # load w who is the wF opt
  load(paste(directoryData,"w",i,"_opt_",minOcc[i],".RData",sep=""))
  wTmp[[1]] <- w
  # load freqCount
  load(paste(directoryData,"w",i,"_freqCount.RData",sep=""))
  wTmp[[2]] <- freqCount
  wFinal[[i]] <- wTmp
}
save(wFinal,file=paste(directoryData,"wFinal_",paste(minOcc, collapse = ''),".RData",sep=""))  

# end main ##########################################################################

tEnd <-Sys.time()
print(paste(tEnd,"end",sep=" > ",scriptName))
print(tEnd-tStart)
