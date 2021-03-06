############################################
# setwd('D:\\_GIT_\\Capston_Project\\')
# source('PredictORama_05_optimize.R')
############################################

# scriptName
scriptName <- "PredictORama_05_optimize.R"
# load comvmon script
source("PredictORama_00_common.R")

tStart <-Sys.time()
print(paste(tStart,"> DEBUT :",scriptName))

# Main ##############################################################################

for (i in 1:nbGram){
  load(paste(directoryData,"w",i,".RData",sep=""))
  #optimize the size  
  w <- wF[wF$freq>minOcc[i],]
  rownames(w) <- 1:nrow(w)
  save(w,file=paste(directoryData,"w",i,"_opt_",minOcc[i],".RData",sep=""))
}

# end main ##########################################################################

tEnd <-Sys.time()
print(paste(tEnd,"end",sep=" > ",scriptName))
print(tEnd-tStart)
