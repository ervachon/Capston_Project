#setwd('D:\\_MOOC_\\git\\Capston_Project\\')
#source('optimize.R')

minOcc <- c(1,3,4,4)
directory = "D:\\_MOOC_\\git\\Capston_Project\\data\\"
nbGram <- 4

wListe <- vector(mode="list", length=nbGram)
for (i in 1:nbGram){
  load(paste(directory,"w",i,".RData",sep=""))
  #optimize the size  
  w <- wF[wF$freq>minOcc[i],]
  rownames(w) <- 1:nrow(w)
  wListe[[i]] <- w
  #save(w,file=paste(directory,"w",i,"_opt_",minOcc[i],".RData",sep=""))  
}

save(w,file=paste(directory,"wListe_",paste(minOcc, collapse = ''),".RData",sep=""))  

