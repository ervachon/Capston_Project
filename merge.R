#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
#install.packages("tm")
#install.packages("RWeka")
#install.packages("devtools")
#install.packages("wordcloud")
#install.packages("foreach")

#setwd('D:\\_MOOC_\\git\\Capston_Project\\')
#source('merge.R')
print(paste(Sys.time(),"> DEBUT"))

# attention 64bit : jre dans c:\\programmes et non x86
library(rJava)
library(NLP)
library(RWeka)
library(tm)
library(data.table)
library(wordcloud)
library(slam) #slam::row_sums

library(parallel)
library(foreach)
library(doParallel)

no_cores <- detectCores() - 1
#no_cores <- 2
registerDoParallel(no_cores)

listFiles = c('en_US.news.txt','en_US.blogs.txt','en_US.twitter.txt')
directoryLoad = "D:\\_MOOC_\\git\\Capston_Project\\data.tmp\\"
directorySave = "D:\\_MOOC_\\git\\Capston_Project\\data\\"
nbGram <- 4

mergeW <- function(w1,w2){
  if (nrow(w1) == 0){
    wTmp <- w2
  } else
    if (nrow(w2) == 0){
      wTmp <- w1
    } else
    { 
      w1 <- w1[order(w1$terms),]
      w2 <- w2[order(w2$terms),]
      w1$freq[w1$terms %in% w2$terms] <- w1$freq[w1$terms %in% w2$terms] + w2$freq[w2$terms %in% w1$terms]
      wTmp <- rbind(w1, w2[!(w2$terms %in% w1$terms),])
      #wTmp <- rbind(w1,w2)
      #wTmp <- aggregate(wTmp$freq,list(wTmp$terms),sum) 
      #colnames(wTmp) <- c('terms','freq')
      #sort dataframe
    }
  wTmp
}

tmp <- foreach (i=1:nbGram, .packages=c('tm','RWeka','slam','data.table')) %dopar% { 
#for (i in 1:nbGram){
  setwd('D:\\_MOOC_\\git\\Capston_Project\\data.tmp')
  print(paste(Sys.time(),i,sep=" > "))
  wF <- data.frame()
  files <- list.files(pattern = paste('^w',i,'.*RData$',sep=''))
  for (j in files){
    load(paste(directoryLoad,j,sep=''))
    print(paste('>',j))
    w  <- as.data.frame(w)
    wF <- mergeW(wF,w)
    rm(w)
    gc()
  }
  #sort wF ?
  wF <- wF[order(-wF$freq,wF$terms),]
  save(wF, file=paste(directorySave,"w",i,".RData", sep=""))
  rm(wF)
  gc()
} 

#pal = brewer.pal(9,"BuPu")
#wordcloud(words = wF$terms,
#          freq = wF$freq,
#          scale=c(5,0.5), 
#          max.words=100, 
#          random.order=FALSE, 
#          rot.per=0.35, 
#          use.r.layout=FALSE, 
#          colors=brewer.pal(8,"Dark2"))

print(paste(Sys.time(),"end",sep=" > "))
