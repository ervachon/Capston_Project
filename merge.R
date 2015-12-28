#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
#install.packages("tm")
#install.packages("RWeka")
#install.packages("devtools")
#install.packages("wordcloud")
#install.packages("foreach")

#setwd('D:\\_MOOC_\\git\\Capston_Project\\')
#source('Create_NGram_100percent_complet.R')
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
#no_cores <- detectCores() - 3
registerDoParallel(2)

listFiles = c('en_US.news.txt','en_US.blogs.txt','en_US.twitter.txt')
directory = "D:\\_MOOC_\\_coursera_\\10 project\\final\\en_US\\"
nbGram <- 4

mergeW <- function(w1,w2){
  if (nrow(w1) == 0){
    wTmp <- w2
  } else
    if (nrow(w2) == 0){
      wTmp <- w1
    } else
    { 
      wTmp <- rbind(w1,w2)
      wTmp <- aggregate(wTmp$freq,list(wTmp$terms),sum) 
      colnames(wTmp) <- c('terms','freq')
    }
  wTmp
}


print(paste(Sys.time(),"merge",sep=" > "))
for (i in 1:nbGram){
  #tmp <- foreach (i=1:nbGram) %dopar% { 
  wF <- data.frame()
  for (j in 1:length(listFiles)){
    setwd('D:\\_MOOC_\\git\\Capston_Project\\data.tmp\\')
    load(paste("w",i,"_",listFiles[j],".RData", sep=""))
    w  <- as.data.frame(w)
    wF <- mergeW(wF,w)
  }
  #sort wF
  print(paste(Sys.time(),i,sep=" > "))
  save(wF, file=paste("w",i,".RData", sep=""))
} 

print(paste(Sys.time(),"end",sep=" > "))
