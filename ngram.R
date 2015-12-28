#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
#install.packages("tm")
#install.packages("RWeka")
#install.packages("devtools")
#install.packages("wordcloud")
#install.packages("foreach")

#setwd('D:\\_MOOC_\\git\\Capston_Project\\')
#source('ngram.R')
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

directory = "D:\\_MOOC_\\_coursera_\\10 project\\final\\en_US\\"
nbGram <- 4
nbLinesLoad<-200000
listFiles = c('en_US.news.txt','en_US.blogs.txt','en_US.twitter.txt')
registerDoParallel(min(length(listFiles),detectCores()-1))
  
nGramMatrix <- function (y,n) { 
  print(Sys.time())
  print(paste("n=",n,sep=""))
  TermDocumentMatrix(y, control = list(tokenize = function(x) NGramTokenizer(x, Weka_control(min = n, max = n))))
}

tmp <- foreach (i=1:length(listFiles), .packages=c('tm','RWeka','slam','data.table')) %dopar% { 
  
  setwd('D:\\_MOOC_\\git\\Capston_Project\\data.tmp')
  load(paste("corpus",listFiles[i],".RData", sep="_"))
  
  #ngramification
  for (j in 1:nbGram){ 
    for (k in seq(0, length(myCorpus), by=nbLinesLoad)) {
      t  <- nGramMatrix(myCorpus[k:min(k+nbLinesLoad,length(myCorpus))],j)
      freq <- sort(row_sums(t), decreasing=TRUE)
      w <- data.table(terms=names(freq), freq=freq)
      save(w, file=paste("w",j,"_", format(k, scientific = F ) , "_", listFiles[i],".RData", sep=""))
      rm(w)
      gc()
    }
  }
}

print(paste(Sys.time(),"end",sep=" > "))
