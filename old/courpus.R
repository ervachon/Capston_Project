#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
#install.packages("tm")
#install.packages("RWeka")
#install.packages("devtools")
#install.packages("wordcloud")
#install.packages("foreach")

#setwd('D:\\_MOOC_\\git\\Capston_Project\\')
#source('corpus.R')
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
registerDoParallel(3)

listFiles = c('en_US.news.txt','en_US.blogs.txt','en_US.twitter.txt')
directory = "D:\\_MOOC_\\_coursera_\\10 project\\final\\en_US\\"

#for (i in 1:length(listFiles))
tmp <- foreach (i=1:length(listFiles), .packages=c('tm','RWeka','slam','data.table')) %dopar% { 
{
  print(paste(Sys.time(),listFiles[i],sep=" > "))
  myFile <- paste(directory,listFiles[i],sep="")
  f  <- readLines(myFile, n = -1L, ok = TRUE, warn = FALSE,  skipNul = FALSE)
  
  # Remove non print data etc
  f <- iconv(f, "latin1", "ASCII", sub=" ");
  f <- gsub("[^[:alpha:][:space:][:punct:]]", "", f);
  
  print(paste(Sys.time(),"VCorpus ",sep=" > "))
  myCorpus <- VCorpus(VectorSource(f))
  
  print(paste(Sys.time(),"tm_map_lower",sep=" > "))
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  
  print(paste(Sys.time(),"tm_map_puntruation",sep=" > "))
  my7Corpus <- tm_map(myCorpus, removePunctuation)
  
  print(paste(Sys.time(),"tm_map_number",sep=" > "))
  myCorpus <- tm_map(myCorpus, removeNumbers)
  
  print(paste(Sys.time(),"tm_map_rm_words",sep=" > "))
  myCorpus <- tm_map(myCorpus, function(x)removeWords(x,stopwords()))
  
  print(paste(Sys.time(),"tm_map_whitespace",sep=" > "))
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  
  save(myCorpus, file=paste("courpus",listFiles[i],".RData", sep="_"))
  
  }
}

print(paste(Sys.time(),"end",sep=" > "))
