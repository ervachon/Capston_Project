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

nGramMatrix <- function (y,n) { 
  print(Sys.time())
  print(paste("n=",n,sep=""))
  TermDocumentMatrix(y, control = list(tokenize = function(x) NGramTokenizer(x, Weka_control(min = n, max = n))))
}

for (i in 1:length(listFiles))
{
  print(paste(Sys.time(),listFiles[i],sep=" > "))
  myFile <- paste(directory,listFiles[i],sep="")
  f  <- readLines(myFile, n = -1L, ok = TRUE, warn = FALSE,  skipNul = FALSE)

  #test 1%
  f <- sample(f,length(f)*10/100)
  
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
  
  #save myCorpus?
  
  #ngramification
  #tmp <- foreach (j=1:nbGram, .packages=c('tm','RWeka','slam','data.table')) %dopar% { 
  for (j in 1:nbGram){ 
    print(paste(Sys.time(),"gramification",sep=" > "))
    t  <- nGramMatrix(myCorpus,j)
    
    print(paste(Sys.time(),"freq",sep=" > "))
    #freq[[j]] <- sort(row_sums(t[[j]]), decreasing=TRUE)
    freq <- sort(row_sums(t), decreasing=TRUE)
    
    print(paste(Sys.time(),"data.table"))
    w <- data.table(terms=names(freq), freq=freq)
    
    print(paste(Sys.time(),"save",sep=" > "))
    setwd('D:\\_MOOC_\\git\\Capston_Project\\data.tmp')
    save(w, file=paste("w",j,"_",listFiles[i],".RData", sep=""))
    #pal = brewer.pal(9,"BuPu")
    #wordcloud(words = w$terms,freq = w$freq,scale = c(3,.8),
    #          random.order = F,colors = pal,max.words=60)
  }
}

print(paste(Sys.time(),"wordcloud",sep=" > "))
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
