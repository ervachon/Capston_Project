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

directory = "D:\\_MOOC_\\_coursera_\\10 project\\final\\"

nGramMatrix <- function (y,n) { 
  print(Sys.time())
  print(paste("n=",n,sep=""))
  TermDocumentMatrix(y, control = list(tokenize = function(x) NGramTokenizer(x, Weka_control(min = n, max = n))))
}

print(paste(Sys.time(),"total.txt",sep=" > "))
myFile <- paste(directory,"total.txt",sep="")
f <- readLines(myFile, n = -1L, ok = TRUE, warn = FALSE,  skipNul = FALSE)
  
if (sample != 1) {f <- sample(f,length(f)*sample)}
  
# Remove non print data etc
f <- iconv(f, "latin1", "ASCII", sub=" ");
f <- gsub("[^[:alpha:][:space:][:punct:]]", "", f);

print(paste(Sys.time(),"VCorpus ",sep=" > "))
myCorpus <- VCorpus(VectorSource(f))
print(paste(Sys.time(),"tm_map_lower",sep=" > "))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  
print(paste(Sys.time(),"tm_map_puntruation",sep=" > "))
myCorpus <- tm_map(myCorpus, removePunctuation)

print(paste(Sys.time(),"tm_map_number",sep=" > "))
myCorpus <- tm_map(myCorpus, removeNumbers)

print(paste(Sys.time(),"tm_map_whitespace",sep=" > "))
myCorpus <- tm_map(myCorpus, stripWhitespace)
  
print(paste(Sys.time(),"end",sep=" > "))

#ngramification
j<-4
print(paste(Sys.time(),"gramification",sep=" > "))
t  <- nGramMatrix(myCorpus,j)

print(paste(Sys.time(),"freq",sep=" > "))
#freq[[j]] <- sort(row_sums(t[[j]]), decreasing=TRUE)
freq <- sort(row_sums(t), decreasing=TRUE)
    
print(paste(Sys.time(),"data.table"))
w <- data.table(terms=names(freq), freq=freq)
    
print(paste(Sys.time(),"save",sep=" > "))
setwd('D:\\_MOOC_\\git\\Capston_Project\\data.tmp')
save(w, file=paste("total.RData"))
pal = brewer.pal(9,"BuPu")
wordcloud(words = w$terms,freq = w$freq,scale = c(3,.8),
          random.order = F,colors = pal,max.words=60)

directoryData = "D:\\_MOOC_\\git\\Capston_Project\\data\\"
load(paste(directoryData,'w4.RData',sep=''))

print(paste(Sys.time(),"end",sep=" > "))

laListe <- c("a case of " ,"would mean the ","make me the ",
             "struggling but the ","date at the ","be on my ",
             "in quite some ","with his little ","faith during the ",
             "you must be ")

for (element in laListe)
{print(wF[grep(paste(element,sep=""), wF$terms),]) }

directoryLoad = "D:\\_MOOC_\\git\\Capston_Project\\data.tmp\\"
load(paste(directoryLoad,'corpus_en_US.twitter.txt_.RData',sep=''))



