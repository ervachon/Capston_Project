#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
#install.packages("tm")
#install.packages("RWeka")
#install.packages("devtools")
#install.packages("wordcloud")

#setwd('D:\\_MOOC_\\git\\Capston_Project\\')
#source('02.R')

# attention 64bit : jre dans c:\\programmes et non x86
library(rJava)
library(NLP)
library(RWeka)
library(tm)
library(data.table)
library(wordcloud)
library(slam) #slam::row_sums

listFiles = c('en_US.twitter.txt','en_US.news.txt','en_US.blogs.txt')
directory = "D:\\_MOOC_\\_coursera_\\10 project\\final\\en_US\\"
setwd('D:\\_MOOC_\\git\\Capston_Project\\data.tmp')

nbLinesLoad   <- 50000
percentSample <- 20

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

    f <- sample(f,length(f)*percentSample/100)

    # Remove non print data etc
    f <- iconv(f, "latin1", "ASCII", sub=" ");
    f <- gsub("[^[:alpha:][:space:][:punct:]]", "", f);
    
    for (j in seq(0, length(f), by=nbLinesLoad)) {
      
      print(paste(">",format(j,  scientific = F),"-",format(min(j+nbLinesLoad,length(f)), scientific = F), sep= "" ))
      print(paste(Sys.time(),"VCorpus ",sep=" > "))
      fCorpus <- VCorpus(VectorSource(f[j:min(j+nbLinesLoad,length(f))]))
      
      print(paste(Sys.time(),"tm_map_lower",sep=" > "))
      fCorpus <- tm_map(fCorpus, content_transformer(tolower))
      
      print(paste(Sys.time(),"tm_map_puntruation",sep=" > "))
      fCorpus <- tm_map(fCorpus, removePunctuation)
      
      print(paste(Sys.time(),"tm_map_number",sep=" > "))
      fCorpus <- tm_map(fCorpus, removeNumbers)
      
      print(paste(Sys.time(),"tm_map_whitespace",sep=" > "))
      fCorpus <- tm_map(fCorpus, stripWhitespace)
      
      # why not ?
      print(paste(Sys.time(),"tm_map_rm_words",sep=" > "))
      fCorpus <- tm_map(fCorpus, function(x)removeWords(x,stopwords()))
      
      #ngramification
      print(paste(Sys.time(),"gramification",sep=" > "))
      t1 <- nGramMatrix(fCorpus,1)
      t2 <- nGramMatrix(fCorpus,2)
      t3 <- nGramMatrix(fCorpus,3)
      t4 <- nGramMatrix(fCorpus,4)
      
      print(paste(Sys.time(),"freq",sep=" > "))
      freq1 <- sort(row_sums(t1), decreasing=TRUE)
      freq2 <- sort(row_sums(t2), decreasing=TRUE)
      freq3 <- sort(row_sums(t3), decreasing=TRUE)
      freq4 <- sort(row_sums(t4), decreasing=TRUE)
      
      print(paste(Sys.time(),"data.table"))
      w1 <- data.table(terms=names(freq1), freq=freq1)
      w2 <- data.table(terms=names(freq2), freq=freq2)
      w3 <- data.table(terms=names(freq3), freq=freq3)
      w4 <- data.table(terms=names(freq4), freq=freq4)
  
      print(paste(Sys.time(),"wordcloud",sep=" > "))
      pal = brewer.pal(9,"BuPu")
      wordcloud(words = w1$terms,freq = w1$freq,scale = c(3,.8),
                random.order = F,colors = pal,max.words=60)
      
      print(paste(Sys.time(),"save",sep=" > "))
      save(t1, file=paste("t1_",listFiles[i],"_", format(j, scientific = F ),".RData", sep=""))
      save(t2, file=paste("t2_",listFiles[i],"_", format(j, scientific = F ),".RData", sep=""))
      save(t3, file=paste("t3_",listFiles[i],"_", format(j, scientific = F ),".RData", sep=""))
      save(t4, file=paste("t4_",listFiles[i],"_", format(j, scientific = F ),".RData", sep=""))
   }
}
print(paste(Sys.time(),"end",sep=" > "))

#concate les df

# rm ?
# Create the word cloud
# strsplit(trimws(laPhrase)," ")
