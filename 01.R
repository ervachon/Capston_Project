#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
#install.packages("tm")
#install.packages("RWeka")
#install.packages("devtools")
#install.packages("wordcloud")

#setwd('D:\\_MOOC_\\git\\Capston_Project\\')
#source('01.R')

# attention 64bit : jre dans c:\\programmes et non x86
library(rJava)
library(NLP)
library(RWeka)
library(tm)
library(data.table)
library(wordcloud)
library(slam)

listFiles = c('en_US.twitter.txt','en_US.news.txt','en_US.blogs.txt')
directory = "D:\\_MOOC_\\_coursera_\\10 project\\final\\en_US\\"
setwd('D:\\_MOOC_\\git\\Capston_Project\\data.tmp')

nbLinesLoad = 100000

print(Sys.time())
for (i in 1:length(listFiles))
{
    myFile <- paste(directory,listFiles[i],sep="")
    f  <- readLines(myFile, n = -1L, ok = TRUE, warn = FALSE,  skipNul = FALSE)
    
    # Remove non print data etc
    f <- iconv(f, "latin1", "ASCII", sub=" ");
    f <- gsub("[^[:alpha:][:space:][:punct:]]", "", f);
    
    for (j in seq(0, length(f), by=nbLinesLoad)) {
        print(paste(format(j,  scientific = F) ,format(min(j+nbLinesLoad,length(f)), scientific = F), sep= "-" ))
      
        fCorpus <- VCorpus(VectorSource(f[j:min(j+nbLinesLoad,length(f))]))
        fCorpus <- tm_map(fCorpus, content_transformer(tolower))
        fCorpus <- tm_map(fCorpus, removePunctuation)
        fCorpus <- tm_map(fCorpus, removeNumbers)
        fCorpus <- tm_map(fCorpus, stripWhitespace)
        
        # why not ?
        fCorpus <- tm_map(fCorpus, function(x)removeWords(x,stopwords()))
        
        #ngramification
        n  <- 1
        print(paste("n=",n,sep=""))
        t1 <- TermDocumentMatrix(fCorpus, control = list(tokenize = function(x) NGramTokenizer(x, Weka_control(min = n, max = n))))
        #n  <- 2
        #t2 <- TermDocumentMatrix(fCorpus, control = list(tokenize = function(x) NGramTokenizer(x, Weka_control(min = n, max = n))))
        #n  <- 3
        #t3 <- TermDocumentMatrix(fCorpus, control = list(tokenize = function(x) NGramTokenizer(x, Weka_control(min = n, max = n))))
        #n  <- 4
        #t4 <- TermDocumentMatrix(fCorpus, control = list(tokenize = function(x) NGramTokenizer(x, Weka_control(min = n, max = n))))

        print("freq")
        
        #slam::row_sums
        freq1 <- sort(row_sums(t1), decreasing=TRUE)
        
        #freq1 <- sort(rowSums(as.matrix(t1)), decreasing=TRUE)
        #freq2 <- sort(rowSums(as.matrix(t2)), decreasing=TRUE)
        #freq3 <- sort(rowSums(as.matrix(t3)), decreasing=TRUE)
        #freq4 <- sort(rowSums(as.matrix(t4)), decreasing=TRUE)
        print("w1")
        w1 <- data.table(terms=names(freq1), freq=freq1)
        #w2 <- data.table(terms=names(freq2), freq=freq2)
        #w3 <- data.table(terms=names(freq3), freq=freq3)
        #w4 <- data.table(terms=names(freq4), freq=freq4)
        
        pal = brewer.pal(9,"BuPu")
        wordcloud(words = w1$terms,
                  freq = w1$freq,
                  scale = c(3,.8),
                  random.order = F,
                  colors = pal,
                  max.words=20)
        
        save(t1, file=paste("t1_",listFiles[i],"_", format(j, scientific = F ),".RData", sep=""))
        #save(t1, file=paste("t1",listFiles[i], j,".RData", sep="_"))
        #save(t2, file="2.RData")
        #save(t3, file="3.RData")
        #save(t4, file="4.RData")
        pal = brewer.pal(9,"BuPu")
        wordcloud(words = w1$terms,
                  freq = w1$freq,
                  scale = c(3,.8),
                  random.order = F,
                  colors = pal,
                  max.words=20)
    }
}
print(Sys.time())

# Create the word cloud

# strsplit(trimws(laPhrase)," ")
