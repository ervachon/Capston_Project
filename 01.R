source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
install.packages("tm")
install.packages("RWeka")
install.packages("devtools")
install.packages("wordcloud")

# attention 64bit : jre dans c:\\programmes et non x86
library(rJava)
library(NLP)
library(RWeka)
library(tm)
library(data.table)
library(wordcloud)

#16Go ?
#memory.limit() 
n5_gram <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
n4_gram <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
n3_gram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
n2_gram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
n1_gram <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
n_gram  <- function(x,n) NGramTokenizer(x, Weka_control(min = n, max = n))

myFile <- "D:/_MOOC_/_coursera_/10 project/final/en_US/en_US.twitter.txt"
f  <- readLines(myFile, n = -1L, ok = TRUE, warn = FALSE,  skipNul = FALSE)

# Remove non print data etc
f <- iconv(f, "latin1", "ASCII", sub=" ");
f <- gsub("[^[:alpha:][:space:][:punct:]]", "", f);

# ici tres gros !
fCorpus <- VCorpus(VectorSource(f[1:10000]))
fCorpus <- tm_map(fCorpus, content_transformer(tolower))
fCorpus <- tm_map(fCorpus, removePunctuation)
fCorpus <- tm_map(fCorpus, removeNumbers)
fCorpus <- tm_map(fCorpus, stripWhitespace)

# ????
fCorpus <- tm_map(fCorpus, function(x)removeWords(x,stopwords()))

t <- TermDocumentMatrix(fCorpus, control = list(tokenize = n1_gram))

freq <- sort(rowSums(as.matrix(t)), decreasing=TRUE)
w <- data.table(terms=names(freq), freq=freq)

setwd('D:\\_MOOC_\\git\\Capston_Project\\data.tmp')
save(t, file="1.RData")

# Create the word cloud
pal = brewer.pal(9,"BuPu")
wordcloud(words = w$terms,
          freq = w$freq,
          scale = c(3,.8),
          random.order = F,
          colors = pal,
          max.words=20)

wordcloud(w$terms,w$freq,c(8,.3),2,,TRUE,TRUE,.15,pal)

# strsplit(trimws(a)," ")

#t <- removeSparseTerms(t, 0.5)
