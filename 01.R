source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
install.packages("tm")
install.packages("RWeka")
install.packages("devtools")

library("RWeka")
library("tm")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

myFile <- "D:/_MOOC_/_coursera_/10 project/final/en_US/en_US.twitter.txt"

# monTexte <- read.table(myFile,sep="\n")
monTexte <- scan(myFile,sep = "\n", what="")
names(monTexte) <- "monTexte"

myCorpus <- Corpus(VectorSource(monTexte))
myStopwords <- c(stopwords('english'),"originally", "posted")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

tdmTri <- TermDocumentMatrix(myCorpus, control = list(tokenize = TrigramTokenizer))
inspect(tdm2[0:20,1:10])
plot(tdm2, terms = findFreqTerms(tdm2, lowfreq = 2)[1:50], corThreshold = 0.5)

current.line <- 1
while (length(line <- readLines(en_US, n = 1, warn = FALSE)) > 0) {
  if (max<nchar(line)){max <- nchar(line)}
  current.line <- current.line + 1
} 
close(en_US)

