#############################################################
#
# source("http://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
# install.packages("tm")
# install.packages("RWeka")
# install.packages("devtools")
# install.packages("wordcloud")
# install.packages("rJava")
# install.packages("NLP")
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("slam") 
# install.packages("parallel")
# install.packages("foreach")
# install.packages("doParallel")
#
# attention 64bit : jre 64bit too 
# => in c:\\programmes a  and not in  x86
# 
#############################################################
#
# setwd('D:\\_GIT_\\Capston_Project\\')
#
#############################################################

# clean stop words
#cleanStopWord <- TRUE
cleanStopWord <- FALSE

# type of data
returnTypeData <- function(){
   t <- "" 
   if (cleanStopWord==TRUE){t <- "_noStopWords"}
   return(t)
}

typeData <- returnTypeData()

#############################################################

# libraries
library(rJava)
library(NLP)
library(RWeka)
library(tm)
library(data.table)
library(wordcloud)
library(ggplot2)
library(slam) 
library(parallel)
library(foreach)
library(doParallel)

#############################################################

# path 
directoryRawData   <- "D:\\_MOOC_\\_coursera_\\10 project\\final\\en_US\\"
directoryDataTmp   <- paste("D:\\_GIT_\\Capston_Project\\data",typeData,".tmp\\",sep="")
directoryData      <- paste("D:\\_GIT_\\Capston_Project\\data",typeData,"\\",sep="")
directoryDataFinal <- paste("D:\\_GIT_\\Capston_Project\\data",typeData,".final\\",sep="")
directoryImages    <- paste("D:\\_GIT_\\Capston_Project\\images",typeData,"\\",sep="")

# sampling
sample <- 1 # if sampling 0 to 1

# parallization
no_cores <- detectCores() - 1

# # of ngram
nbGram <- 4

# # of line to load
nbLinesLoad<-200000

#   optimize size of dump
minOcc <- c(1,3,4,4)

# # of res
nbRes <- 3

#############################################################

# list all txt files of directory
returnListeFiles <- function(directory){
  listTmp <- c()
  files <- list.files(pattern = paste('^.*txt$',sep=''),path=directoryRawData)
  for (f in files){  
    listTmp<-c(listTmp,f)
  }
  return (listTmp)
} 

# gramization
nGramMatrix <- function (y,n) { 
  print(Sys.time())
  print(paste("n=",n,sep=""))
  TermDocumentMatrix(y, control = list(tokenize = function(x) NGramTokenizer(x, Weka_control(min = n, max = n))))
}

# merge data
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
    }
  wTmp
}

# create directory if not existe
createDirectoryIfNotExist <- function(directory){
  dir.create(file.path(directory))
}

options(warn=-1)
# test and create directory :
createDirectoryIfNotExist(directoryDataTmp)
createDirectoryIfNotExist(directoryData)
createDirectoryIfNotExist(directoryDataFinal)
createDirectoryIfNotExist(directoryImages)
options(warn=0)