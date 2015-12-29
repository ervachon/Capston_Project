#setwd('D:\\_MOOC_\\git\\Capston_Project\\')
#source('optimize.R')

library(rJava)
library(NLP)
library(RWeka)
library(tm)
library(data.table)
library(wordcloud)
library(slam) #slam::row_sums

minOcc <- c(100,30,20,10)
directory = "D:\\_MOOC_\\git\\Capston_Project\\data\\"
nbGram <- 4

for (i in 1:nbGram){
  load(paste(directory,"w",i,".RData",sep=""))
  #optimize the size  
  
  save(paste(directory,"w",i,"_opt.RData",sep=""))  
}
