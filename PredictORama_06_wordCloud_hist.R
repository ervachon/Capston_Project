############################################
# setwd('D:\\_GIT_\\Capston_Project\\')
# source('PredictORama_06_wordCloud_hist.R')
############################################

# scriptName
scriptName = "PredictORama_06_wordCloud_hist.R"
# load comvmon script
source("PredictORama_00_common.R")

tStart <-Sys.time()
print(paste(tStart,"> DEBUT",scriptName))

# Main ###############################################################################

pal = brewer.pal(9,"BuPu")

for (i in 1:nbGram) { 
  load(paste(directoryData,"w",i,"_opt_",minOcc[i],".RData",sep=""))
  a<- w[1:50,]
  rownames(a) <- 1:nrow(a)
  a$terms <- factor(a$terms, levels = a$terms[order(a$freq,decreasing=T)])
  
  png(filename=paste(directoryImages,i,"gram.png",sep=""), width=3.5, height=3.5, units="in", res=125)
  wordcloud(words = w$terms,
            freq = w$freq,
            scale=c(5-i,0.35), 
            max.words=100, 
            random.order=FALSE, 
            rot.per=0.5, 
            use.r.layout=FALSE, 
            colors=brewer.pal(8,"Dark2"))
  dev.off()
  
  ggplot(a, aes(x=terms,y= freq)) +
    geom_bar(stat = "identity", aes(fill = terms)) +
    theme(legend.position="none",axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave(filename=paste(directoryImages,i,"gram_dist.png",sep=""),width=5, height=5, units="in", dpi=100)

} 

# end main ###########################################################################
tEnd <-Sys.time()
print(paste(tEnd,"end",sep=" > ",scriptName))
print(tEnd-tStart)
