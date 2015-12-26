en_US <- file("D:/_MOOC_/_coursera_/10 project/final/en_US/en_US.twitter.txt")
open(en_US);
t <- file("D:/_MOOC_/git/Capston_Project/head.twitter.txt",'a')
current.line <- 1
max<-0;
while (length(line <- readLines(en_US, n = 1, warn = FALSE)) > 0) {
  if (max<nchar(line))  {max <- nchar(line)}
  
  if  (current.line <= 5) {
    writeLines(line,con=t, sep = '\n' )
    print (line)
  }
  
  current.line <- current.line + 1
} 
close(en_US)
close(t)

max





en_US <-  file("D:/_MOOC_/_coursera_/10 project/final/en_US/en_US.news.txt")
open(en_US);
current.line <- 1
while (length(line <- readLines(en_US, n = 1, warn = FALSE)) > 0) {
  if (max<nchar(line)){max <- nchar(line)}
  current.line <- current.line + 1
} 
close(en_US)

max 

en_US <- file("D:/_MOOC_/_coursera_/10 project/final/en_US/en_US.blogs.txt")
open(en_US);
current.line <- 1
while (length(line <- readLines(en_US, n = 1, warn = FALSE)) > 0) {
  if (max<nchar(line))  {max <- nchar(line)}
  current.line <- current.line + 1
} 
close(en_US)

max
