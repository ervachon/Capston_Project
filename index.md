---
title       : Coursera Data Science Specialization
subtitle    : "Predict next Word"
author      : Eric VACHON
job         : December 2015
logo        : logo.jpg
framework   : io2012   # {io2012, html5slides, shower, dzslides, deckjs...}
widgets     : [bootstrap,quiz]   # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
--- 
<!--
https://support.rstudio.com/hc/en-us/articles/205002917-SSL-certificate-problem-when-publishing-to-RPubs
file.edit('~/.Rprofile') **<--options(rpubs.upload.method = "internal")**
library(slidify)
setwd("D:\\_MOOC_\\git\\Capston_Project")
publish(title = 'Coursera Capston Project', 'index.html', host = 'rpubs') 
-->

## Aim of the capston project.
<br>
The aim of this project is to use 3 dataset from .... to create a algorithm to predict the next word of a sentence.  
<br>
This project is the capston project of the coursera data science specialisation.  
<br>

--- .class #id 

## Download, load and summary the data source.
The data source :
- en_US.blogs.txt (210160014 bytes,  899288 lines)
- en_US.news.txt (205811889 bytes, 1010242 lines)
- en_US.twitter.txt (167105338 bytes, 2360148 lines)  
> head en_US.twitter.txt > head.twitter.txt :  

```r
t <- file("head.twitter.txt"); open(t)
while (length(line <- readLines(t, n = 1, warn = FALSE)) > 0) {print (line)} 
```

```
## [1] "How are you? Btw thanks for the RT. You gonna be in DC anytime soon? Love to see you. Been way, way too long."
## [1] "When you meet someone special... you'll know. Your heart will beat more rapidly and you'll smile for no reason."
## [1] "they've decided its more fun if I don't."
## [1] "So Tired D; Played Lazer Tag & Ran A LOT D; Ughh Going To Sleep Like In 5 Minutes ;)"
## [1] "Words from a complete stranger! Made my birthday even better :)"
```

```r
close(t) 
```


--- .class #id 

## Clean data.

1. Load data
2. lower case the data
3. Remove 
   a. whitespace
   b. sign
4. tokenize   


--- .class #id  

## Algorithms.

use n-gram / backoff model

2-gram
3-gram


--- .class #id  

## Shiny app. 



--- .class #id  

## Links

- [My Shiny App will be here](https://ervachon.shinyapps.io/Capston_Project)  
- [My Github](https://github.com/ervachon/Capston_Project)  
- [Coursera link](https://class.coursera.org/dsscapstone-006)
