#' pre process the SwiftKey text to test the accuracy of the prediction 
#' algorithm

library(tidyverse)
library(cld2) # detect language
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(glue)

set.seed(666)

blog <- file("Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
news <- file("Coursera-SwiftKey/final/en_US/en_US.news.txt")
twitter <- file("Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
profanity <- read_lines("bad-words.txt")

#' sample 1/3 of the blog text file then sample 1000 texts in the portion not 
#' taken to build the model
blog.txt <- readLines(blog)

sample.blog <- sample(1:length(blog.txt),round(length(blog.txt)*0.75))
blog.txt.sub <- blog.txt[-sample.blog]
sample.blog <- sample(1:length(blog.txt.sub),1000)
blog.txt.sub <- blog.txt.sub[sample.blog]

rm(blog.txt)

#' sample 1/3 of the news text file then sample 1000 texts in the portion not 
#' taken to build the model
news.txt <- readLines(news)

sample.news <- sample(1:length(news.txt),round(length(news.txt)*0.75))

news.txt.sub <- news.txt[-sample.news]

sample.news <- sample(1:length(news.txt.sub),1000)
news.txt.sub <- news.txt.sub[sample.news]
rm(news.txt)

#' sample 1/3 of the twitter text file then sample 1000 texts in the portion not 
#' taken to build the model
twitter.txt <- readLines(twitter)

sample.twitter <- sample(1:length(twitter.txt),round(length(twitter.txt)*0.75))

twitter.txt.sub <- twitter.txt[-sample.twitter]

sample.twitter <- sample(1:length(twitter.txt.sub),1000)
twitter.txt.sub <- twitter.txt.sub[sample.twitter]
rm(twitter.txt)

#' detect the language of each line in the text file and remove the non-english 
#' lines
lng1 <- detect_language(blog.txt.sub) 
lng2 <- detect_language(news.txt.sub) 
lng3 <- detect_language(twitter.txt.sub) 

#' keep only lines marked as english

blog.txt.sub <- blog.txt.sub[lng1 =="en"]
news.txt.sub <- news.txt.sub[lng2 =="en"]
twitter.txt.sub <- twitter.txt.sub[lng3 =="en"]

#' combine the subsamples of different input files into one corpus for further 
#' analysis

blog.txt.sub <- data.frame(text = blog.txt.sub, source = "blog")
news.txt.sub <- data.frame(text = news.txt.sub, source = "news")
twitter.txt.sub <- data.frame(text = twitter.txt.sub, source = "twitter")

blog.txt.sub %>% bind_rows(news.txt.sub, twitter.txt.sub) -> all.txt.sub

rm(list=c("blog.txt.sub", "news.txt.sub", "twitter.txt.sub"))

all.txt.sub %>% group_by(source) %>% sample_n(100) -> sample.txt

c.test <- corpus(sample.txt)
c.test <- corpus_reshape(c.test, to = "sentences")

tk1.test <- tokens(c.test, what = "word", remove_punct = TRUE, 
                   remove_symbols = TRUE, 
                   remove_numbers = TRUE, 
                   remove_url = TRUE, 
                   remove_separators = TRUE)

rm(list=setdiff(ls(),"tk1.test"))

tk1.test <- tk1.test[sapply(tk1.test, length) > 1]

source("capstone_algorithm.R")

set.seed(666)

for (n in 1:1){
        if (n == 1){ 
                
                accuracy.measure <- tibble(trial=c(1:10), 
                                           accuracy.t1 = 0,
                                           accuracy.t3 = 0)
                
        }
        for (i in 1:length(tk1.test)){
                
                if (i == 1){
                        
                        sentences.TC <- data.frame(sentence = vector(),
                                                   correct.word = vector(),
                                                   prediction.w1 = vector(),
                                                   prediction.w2 = vector(),
                                                   prediction.w3 = vector())
                }
                
                sent.lght <- length(tk1.test[[i]])
                end.pos <- sample(1:(length(tk1.test[[i]])-1),1)
                
                sentences.TC[i,1] <- glue_collapse(as.character(
                        tokens_select(tk1.test[i], startpos = 1,
                                      endpos = end.pos)), sep = " ")
                
                sentences.TC[i,2] <- tk1.test[[i]][end.pos+1]
                
                
        }
        
        for (i1 in 1:nrow(sentences.TC)){
                
                
                sentences.TC [i1,3:5] <- next.word.prediction(sentences.TC[i1,1])[1:3,1]
                
                
        }
        
        
        sentences.TC %>% mutate(correct.t1 = (correct.word == prediction.w1),
                                correct.t2 = (correct.word == prediction.w2),
                                correct.t3 = (correct.word == prediction.w3)) %>%
                mutate(correct.prediction.t3 = (correct.t1==TRUE|
                                                        correct.t2==TRUE|
                                                        correct.t3==TRUE)) -> sentences.TC
        
        accuracy.measure[n,2] <- sum(sentences.TC$correct.t1/nrow(sentences.TC)*100)
        accuracy.measure[n,3] <- sum(sentences.TC$correct.prediction.t3/nrow(sentences.TC)*100)
        
        
}

#accuracy.measure[9,2] <- sum(sentences.TC$correct.t1/nrow(sentences.TC)*100)
#accuracy.measure[9,3] <- sum(sentences.TC$correct.prediction.t3/nrow(sentences.TC)*100)

#write.csv(accuracy.measure, "0.33_without_stop_words.csv", row.names = F)
#write.csv(accuracy.measure, "0.33_with_stop_words.csv", row.names = F)


