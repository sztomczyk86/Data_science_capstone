#' pre process the SwiftKey text into a corpus

library(tidyverse)
library(cld2) # detect language
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)


set.seed(666)

blog <- file("Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
news <- file("Coursera-SwiftKey/final/en_US/en_US.news.txt")
twitter <- file("Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
profanity <- read_lines("bad-words.txt")

#' sample only 0.5% of the blog text file
blog.txt <- readLines(blog)

sample.blog <- sample(1:length(blog.txt),round(length(blog.txt)*0.7))
blog.txt.sub <- blog.txt[sample.blog]

rm(blog.txt)

#' sample only 0.5% of the news text file
news.txt <- readLines(news)

sample.news <- sample(1:length(news.txt),round(length(news.txt)*0.7))

news.txt.sub <- news.txt[sample.news]

rm(news.txt)

#' sample only 0.5% of the twitter text file
twitter.txt <- readLines(twitter)

sample.twitter <- sample(1:length(twitter.txt),round(length(twitter.txt)*0.7))

twitter.txt.sub <- twitter.txt[sample.twitter]

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

c.all <- corpus(all.txt.sub)
c.all <- corpus_reshape(c.all, to = "sentences")

rm(all.txt.sub)

#' create the corpus from the sampled text and tokenize it by words 
tk1 <- tokens(c.all, what = "word", remove_punct = TRUE, 
              remove_symbols = TRUE, 
              remove_numbers = TRUE, 
              remove_url = TRUE, 
              remove_separators = TRUE)

#' remove profanity
tk1 <- tokens_remove(tk1, pattern = profanity)

#' remove stopwords

#tk1 <- tokens_select(tk1, pattern = stopwords("en"), selection = "remove")

#' create 2, 3, 4 and 5-ngrams and analyze their frequency

tk1.dfm <- dfm(tk1)
tk1.freq <- as_tibble(textstat_frequency(tk1.dfm)) %>% 
        select(feature, frequency) %>% filter(frequency > 4)

rm(tk1.dfm)

tk2 <- tokens_ngrams(tk1, n = 2)
tk2.dfm <- dfm(tk2)
rm(tk2)

tk2.freq <- as_tibble(textstat_frequency(tk2.dfm)) %>% 
        select(feature, frequency) %>% filter(frequency > 4)
rm(tk2.dfm)

tk3 <- tokens_ngrams(tk1, n = 3)
tk3.dfm <- dfm(tk3)
rm(tk3)
tk3.freq <- as_tibble(textstat_frequency(tk3.dfm)) %>% 
        select(feature, frequency) %>% filter(frequency > 4)
rm(tk3.dfm)

tk4 <- tokens_ngrams(tk1, n = 4)
tk4.dfm <- dfm(tk4)
rm(tk4)
tk4.freq <- as_tibble(textstat_frequency(tk4.dfm)) %>% 
        select(feature, frequency) %>% filter(frequency > 4)
rm(tk4.dfm)

tk5 <- tokens_ngrams(tk1, n = 5)
tk5.dfm <- dfm(tk5)
rm(tk5)
tk5.freq <- as_tibble(textstat_frequency(tk5.dfm)) %>% 
        select(feature, frequency) %>% filter(frequency > 4)
rm(tk1)
rm(tk5.dfm)


# Stupid Backoff Model


rm(list = setdiff(ls(), c("tk1.freq","tk2.freq", "tk3.freq", "tk4.freq",
                          "tk5.freq","profanity")))
gc()

# write.csv(tk1.freq, "tk1.freq.csv", row.names = F)
# write.csv(tk2.freq, "tk2.freq.csv", row.names = F)
# write.csv(tk3.freq, "tk3.freq.csv", row.names = F)
# write.csv(tk4.freq, "tk4.freq.csv", row.names = F)
# write.csv(tk5.freq, "tk5.freq.csv", row.names = F)

rm(list=ls())