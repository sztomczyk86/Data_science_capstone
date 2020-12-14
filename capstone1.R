library(tidyverse)
library(caret)
library(tokenizers)
library(cld2) # detect language
library(stringr)
library(quanteda)
library(text2vec)
library(tidytext)
library(sentimentr) # remove profanity
library(stringi)


set.seed(666)


blog <- file("Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
news <- file("Coursera-SwiftKey/final/en_US/en_US.news.txt")
twitter <- file("Coursera-SwiftKey/final/en_US/en_US.twitter.txt")

#' sample only 5% of the blog text file
blog.txt <- readLines(blog)

sample.blog <- sample(1:length(blog.txt),round(length(blog.txt)/20))

blog.txt.sub <- blog.txt[sample.blog]

rm(blog.txt)

#' sample only 5% of the news text file
news.txt <- readLines(news)

sample.news <- sample(1:length(news.txt),round(length(news.txt)/20))

news.txt.sub <- news.txt[sample.news]

rm(news.txt)

#' sample only 5% of the twitter text file
twitter.txt <- readLines(twitter)

sample.twitter <- sample(1:length(twitter.txt),round(length(twitter.txt)/20))

twitter.txt.sub <- twitter.txt[sample.twitter]

rm(twitter.txt)

#' detect the language of each line in the text file and remove the non-english 
#' lines
lng1 <- detect_language(blog.txt.sub) 

#' verify the accuracy of the detection 
blog.txt.sub[lng1 !="en"| is.na(lng1)]
#' keep only lines marked as english

blog.txt.sub <- blog.txt.sub[lng1 =="en"]

#' remove profanity
profanity<-profanity(blog.txt.sub)

profanity %>% group_by(element_id) %>% 
        summarise(sum=sum(profanity_count)) -> profanity_count 
        
blog.txt.sub <- blog.txt.sub[!(profanity_count$sum > 0)]

rm(profanity)
rm(profanity_count)

#' create the corpus from the sampled text and tokenize it by words or 2 and 
#' 3-ngrams
tk1 <- tokenize_words(corpus(blog.txt.sub), strip_numeric = TRUE)
tk2 <- tokenize_ngrams(corpus(blog.txt.sub), n = 2)
tk3 <- tokenize_ngrams(corpus(blog.txt.sub), n = 3)


tk1 <- lapply(tk1, gsub, pattern = "[^[:alnum:]' ]", replacement = "") 
tk1 <- lapply(tk1, str_squish) 
tk1 <- lapply(tk1, stri_trans_general, id="latin-ascii")
tk1 <- lapply(tk1, iconv, from="latin1",to="ASCII", sub="")


word_freq <- data.frame(sort(table(unlist(tk1)), decreasing = TRUE))
#word_freq_2 <- sort(table(unlist(tk2)), decreasing = TRUE)
#word_freq_3 <- sort(table(unlist(tk3)), decreasing = TRUE)

#' how many unique words are needed to cover 50% or 90% of word instances
#' in the corpora

word_freq %>% mutate(cumsum=cumsum(Freq)) -> word_freq

max(which(word_freq$cumsum < round(sum(word_freq$Freq)*.5)))
max(which(word_freq$cumsum < round(sum(word_freq$Freq)*.9)))
max(which(word_freq$Freq > 1))

dfm(tk1)

it1 <- itoken(blog.txt.sub, tolower, word_tokenizer, n_chunks = 10)
voc1 <- create_vocabulary(it1, stopwords = character(0))
voc1 <- voc1[voc1$term_count > 5,]
nums1 <- grepl("[0-9]", voc1$term)
voc1 <- voc1[!nums1,]
vector1 <- vocab_vectorizer(vocabulary = voc1)

voc2 <- create_vocabulary(it1, ngram = c(ngram_min =1, ngram_max=3))
 
tcm1 <- create_tcm(it1, vector1)

dtm1 <- create_dtm(it1, vector1)

bla1 <- corpus(blog.txt.sub)
docfreq(dfm(bla1))

