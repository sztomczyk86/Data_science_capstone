library(tidyverse)
library(cld2) # detect language
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
#library(sbo)

set.seed(666)

blog <- file("Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
news <- file("Coursera-SwiftKey/final/en_US/en_US.news.txt")
twitter <- file("Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
profanity <- read_lines("bad-words.txt")

#' sample only 0.5% of the blog text file
blog.txt <- readLines(blog)

sample.blog <- sample(1:length(blog.txt),round(length(blog.txt)/5))
blog.txt.sub <- blog.txt[sample.blog]

rm(blog.txt)

#' sample only 0.5% of the news text file
news.txt <- readLines(news)

sample.news <- sample(1:length(news.txt),round(length(news.txt)/5))

news.txt.sub <- news.txt[sample.news]

rm(news.txt)

#' sample only 0.5% of the twitter text file
twitter.txt <- readLines(twitter)

sample.twitter <- sample(1:length(twitter.txt),round(length(twitter.txt)/5))

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


c.all <- corpus(all.txt.sub)


#' create the corpus from the sampled text and tokenize it by words 
tk1 <- tokens(c.all, what = "word", remove_punct = T, remove_symbols = T, 
              remove_numbers = T, remove_url = T, remove_separators = T)

#' remove profanity
tk1 <- tokens_remove(tk1, pattern = profanity)

#' remove stopwords

tk1 <- tokens_select(tk1, pattern = stopwords("en"), selection = "remove")

#' create 2, 3, 4 and 5-ngrams
tk2 <- tokens_ngrams(tk1, n = 2)
tk3 <- tokens_ngrams(tk1, n = 3)
tk4 <- tokens_ngrams(tk1, n = 4)
tk5 <- tokens_ngrams(tk1, n = 5)

#' analyzing the frequency of n-grams 
tk1.freq <- as_tibble(textstat_frequency(dfm(tk1))) %>% 
        select(feature, frequency) %>% filter(frequency > 4)
tk2.freq <- as_tibble(textstat_frequency(dfm(tk2))) %>% 
        select(feature, frequency) %>% filter(frequency > 4)
tk3.freq <- as_tibble(textstat_frequency(dfm(tk3))) %>% 
        select(feature, frequency) %>% filter(frequency > 4)
tk4.freq <- as_tibble(textstat_frequency(dfm(tk4))) %>% 
        select(feature, frequency) %>% filter(frequency > 4)
tk5.freq <- as_tibble(textstat_frequency(dfm(tk5))) %>% 
        select(feature, frequency) %>% filter(frequency > 4)

# Stupid Backoff Model


rm(list = setdiff(ls(), c("tk1.freq","tk2.freq", "tk3.freq", "tk4.freq",
                          "tk5.freq","profanity")))
gc()


next.word.prediction <- function(phrase){
        
        ## tokenize the input phrase in the same way as the corpus
        tk.phrase <- tokens(phrase, what = "word", remove_punct = T, 
                            remove_symbols = T, remove_numbers = T, 
                            remove_url = T, remove_separators = T)
        
        tk.phrase <- tokens_select(tk.phrase, pattern = stopwords("en"),
                                   selection = "remove")
        
        tk.phrase <- tokens_remove(tk.phrase, pattern = profanity)
        
        to.match <- tokens_select(tk.phrase, startpos = (length(unlist((tk.phrase)))-2), 
                                  endpos = length(unlist(tk.phrase)))
        
        ## prepare the end bigram, trigram and 4-gram to use later as a query in 
        ## frequency tables
        tail_trigram <- paste(to.match[[1]][1], to.match[[1]][2],to.match[[1]][3],
                              sep = "_")
        tail_bigram <- paste(to.match[[1]][2], to.match[[1]][3], sep = "_")
        tail_unigram <- to.match[[1]][3] 
        
        ## find all the 4-grams starting with the tail trigram
        tk4.freq[grep(paste0("^", tail_trigram, "_"), tk4.freq$feature),] -> obs_4grams
        ## find the frequency of the tail bigram in the corpus
        tk3.freq[grep(paste0("^", tail_trigram, "$"), tk3.freq$feature),2] -> tail_trigram_fq
        ## calcualte the frequency of the observed 4-grams and put them in a tibble
        obs_4grams %>% mutate(word = str_split_fixed(obs_4grams$feature, "_",4)[,4],
                                S=(frequency/tail_trigram_fq$frequency)) %>% 
                select(word, S) -> word.prediction
        
        ## find all the trigrams starting with the tail bigram
        tk3.freq[grep(paste0("^", tail_bigram, "_"), tk3.freq$feature),] -> obs_trigrams
        ## find the frequency of the tail bigram in the corpus
        tk2.freq[grep(paste0("^", tail_bigram, "$"), tk2.freq$feature),2] -> tail_bigram_fq
        
        ## calcualte the frequency of the observed trigrams and put them in a tibble
        obs_trigrams %>% mutate(word = str_split_fixed(obs_trigrams$feature, "_",3)[,3],
                                S=(0.4*(frequency/tail_bigram_fq$frequency))) %>% 
                select(word, S) %>% bind_rows(word.prediction) -> word.prediction
        
        ## find all the bigrams starting with the tail unigram
        tk2.freq[grep(paste0("^", tail_unigram, "_"), tk2.freq$feature),] -> obs_bigrams
        ## find the frequency of the tail unigram in the corpus
        tk1.freq[grep(paste0("^", tail_unigram, "$"), 
                      tk1.freq$feature, useBytes = T),2] -> tail_unigram_fq
        
        ## calcualte the frequency of the observed bigrams and add them to the tibble
        obs_bigrams %>% mutate(word = str_split_fixed(obs_bigrams$feature, "_",2)[,2],
                               S=(0.4*0.4*(frequency/tail_unigram_fq$frequency))) %>% 
                select(word, S) %>% bind_rows(word.prediction) -> word.prediction
        
        ## look at the most frequent unigrams and calculate their frequency with the 
        ## backoff rate, add them to the tibble
        tk1.freq %>% slice(1:5) %>% mutate(word = feature, 
                                           S=(0.4*0.4*0.4*(frequency/sum(tk1.freq$frequency)))) %>% 
                select(word, S) %>% bind_rows(word.prediction) -> word.prediction
        ## sort the words that could potentially complete the phrase by the frequency
        ## and display only the top 5 candidates
        word.prediction %>% arrange(desc(S)) %>% distinct(word, .keep_all = T) %>% 
                slice(1:5) -> word.prediction        
        
        return(word.prediction)
}

q1 <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
next.word.prediction(q1)

q2 <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
next.word.prediction(q2)

q3 <- "Hey sunshine, can you follow me and make me the"
next.word.prediction(q3)

q4 <- "Very early observations on the Bills game: Offense still struggling but the"
next.word.prediction(q4)

q5 <- "Go on a romantic date at the"
next.word.prediction(q5)

q6 <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
next.word.prediction(q6)

q7 <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
next.word.prediction(q7)

q8 <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
next.word.prediction(q8)

q9 <- "Be grateful for the good times and keep the faith during the"
next.word.prediction(q9)

q10 <- "If this isn't the cutest thing you've ever seen, then you must be"
next.word.prediction(q10)

tk.phrase <- tokens(q6, what = "word", remove_punct = T, 
                    remove_symbols = T, remove_numbers = T, 
                    remove_url = T, remove_separators = T)

tk.phrase <- tokens_select(tk.phrase, pattern = stopwords("en"),
                          selection = "remove")

tk.phrase <- tokens_remove(tk.phrase, pattern = profanity)
   
to.match <- tokens_select(tk.phrase, startpos = (length(unlist((tk.phrase)))-1), 
                                     endpos = length(unlist(tk.phrase)))
tail_bigram <- paste(to.match[[1]][1], to.match[[1]][2], sep = "_")
tail_unigram <- to.match[[1]][2]      

tk3.freq[grep(paste0("^", tail_bigram, "_"), tk3.freq$feature),] -> obs_trigrams
tk2.freq[grep(paste0("^", tail_bigram, "$"), tk2.freq$feature),2] -> tail_bigram_fq

obs_trigrams %>% mutate(word = str_split_fixed(obs_trigrams$feature, "_",3)[,3],
                        S=(frequency/tail_bigram_fq$frequency)) %>% 
                select(word, S) -> word.prediction

tk2.freq[grep(paste0("^", tail_unigram, "_"), tk2.freq$feature),] -> obs_bigrams
tk1.freq[grep(paste0("^", tail_unigram, "$"), 
              tk1.freq$feature, useBytes = T),2] -> tail_unigram_fq


obs_bigrams %>% mutate(word = str_split_fixed(obs_bigrams$feature, "_",2)[,2],
                        S=(0.4*(frequency/tail_unigram_fq$frequency))) %>% 
        select(word, S) %>% bind_rows(word.prediction) -> word.prediction

tk1.freq %>% slice(1:5) %>% mutate(word = feature, 
                                    S=(0.4*0.4*(frequency/sum(tk1.freq$frequency)))) %>% 
        select(word, S) %>% bind_rows(word.prediction) -> word.prediction

word.prediction %>% arrange(desc(S)) %>% distinct(word, .keep_all = T) %>% 
        slice(1:5) -> word.prediction




              