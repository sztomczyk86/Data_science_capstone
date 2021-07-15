#' build the prediction alogorthm and test it


library(tidyverse)
library(cld2) # detect language
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)


set.seed(666)

# load the pre-processed frequency tables
if (!exists("tk1.freq")){
        
        tk1.freq <- read.csv("tk1.freq.csv")
        tk2.freq <- read.csv("tk2.freq.csv")
        tk3.freq <- read.csv("tk3.freq.csv")
        tk4.freq <- read.csv("tk4.freq.csv")
        tk5.freq <- read.csv("tk5.freq.csv")
        
}

profanity <- read_lines("bad-words.txt")

next.word.prediction <- function(phrase){
        word.prediction <- tibble(word = NA, S=NA)
        ## tokenize the input phrase in the same way as the corpus
        tk.phrase <- tokens(phrase, what = "word",
                            remove_punct = TRUE,
                            remove_symbols = TRUE,
                            remove_numbers = TRUE,
                            remove_url = TRUE,
                            remove_separators = TRUE)
        # tk.phrase <- tokens_select(tk.phrase, pattern = stopwords("en"),
        #                            selection = "remove")
        tk.phrase <- tokens_remove(tk.phrase, pattern = profanity)
        if (length(tk.phrase[[1]]) >= 4){
                to.match <- tokens_select(tk.phrase, startpos = (length(unlist((tk.phrase)))-3),
                                          endpos = length(unlist(tk.phrase)))
        } else if (length(tk.phrase[[1]]) == 3){
                to.match <- tokens_select(tk.phrase, startpos = (length(unlist((tk.phrase)))-2),
                                          endpos = length(unlist(tk.phrase)))
        } else if (length(tk.phrase[[1]]) == 2){
                to.match <- tokens_select(tk.phrase, startpos = (length(unlist((tk.phrase)))-1),
                                          endpos = length(unlist(tk.phrase)))
        } else  if (length(tk.phrase[[1]]) == 1){
                to.match <- tokens_select(tk.phrase, startpos = (length(unlist((tk.phrase)))),
                                          endpos = length(unlist(tk.phrase)))
        }
        ## prepare the end bigram, trigram, 4 and 5-gram to use later as a query
        ## in frequency tables
        if (length(tk.phrase[[1]]) >= 4){
                tail_4gram <- paste(to.match[[1]][length(to.match[[1]])-3],
                                    to.match[[1]][length(to.match[[1]])-2],
                                    to.match[[1]][length(to.match[[1]])-1],
                                    to.match[[1]][length(to.match[[1]])], sep = "_")
                ## find all the 5-grams starting with the tail 4gram
                tk5.freq[grep(paste0("^", tail_4gram, "_"), tk5.freq$feature),] -> obs_5grams
                ## find the frequency of the tail bigram in the corpus
                tk4.freq[grep(paste0("^", tail_4gram, "$"), tk4.freq$feature),2] -> tail_4gram_fq
                ## calcualte the frequency of the observed 4-grams and put them in a tibble
                obs_5grams %>% mutate(word = str_split_fixed(obs_5grams$feature, "_",5)[,5],
                                      S=(frequency/tail_4gram_fq)) %>%
                        select(word, S) -> word.prediction
        }
        if (length(tk.phrase[[1]]) >= 3){
                tail_trigram <- paste(to.match[[1]][length(to.match[[1]])-2],
                                      to.match[[1]][length(to.match[[1]])-1],
                                      to.match[[1]][length(to.match[[1]])],
                                      sep = "_")
                ## find all the 4-grams starting with the tail trigram
                tk4.freq[grep(paste0("^", tail_trigram, "_"), tk4.freq$feature),] -> obs_4grams
                ## find the frequency of the tail bigram in the corpus
                tk3.freq[grep(paste0("^", tail_trigram, "$"), tk3.freq$feature),2] -> tail_trigram_fq
                ## calcualte the frequency of the observed 4-grams and put them in a tibble
                obs_4grams %>% mutate(word = str_split_fixed(obs_4grams$feature, "_",4)[,4],
                                      S=(0.4*(frequency/tail_trigram_fq))) %>%
                        select(word, S) %>% bind_rows(word.prediction) -> word.prediction
        }
        if (length(tk.phrase[[1]]) >= 2){
                tail_bigram <- paste(to.match[[1]][length(to.match[[1]])-1],
                                     to.match[[1]][length(to.match[[1]])], sep = "_")
                ## find all the trigrams starting with the tail bigram
                tk3.freq[grep(paste0("^", tail_bigram, "_"), tk3.freq$feature),] -> obs_trigrams
                ## find the frequency of the tail bigram in the corpus
                tk2.freq[grep(paste0("^", tail_bigram, "$"), tk2.freq$feature),2] -> tail_bigram_fq
                ## calcualte the frequency of the observed trigrams and put them in a tibble
                obs_trigrams %>% mutate(word = str_split_fixed(obs_trigrams$feature, "_",3)[,3],
                                        S=(0.4*0.4*(frequency/tail_bigram_fq))) %>%
                        select(word, S) %>% bind_rows(word.prediction) -> word.prediction
        }
        if (length(tk.phrase[[1]]) >= 1){
                tail_unigram <- to.match[[1]][length(to.match[[1]])]
                ## find all the bigrams starting with the tail unigram
                tk2.freq[grep(paste0("^", tail_unigram, "_"), tk2.freq$feature),] -> obs_bigrams
                ## find the frequency of the tail unigram in the corpus
                tk1.freq[grep(paste0("^", tail_unigram, "$"),
                              tk1.freq$feature, useBytes = T),2] -> tail_unigram_fq
        }
        ## calcualte the frequency of the observed bigrams and add them to the tibble
        obs_bigrams %>% mutate(word = str_split_fixed(obs_bigrams$feature, "_",2)[,2],
                               S=(0.4*0.4*0.4*(frequency/tail_unigram_fq))) %>%
                select(word, S) %>% bind_rows(word.prediction) -> word.prediction
        ## look at the most frequent unigrams and calculate their frequency with the
        ## backoff rate, add them to the tibble
        tk1.freq %>% slice(1:5) %>% mutate(word = feature,
                                           S=(0.4*0.4*0.4*0.4*(frequency/sum(tk1.freq$frequency)))) %>%
                select(word, S) %>% bind_rows(word.prediction) -> word.prediction
        ## sort the words that could potentially complete the phrase by the frequency
        ## and display only the top 5 candidates
        word.prediction %>% arrange(desc(S)) %>% distinct(word, .keep_all = T) %>%
                slice(1:5) -> word.prediction
        return(word.prediction)
}

# q1 <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
# next.word.prediction(q1)
# 
# q2 <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
# next.word.prediction(q2)
# 
# q3 <- "Hey sunshine, can you follow me and make me the"
# next.word.prediction(q3)
# 
# q4 <- "Very early observations on the Bills game: Offense still struggling but the"
# next.word.prediction(q4)
# 
# q5 <- "Go on a romantic date at the"
# next.word.prediction(q5)
# 
# q6 <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
# next.word.prediction(q6)
# 
# q7 <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
# next.word.prediction(q7)
# 
# q8 <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
# next.word.prediction(q8)
# 
# q9 <- "Be grateful for the good times and keep the faith during the"
# next.word.prediction(q9)
# 
# q10 <- "If this isn't the cutest thing you've ever seen, then you must be"
# next.word.prediction(q10)


