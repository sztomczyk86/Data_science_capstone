library(tidyverse)
library(cld2) # detect language
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(wordcloud)

set.seed(666)


blog <- file("Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
news <- file("Coursera-SwiftKey/final/en_US/en_US.news.txt")
twitter <- file("Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
profanity <- read_lines("bad-words.txt")

gc()

#' sample only 0.5% of the blog text file
blog.txt <- readLines(blog)

sample.blog <- sample(1:length(blog.txt),round(length(blog.txt)/20))
blog.txt.sub <- blog.txt[sample.blog]

rm(blog.txt)

#' sample only 0.5% of the news text file
news.txt <- readLines(news)

sample.news <- sample(1:length(news.txt),round(length(news.txt)/20))

news.txt.sub <- news.txt[sample.news]

rm(news.txt)

#' sample only 0.5% of the twitter text file
twitter.txt <- readLines(twitter)

sample.twitter <- sample(1:length(twitter.txt),round(length(twitter.txt)/20))

twitter.txt.sub <- twitter.txt[sample.twitter]

rm(twitter.txt)

#' detect the language of each line in the text file and remove the non-english 
#' lines
lng1 <- detect_language(blog.txt.sub) 
lng2 <- detect_language(news.txt.sub) 
lng3 <- detect_language(twitter.txt.sub) 

#' verify the accuracy of the detection 
blog.txt.sub[lng1 !="en"| is.na(lng1)]
news.txt.sub[lng2 !="en"| is.na(lng2)]
twitter.txt.sub[lng3 !="en"| is.na(lng3)]

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
 
#' create 2 and 3-ngrams
tk2 <- tokens_ngrams(tk1, n = 2)
tk3 <- tokens_ngrams(tk1, n = 3)

#' look at the word and ngram frequency
tk1.dfm <- dfm(tk1)
dfm_weight(tk1.dfm, scheme  = "prop")
par(mfrow=c(1,3))
textplot_wordcloud(tk1.dfm, max_words = 100)
textstat_frequency(tk1.dfm)

tk2.dfm <- dfm(tk2)
textplot_wordcloud(tk2.dfm, max_words = 100)

tk3.dfm <- dfm(tk3)
textplot_wordcloud(tk3.dfm, max_words = 100)


#' analyzing the frequency of n-grams and calculating the adjusted frequency 
#' using Good-Turing Discounting
tk1.freq <- textstat_frequency(tk1.dfm)
tk2.freq <- textstat_frequency(tk2.dfm)
tk3.freq <-textstat_frequency(tk3.dfm)


#' calculate the Good-Turing smoothed frequencies for 2-grams

# remove the n-grams that have frequency of 1
#tk2.freq <- tk2.freq[tk2.freq$frequency>1,]

#' prepare frequency of frequency tables
# tk1.fq.fq <- as_tibble(table(tk1.freq$frequency), .name_repair = "minimal") 
# names(tk1.fq.fq) <- c("r", "Nr")
# tk1.fq.fq$r <- as.numeric(tk1.fq.fq$r)
# 
# tk2.fq.fq <- as_tibble(table(tk2.freq$frequency), .name_repair = "minimal") 
# names(tk2.fq.fq) <- c("r", "Nr")
# tk2.fq.fq$r <- as.numeric(tk2.fq.fq$r)
# 
# tk3.fq.fq <- as_tibble(table(tk3.freq$frequency), .name_repair = "minimal") 
# names(tk3.fq.fq) <- c("r", "Nr")
# tk3.fq.fq$r <- as.numeric(tk3.fq.fq$r)



GT.adj.freq <- function(r, ngram, k = 5){
        
if (ngram == 1){ freq.table <- tk1.fq.fq}

else if (ngram == 2){ freq.table <- tk2.fq.fq}
        
else if (ngram == 3){ freq.table <- tk3.fq.fq}

else { print("Invalid n-gram")
        stop()}

if (r <= k){ r.adj <- r }

else {
        C <- freq.table$r
        Nr <- freq.table$Nr
        Zr <- vector("numeric")
        for (i in 2:nrow(freq.table)){
                
                if (i < nrow(freq.table)){
                        
                        Zr[i] <- Nr[i]/(0.5*(((C[i+1])-(C[i-1]))))
                
                        }
                
                else if (i == nrow(freq.table)) {
                        
                        Zr[i] <- Nr[i]/(0.5*(((2*(C[i]))-(C[i-1]))))
                }
                
        }

        df.lm <- tibble(C = freq.table$r, Zr = Zr)

        lm1 <- lm(log(df.lm$Zr)~log(df.lm$C))
        
        b <- lm1$coefficients[2]
        
        if( b > -1) { warning()
                print("b > -1, assumption not valid")} 
        
        r.adj <- r*((1+1/r)^(b+1))
        r.adj <- as.numeric(r.adj)
}       
        
return(r.adj)
        
}

GT.adj.freq(1, 2)


# Stupid Backoff Model

rm(list = setdiff(ls(), c("tk1.freq","tk2.freq", "tk3.freq")))

phrase <- "I started couple days"
tk.phrase <- tokens(phrase, what = "word", remove_punct = T, remove_symbols = T, 
              remove_numbers = T, remove_url = T, remove_separators = T)

query <- paste(tk.phrase[[1]][length(tk.phrase[[1]])-1], 
               tk.phrase[[1]][length(tk.phrase[[1]])], sep = "_")

tk3.freq %>% filter(grepl(paste0("^", query, "_*"), tk3.freq$feature, perl=T))


## things for the report
as.data.frame(tk1.freq) %>% select(feature, frequency) %>% 
        mutate(ngram = "uni") -> tk1.freq.
as.data.frame(tk2.freq) %>% select(feature, frequency) %>% 
        mutate(ngram = "bi") -> tk2.freq
as.data.frame(tk3.freq) %>% select(feature, frequency) %>% 
        mutate(ngram = "tri") -> tk3.freq
tk1.freq %>% bind_rows(tk2.freq, tk3.freq) -> tk.freq.to.plot

ggplot(tk.freq.to.plot, mapping = aes(x=feature, y=frequency)) + 
        geom_jitter()



sum(cumsum(tk1.freq$frequency) <= sum(tk1.freq$frequency*0.5))
sum(cumsum(tk1.freq$frequency) <= sum(tk1.freq$frequency*0.9))

tk1.fq.fq <- as_tibble(table(tk1.freq$frequency), .name_repair = "minimal")
tk2.fq.fq <- as_tibble(table(tk2.freq$frequency), .name_repair = "minimal") 
tk3.fq.fq <- as_tibble(table(tk3.freq$frequency), .name_repair = "minimal") 
data.frame(ngram=c("unigram", "bigram", "trigram"), 
           x1=c(tk1.fq.fq[[2]][1],tk2.fq.fq[[2]][1],tk3.fq.fq[[2]][1]),
           x2=c(tk1.fq.fq[[2]][2],tk2.fq.fq[[2]][2],tk3.fq.fq[[2]][2]),
           x3=c(tk1.fq.fq[[2]][3],tk2.fq.fq[[2]][3],tk3.fq.fq[[2]][3]),
           `x1_percent`=c(round(tk1.fq.fq[[2]][1]/length(tk1.freq$feature)*100,2),
                          round(tk2.fq.fq[[2]][1]/length(tk2.freq$feature)*100,2),
                          round(tk3.fq.fq[[2]][1]/length(tk3.freq$feature)*100,2)))
           
           