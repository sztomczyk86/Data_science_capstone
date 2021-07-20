#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(cld2) # detect language
library(quanteda)

# Define UI for application that draws a histogram


ui <- fluidPage(

    # Application title
    titlePanel("Next Word Prediction - Capstone Project"),
    h5("by Szymon Tomczyk, July 2021"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        verticalLayout(
            
            textInput("sentence.TC", "Write a sentence to be completed:",
                      width = "90%", placeholder = "Type something here and press 'Predict'"),
            
            actionButton("click.me", "Predict", width = "20%")
        ),

        # Show a plot of the generated distribution
        verticalLayout(
            dataTableOutput("prediction.table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
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
    
        sentence.TC <-eventReactive(input$click.me,{
            sentence.TC <- input$sentence.TC })
        
        output$prediction.table <- renderDataTable({
        
        next.word.prediction(sentence.TC())
            
        })
                                                
}

# Run the application 
shinyApp(ui = ui, server = server)
