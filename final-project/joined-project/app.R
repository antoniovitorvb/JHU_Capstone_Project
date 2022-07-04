#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tm)
library(ngramrr)

# Loading the ngrams processed in ea_finalproject_4.R
load("ngrams.RData")

preprocess_corpus <- function(corpus){
     return(corpus %>%
                 tm_map(removePunctuation) %>%
                 tm_map(removeNumbers) %>%
                 tm_map(content_transformer(tolower)) %>%
                 tm_map(stripWhitespace) %>%
                 tm_map(removeWords, stopwords("en"))
     )
}

katz_backoff_model <- function(phrase) {
     
     if (typeof(phrase) == "character") {
          
          trigram_model <- function(tokens) {
               
               key <- function(tokens) {
                    paste(tail(tokens, n = 2)[1],
                          tail(tokens, n = 2)[2])
               }
               
               # find matches and their count
               matches_count <- function(phrase) {
                    sapply(
                         names(
                              which(
                                   sapply(
                                        Terms(tdm_trigram),
                                        function(terms) {
                                             grepl(
                                                  phrase,
                                                  paste(strsplit(terms,
                                                                 split = " ")[[1]][1],
                                                        strsplit(terms,
                                                                 split = " ")[[1]][2]),
                                                  ignore.case = TRUE)
                                        }
                                   )
                              )
                         ),
                         
                         function(match) {
                              sum(tm_term_score(tdm_trigram, match))
                         }
                    )
               }
               
               # find the last word of the most frequent match
               tail_of_most_frequent_match <- function(phrase) {
                    matches <- matches_count(phrase)
                    if (length(matches) > 0) {
                         tail(strsplit(names(head(which(matches == max(matches)),
                                                  n = 1)), 
                                       split = " ")[[1]],
                              n = 1)
                    } else {
                         bigram_model(tail(corpus_input, n = 1))
                    }
               }
               
               return(tail_of_most_frequent_match(key(tokens)))
          }
          
          bigram_model <- function(token) {
               
               # find matches and their count
               matches_count <- function(phrase) {
                    sapply(
                         names(
                              which(
                                   sapply(
                                        Terms(tdm_bigram),
                                        function(terms) {
                                             grepl(phrase,
                                                   strsplit(terms,
                                                            split = " ")[[1]][1],
                                                   ignore.case = TRUE)
                                        }
                                   )
                              )
                         ),
                         
                         function(match) sum(tm_term_score(tdm_bigram, match))
                    )
               }
               
               # find the last word of the most frequent match
               tail_of_most_frequent_match <- function(phrase) {
                    matches <- matches_count(phrase)
                    if (length(matches) > 0) {
                         tail(strsplit(names(head(which(matches == max(matches)),
                                                  n = 1)), 
                                       split = " ")[[1]],
                              n = 1)
                         
                    } else {
                         unigram_model(tail(corpus_input, n = 1))
                    }
               }
               
               return(tail_of_most_frequent_match(token))
               
          }
          
          unigram_model <- function(token) {
               
               associations <- findAssocs(tdm_unigram, 
                                          token, 
                                          corlimit = .99)[[1]]
               
               if (length(associations) > 0) {
                    
                    names(sample(which(associations == max(associations)), 
                                 1))
                    
               } else return("will")
          }
          
          # preprocess phrase
          corpus_input <- VCorpus(VectorSource(phrase),
                                  list(reader = PlainTextDocument))
          
          corpus_input <- preprocess_corpus(corpus_input)
          
          corpus_input <- scan_tokenizer(corpus_input[[1]][[1]][1])
          
          return(
               if (length(corpus_input) >= 2) {
                    trigram_model(corpus_input)
                    
               } else if (length(corpus_input) == 1) {
                    bigram_model(corpus_input)
                    
               } else return("will")
          )
          
     } else {
          stop("non-character or null input")
     }
}

# Define UI for application that draws a histogram
ui <- fluidPage(

     # Application title
     titlePanel("Final Project Submission"),
     
     h2("Capstone Project"),
     
     h3("Johns Hopkins University - Data Science Specialization"),
     
     # Sidebar with a slider input for number of bins
     sidebarLayout(
          sidebarPanel(
               h3("How It Works:"),
               "Type any word or sentence and the output will show the most 
              possible suggestion to complete it."
          ),
          
          # Show a plot of the generated distribution
          mainPanel(
               textInput(inputId = "text",
                         label = "Type here"),
               
               textOutput(outputId = "predict")
          )
     )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

     output$predict <- renderText({
          if (input$text == "") "Waiting for input..."
          else katz_backoff_model(input$text)
     })
}

# Run the application 
shinyApp(ui = ui, server = server)
