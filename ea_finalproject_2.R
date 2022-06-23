setwd("D:/vitor/Documentos/GitHub/JHU_Capstone_Project/final-project")

library(dplyr)
library(tm)
library(ngramrr)

if (!dir.exists("final")){
     
     if(!file.exists("Coursera-Swiftkey.zip")){
          download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                        destfile = "Coursera-Swiftkey.zip")
     }
     
     unzip("Coursera-Swiftkey.zip")
}

text <- file("final/en_US/en_US.blogs.txt", open = "r")
blogs <- readLines(text, skipNul = T)
close(text)

text <- file("final/en_US/en_US.news.txt", open = "r")
news <- readLines(text, skipNul = T)
close(text)

text <- file("final/en_US/en_US.twitter.txt", open = "r")
twitter <- readLines(text, skipNul = T)
close(text)

rm("text")

data <- c(blogs, news, twitter)

sample_data <- sample(data, 10000)

corpus_data <- VectorSource(sample_data) %>%
     VCorpus()

preprocess_corpus <- function(corpus){
     return(corpus %>%
                 tm_map(removePunctuation) %>%
                 tm_map(removeNumbers) %>%
                 tm_map(content_transformer(tolower)) %>%
                 tm_map(stripWhitespace) %>%
                 tm_map(removeWords, stopwords("en"))
            )
}

vc_data <- corpus_data %>%
     tm_map(removePunctuation) %>%
     tm_map(removeNumbers) %>%
     tm_map(content_transformer(tolower)) %>%
     tm_map(stripWhitespace) %>%
     tm_map(removeWords, stopwords("en"))

katz_backoff_model <- function(phrase) {
     
     if (typeof(phrase) == "character") {
          
          trigram_model <- function(tokens) {
               
               key <- function(tokens) {
                    paste(
                         tail(
                              tokens,
                              n = 2
                         )[1],
                         tail(
                              tokens,
                              n = 2
                         )[2]
                    )
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
                                                  paste(
                                                       strsplit(
                                                            terms, split = " "
                                                       )[[1]][1],
                                                       strsplit(
                                                            terms, split = " "
                                                       )[[1]][2]
                                                  ),
                                                  ignore.case = TRUE
                                             )
                                        }
                                   )
                              )
                         ),
                         function(match) sum(tm_term_score(tdm_trigram, match))
                    )
               }
               
               # find the last word of the most frequent match
               tail_of_most_frequent_match <- function(phrase) {
                    matches <- matches_count(phrase)
                    if (length(matches) > 0) {
                         tail(
                              strsplit(
                                   names(
                                        head(
                                             which(matches == max(matches)),
                                             n = 1
                                        )
                                   )
                                   , split = " ")[[1]],
                              n = 1
                         )
                    } else bigram_model(tail(corpus_input, n = 1))
               }
               
               return(
                    tail_of_most_frequent_match(key(tokens))
               )
               
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
                                             grepl(
                                                  phrase,
                                                  strsplit(
                                                       terms, split = " "
                                                  )[[1]][1],
                                                  ignore.case = TRUE
                                             )
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
                         tail(
                              strsplit(
                                   names(
                                        head(
                                             which(matches == max(matches)),
                                             n = 1
                                        )
                                   )
                                   , split = " ")[[1]],
                              n = 1
                         )
                    } else unigram_model(tail(corpus_input, n = 1))
               }
               
               return(
                    tail_of_most_frequent_match(token)
               )
               
          }
          
          unigram_model <- function(token) {
               
               associations <-
                    findAssocs(tdm_unigram, token, corlimit = .99)[[1]]
               if (length(associations) > 0) {
                    names(sample(which(associations == max(associations)), 1))
               } else return("will")
               
          }
          
          # preprocess phrase
          corpus_input <-
               VCorpus(
                    VectorSource(phrase),
                    list(reader = PlainTextDocument)
               )
          corpus_input <- preprocess_corpus(corpus_input)
          corpus_input <- scan_tokenizer(corpus_input[[1]][[1]][1])
          
          return(
               if (length(corpus_input) >= 2) {
                    trigram_model(corpus_input)
               } else if (length(corpus_input) == 1) {
                    bigram_model(corpus_input)
               } else unigram_model(corpus_input)
          )
          
     } else {
          stop("non-character or null input")
     }
     
}

############################################################################

# my_ngram <- function(corpus, n = 2){
#      if (n == 1) TermDocumentMatrix(corpus)
#      else tdm2(corpus,
#                ngmin = n,
#                ngmax = n)
# }
# 
# tdm_unigram <- my_ngram(vc_data, 1)
# tdm_bigram <- my_ngram(vc_data, 2)
# tdm_trigram <- my_ngram(vc_data, 3)

########################################################################

# sample_data <- sample(data, 10000) %>%
#      iconv(from = "latin1",
#            to = "UTF-8",
#            sub="") %>%
#      stri_replace_all_regex(pattern = "\u2019|`",
#                             replacement = "'") %>%
#      stri_replace_all_regex(pattern = "\u201c|\u201d|u201f|``",
#                             replacement = '"')
# 
# vc_data <- data.frame(doc_id = 1:length(sample_data),
#                 text = sample_data) %>%
#      DataframeSource() %>%
#      VCorpus %>%
#      tm_map(stripWhitespace)
# 
# tdm_data <- vc_data %>%
#      TermDocumentMatrix(control = list(removePunctuation = T,
#                                        removeNumbers = T,
#                                        wordLengths = c(1, Inf)))
