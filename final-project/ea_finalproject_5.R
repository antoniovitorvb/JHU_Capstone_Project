#####   DATA ACQUISITION   #####
library(dplyr)
library(tm)
library(ngramrr)
library(quanteda)

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

data <- c(blogs, news, twitter)
rm(list = c("text", "blogs", "news", "twitter"))

set.seed(123)
sample_data <- sample(data, 1000)

# capture.output(unlist(sample_data,
#                       use.names = F),
#                file = "sampleData.txt")

# write.table(sample_data,
#             file = "sampleData2.txt",
#             sep = "\n",
#             row.names = F)

sampleCorpus <- VectorSource(sample_data) %>%
     VCorpus(readerControl = list(reader = readPlain))

preprocess_corpus <- function(corpus){
     return(corpus %>%
                 tm_map(removePunctuation) %>%
                 tm_map(removeNumbers) %>%
                 tm_map(content_transformer(tolower)) %>%
                 tm_map(stripWhitespace) %>%
                 tm_map(removeWords, stopwords("en"))
     )
}

samplecorpus_cleaned <- preprocess_corpus(sampleCorpus)

# save(samplecorpus_cleaned,
#      file = "samplecorpus.RData")

##### CREATING THE N-GRAMS #####
# load("samplecorpus.RData")

create_ngram <- function(corpus, n = 2){
     if (n == 1) TermDocumentMatrix(corpus)
     else tdm2(corpus,
               ngmin = n,
               ngmax = n)
}

tdm_unigram <- create_ngram(samplecorpus_cleaned, 1)
tdm_bigram <- create_ngram(samplecorpus_cleaned, 2)
tdm_trigram <- create_ngram(samplecorpus_cleaned, 3)

# save(tdm_unigram,
#      file = "sampleunigram.RData")

# save(tdm_unigram,
#      file = "samplebigram.RData")

# save(tdm_unigram,
#      file = "sampletrigram.RData")

save(tdm_unigram, tdm_bigram, tdm_trigram,
     file = "./capstone-project/ngrams.RData")



#####   BUILDING THE MODEL   #####
load("./capstone-project/ngrams.RData")


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