setwd("D:/vitor/Documentos/GitHub/JHU_Capstone_Project/final-project")

library(dplyr)
library(stringr)
library(ggplot2)
library(tm)
library(RWeka)
library(magrittr)


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

table_words <- function(x) {
  require(dplyr); require(stringr); require(tm)
  
  UW <- str_remove_all(string = x,
                       pattern = '[[:punct:]]') %>%
    removeWords(words = stopwords('en')) %>%
    tolower() %>%
    str_split(pattern = " ") %>%
    unlist()
  
  UW <- UW[(grepl(pattern = "^[a-z]", UW))&(!grepl(pattern = "[^\x01-\x7F]+", UW))] # "[^\x01-\x7F]+" removes non-english characters 
  
  # return(as.data.frame(sort(table(UW), decreasing = T)))
  return(UW)
}

# blogs_df <- table_words(blogs)
# news_df <- table_words(news)
# twitter_df <- table_words(twitter)

df_data <- table_words(data)

data_corpus <- VectorSource(data) %>%
     Corpus() %>% # Converts from character to Corpus
     tm_map(tolower) %>%
     tm_map(removePunctuation) %>%
     tm_map(FUN = removeWords,
            stopwords(kind = "en"))

data_tdm <- data_corpus %>%
     TermDocumentMatrix(control = list(removePunctuation = TRUE,
                                       removeNumbers = TRUE,
                                       wordLengths = c(1, Inf)))



