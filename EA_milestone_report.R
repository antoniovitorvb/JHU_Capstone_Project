library(tidyverse)
library(dplyr)
library(wordcloud2)



if (!dir.exists("final")){
     
     if(!file.exists("Coursera-Swiftkey.zip")){
          download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                        destfile = "Coursera-Swiftkey.zip")
     } else {
          unzip("Coursera-Swiftkey.zip")
     }
}

setwd("./final")

text <- file("en_US/en_US.blogs.txt", open = "r")
blogs <- readLines(text, skipNul = T)
close(text)

text <- file("en_US/en_US.news.txt", open = "r")
news <- readLines(text, skipNul = T)
close(text)

text <- file("en_US/en_US.twitter.txt", open = "r")
twitter <- readLines(text, skipNul = T)
close(text)

rm("text")

library(stringr)
# File size:
file.info("en_US/en_US.twitter.txt")

file_MB <- round(file.info(c("en_US/en_US.blogs.txt",
                             "en_US/en_US.news.txt",
                             "en_US/en_US.twitter.txt"))$size / 1024^2,
                 digits = 1)

# Number of characteres
nchar(blogs) %>% head()

numChars <- sapply(list(nchar(blogs), nchar(news), nchar(twitter)),
                   FUN = sum)

# Number of lines
numLines <- sapply(list(blogs, news, twitter),
                   FUN = length)

# number of words
## Method 1:
str_split(blogs,
          pattern = " ") %>%
     lengths() %>%
     system.time()

## Method 2:
gregexpr(pattern = "\\w+", 
         text = blogs) %>%
     lengths() %>%
     system.time()

## Method 3:
str_count(blogs,
          pattern = "\\w+") %>%
     sum() %>%
     system.time()
###
### Method 3 is the fastest! ###
###

str_count(blogs[1:10],
          pattern = "\\w+")

WPL <- sapply(list(blogs, news, twitter),
                   FUN = function(x){
                        str_count(string = x,
                                  pattern = "\\w+")
                   })

numWords <- sapply(X = WPL,
                   FUN = sum)

# Testing with a smaller sentence
library(tm)

text_string <- 'I have been using the tm package to run some text analysis. My problem is with creating a list with words and their frequencies associated with the same. I typically use the following code for generating list of words in a frequency range. Is there any way to automate this such that we get a dataframe with all words and their frequency?
The other problem that i face is with converting the term document matrix into a data frame. As i am working on large samples of data, I run into memory errors. Is there a simple solution for this?'

text_corpus <- VectorSource(blogs) %>%
     Corpus() %>% # Converts from character to Corpus
     tm_map(tolower) %>%
     tm_map(removePunctuation) %>%
     tm_map(FUN = removeWords,
            stopwords(kind = "en")) # %>%
#      TermDocumentMatrix() %>% # Converts to a Document-term matrix
#      as.matrix()

# text_words <- text_corpus$dimnames$Terms # takes each word as a char vector

text_matrix <- sort(rowSums(text_corpus), 
                    decreasing = T)

df_word <- data.frame(word = names(text_matrix),
                      freq = text_matrix)
rownames(df_word) <- NULL

head(df_word)

# word_freq() will take a Character List and return a Dataframe of each word frequency in that list

word_freq <- function(char_list) {
     require(dplyr)
     require(tm)
     
     defaultW <- getOption("warn")
     options(warn = -1)
     x <- VectorSource(char_list) %>%
          Corpus() %>% # Converts from character to Corpus
          tm_map(removePunctuation) %>%
          tm_map(tolower) %>%
          tm_map(FUN = removeWords,
                 stopwords(kind = "en")) %>%
          TermDocumentMatrix() %>% # Converts to a Document-term matrix
          as.matrix()
     options(warn = defaultW)
     
     text_matrix <- sort(rowSums(x), 
                         decreasing = T)
     
     df_word <- data.frame(word = names(text_matrix),
                           freq = text_matrix)
     rownames(df_word) <- NULL
     
     return(df_word[df_word$freq > 1,])
}


# token <- str_remove_all(string = text_string,
#                         pattern = '[[:punct:]]') %>% # remove all punctuation
#      tolower() # converts uppercase to lowercase
#      # str_remove_all(pattern = stopwords(kind = "en"))
# 
# str_split(text_string, pattern = " ")[[1]] %>% 
#      length() # count words
