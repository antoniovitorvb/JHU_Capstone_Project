library(dplyr)

if (!dir.exists("final")){
     
     if(!file.exists("Coursera-Swiftkey.zip")){
          download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")
     } else {
          unzip("Coursera-Swiftkey.zip")
     }
}

setwd("./final")

# Q1 - The en_US.blogs.txt file is how many megabytes?

file.size("en_US/en_US.blogs.txt")/1024^2


# Q2 - The en_US.twitter.txt has how many lines of text?

twitter <- file("en_US/en_US.twitter.txt", open = "r")
twitter_en <- readLines(twitter, skipNul = T)
close(twitter)

twitter_en %>% length()


# Q3 - What is the length of the longest line seen in any of the three en_US data sets?

# blogs_en <- read.delim("en_US/en_US.blogs.txt", header = F)
# news_en <- read.delim("en_US/en_US.news.txt", header = F)
# twitter_en <- read.delim("en_US/en_US.twitter.txt", header = F)

blogs <- file("en_US/en_US.blogs.txt", open = "r")
blogs_en <- readLines(blogs, skipNul = T)
close(blogs)

news <- file("en_US/en_US.news.txt", open = "r")
news_en <- readLines(news, skipNul = T)
close(news)

blogs_en %>% nchar() %>% max()
news_en %>% nchar() %>% max()
twitter_en %>% nchar() %>% max()


# Q4 - In the en_US twitter data set, if you divide the number of lines 
#where the word "love" (all lowercase) occurs by the number of lines the word 
#"hate" (all lowercase) occurs, about what do you get?

grep("love", twitter_en, value = F) # returns the row indices -> default = FALSE

love <- grep("love", twitter_en) %>% length()
hate <- grep("hate", twitter_en) %>% length()

love/hate


# Q5 - The one tweet in the en_US twitter data set that matches the word "biostats" says what?

grep("biostats", twitter_en, value = T)


# Q6

grep("A computer once beat me at chess, but it was no match for me at kickboxing", twitter_en) %>%
     length()

'[[: