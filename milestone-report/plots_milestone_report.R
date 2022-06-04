library(dplyr)

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


library(stringr)
library(tm)

words <- str_split(blogs[1:3],
                   pattern = " ") %>%
     unlist() %>%
     unique()

unique(words)

blogs[1:10] %>% 
     str_remove_all(pattern = '[[:punct:]]') %>%
     removeWords(words = stopwords('en'))

blogs[1:10] %>% 
     str_remove_all(pattern = '[[:punct:]]') %>%
     removeWords(words = stopwords('en')) %>%
     tolower() %>%
     str_split(pattern = " ") %>%
     unlist() %>% grep(pattern = "^[a-z]") %>%
     table() %>% sort(decreasing = T)

clean_words <- function(x) {
     require(dplyr); require(stringr)
     
     UW <- str_remove_all(string = x,
                          pattern = '[[:punct:]]') %>%
          removeWords(words = stopwords('en')) %>%
          tolower() %>%
          str_split(pattern = " ") %>%
          unlist()
     
     return(UW[(grepl(pattern = "^[a-z]", UW))&(!grepl(pattern = "[^\x01-\x7F]+", UW))]) # "[^\x01-\x7F]+" removes non-english characters 
}

table_words <- function(x) {
     require(dplyr); require(stringr); require(tm)
     
     UW <- str_remove_all(string = x,
                          pattern = '[[:punct:]]') %>%
          removeWords(words = stopwords('en')) %>%
          tolower() %>%
          str_split(pattern = " ") %>%
          unlist()
     
     UW <- UW[(grepl(pattern = "^[a-z]", UW))&(!grepl(pattern = "[^\x01-\x7F]+", UW))] # "[^\x01-\x7F]+" removes non-english characters 
     
     return(as.data.frame(sort(table(UW), decreasing = T)))
}

blogs_df <- table_words(blogs)
news_df <- table_words(news)
twitter_df <- table_words(twitter)

mytable %>%head()
hist(head(mytable, 20))

# Joining data frames
full_join(x = blogs_df[1:150,],
          y = news_df[1:150,],
          by = "UW") %>% rowwise() %>% # RowWise applies vectorized formulas for each row
     mutate(rowsum = sum(Freq.x, Freq.y,
                      na.rm = T)) %>%
     select(UW, rowsum) # %>% wordcloud2()

joined_df <- full_join(x = blogs_df[1:150,],
                       y = news_df[1:150,],
                       by = "UW") %>%
     full_join(y = twitter_df[1:150,],
               by = "UW") %>% rowwise() %>%
     mutate(rowsum = sum(Freq.x, Freq.y, Freq,
                         na.rm = T)) %>%
     select(UW, rowsum) %>% arrange(desc(rowsum)) %>%
     as.data.frame()

joined_df %>% head(20)

# library(tidyverse)
library(wordcloud2)

wordcloud2(head(joined_df, 150), size = 3)


# Histograms
library(ggplot2)
library(scales)
library(plotly)

g <- ggplot(head(joined_df, 20),
            aes(x = UW,
                y = rowsum,
                fill = UW)) +
     geom_histogram(stat = "identity") +
     theme(axis.text.x = element_text(angle = 90, 
                                      vjust = 0.5, 
                                      hjust=1),
           legend.position = "none",
           axis.title.x = element_blank()) +
     ylab("Frequency") +
     scale_y_continuous(labels = comma)

ggplotly(g)

### tests

news %>% str_remove_all(pattern = '[[:punct:]]') %>%
     system.time()

news %>% gsub(pattern = '[[:punct:]]', replacement = "") %>%
     system.time()

blogs[1:10] %>% str_remove_all(pattern = '[[:punct:]]') %>% nchar()

blogs[1:10] %>% gsub(pattern = '[[:punct:]]', replacement = "") %>% nchar()



     str_remove_all(pattern = "")

identical(blogs[1:3] %>% str_remove_all(pattern = '[[:punct:]]'),
          blogs[1:3] %>% gsub(pattern = '[[:punct:]]', replacement = ""))