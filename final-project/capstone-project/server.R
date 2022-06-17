#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(stringr)

# Loading the data
if (!dir.exists("final")){
     
     if(!file.exists("Coursera-Swiftkey.zip")){
          download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                        destfile = "Coursera-Swiftkey.zip")
     }
     
     unzip("Coursera-Swiftkey.zip")
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
     
     predOutput <- reactive({
          paste(input$text, "Oh Yeah!", sep = " ")
     })
     
     if(!is.null(input$text)){
          output$word <- predOutput
     }
})
