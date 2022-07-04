#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tm)
library(ngramrr)

load("ngrams.RData")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

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
)
