#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Final Project Submission"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
         sidebarPanel(
              h3("How It Works:"),
              "Type any word or sentence then press 'Enter' to see the predicted word."
              ),

        # Show a plot of the generated distribution
          mainPanel(
               textInput(inputId = "text",
                         label = "Type here"),
               
               textOutput("word")
               )
        )
    )
)
