library(shiny)
library(datasets)

source("predictword.R")

# Define server logic , its really simple...
shinyServer(function(input, output) {
  
  # Return the entered sentence
  textInput <- reactive({
    input$text
  })
  
  # Use the given text to lookup/predict next word.
  # the result is a table
  output$view <- renderDataTable({
    sentence <- textInput()
    wordspredicted <- predictnextword(sentence)
  })
})