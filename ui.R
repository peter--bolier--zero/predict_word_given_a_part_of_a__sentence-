library(shiny)

# User interface for simple word prediction application
# 
# input: 
#        - line of text
#        - button to predict next word
# Output:
#        - some statistics to give insight about the progess on prediction
#        - the top 10 of words (max)

shinyUI(fluidPage(
  
  theme = shinythemes::shinytheme("slate"),
  
  # Started with other layouts.. didn't look very well when scaling...

  hr(style="border-color: #eeeeee; border-width: 5px;"),
  
  # Page header
  fluidRow(
    # Application title
    h1("Word prediction", align = "center", style = "font-family: 'Lobster', cursive;
        font-weight: 500; line-height: 1.1; "),
    h6("or what will the next word be?", align = "center", style = "font-family: 'Lobster';")
    
  ),
  hr(style="border-color: #eeeeee; border-width: 2px;"),
  
  # Data entry
  fluidRow(
    # Indicate what to do
    h4("Please enter a few words, or a short sentence...", 
       align = "center", style = "font-family: 'Lobster', cursive; font-weight: 500; line-height: 1.1;", width="80%")
  ),
  # Field to enter sentence
  fluidRow(
    column(10,
           # For the input
           textInput("text", width="80%", label=NULL, value = "Love that film and haven't seen it in quite some ")
    ),
    column(2,
           # For now we dont calcaulte after every character entered
           #actionButton("predict", label="Predict word")
           submitButton("Predict word")
    )
  ),
  hr(style="border-color: #eeeeee; border-width: 2px;"),
  
  # Wanted to show intermediate steps, didnt work in shiny, only a progress bar.
  
  # Footer, progress, result and explanation
  fluidRow(
    h4("Let's see", 
       align = "center", style = "font-family: 'Lobster', cursive; font-weight: 500; line-height: 1.1;", width="80%"),
    column(width=10, offset = 1,
       h4("Words and probablity"),
       dataTableOutput("view")
    )
  ),
  hr(style="border-color: #eeeeee; border-width: 2px;"),
  
  
  h5("Explanation"),
  h6("The word prediction is based on a (relative) simple set of ngrams, in this application we first use a 
     four-gram using three words to lookup the fourth. To enhance the prediction capabilities, we also look for only two words
     in the fourgram skipping one." ),
  h6("Example, sentence: Love that film and haven't seen it in quite some"),
  h6("Used words: in quite some"),
  h6("n=4 gram lookup:"),
  h6("in quite some; in <skip> some"),
  h6("If we haven't found enough words (like 10) we look further in three-grams, two-grams or in the end in the unigrams."),
  
  hr(style="border-color: #eeeeee; border-width: 5px;")
))