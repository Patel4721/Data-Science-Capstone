# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  # Set the page title
  titlePanel("Next Word Prediction"),
  
  sidebarPanel(
    textInput("entry",
              h5("Enter your sentence"),
              "Let me guess! Type here..."),
    numericInput("n",
                 h5("Numbers of words to predict"),
                 value = 3),
    submitButton("Submit"),
    br(),
    "The author of this app is: ",
    a("Shailesh Patel", href = "patel4721@gmail.com"),
    p(em("Github repository:",
                a("A John Hopkins University - Capstone Project",
                  href="https://github.com/Patel4721/Data-Science-Capstone")))
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Using this App",br(),
                         p('This application predicts the next word from the sentence you provide.  It uses a predictive text algorithm to determine most likely next word. The algorithm uses processed data from a large quantity of twitter feeds, news stories, and blogs. Enter your sentence in the panel to the left and press the Submit button.  You have the option of selecting the number of words to predict as well (with a default of 3 words).  Try it!'),
                         h4('Sentence You Provided'),
                         tags$style(type='text/css', '#sent {background-color: rgba(0,0,255,0.10); color: blue;font-size:25px;}'), 
                         h4(verbatimTextOutput("sent"),style = "color:green"),                               
                         h4('Predicted Words'),
                         tags$style(type='text/css', '#text {background-color: rgba(255,255,0,0.10); color: green;font-size:30px;}'),
                         span(h4(verbatimTextOutput('text'),style = "color:green"))),
#                 tabPanel('How it Works',
#                          h5('This is how it is done'),
#                          verbatimTextOutput("oiBMI"),
#                          verbatimTextOutput("oiBMIclass"),
#                          img(src="ney.jpg", height = 500, width =500),
#                          p("View This: ", a("Text Mining process", 
#                                             href = "http://www.r-bloggers.com/text-mining/")),
#                          p("And This: ", a("ngrams", 
#                                            href = "http://www.slideshare.net/ShangxuanZhang/introducing-nlp-with-r"))
#                 ),
#                 
#                 tabPanel("Shortcomings",
#                          h5("Kneser-Ney Smoothing is slow"),
#                          p('Well, I suggested using Stupid Kick-off first as it fast and then using Kneser-Ney when necessary because it is usually slow.'),
#                          h5("words!"),
#                          p("Dirty words will be deleted from the final result."),
#                          h5("Nothing return when you input something?"),
#                          p('This will occur when you only input punctuation, numbers and some common words. The model will remove them in the input and nothing will return.'),
#                          h4("Important"),
#                          p("To make this model faster, I only extracted the terms occurred in the whole sources more than 5 times. ")              
#                 ),
                tabPanel("Reference",
                         p("Körner, M. C. (n.d.). Implementation of Modified Kneser-Ney Smoothing on Top of Generalized Language Models for Next Word Prediction Bachelorarbeit, (September 2013)."),
                         p("Williams, G. (2014). Data Science with R Text Mining."),
                         p("Kneser, Ney (1995). Improved backing-off for m-gram language modeling"),
                         p("Christopher D. Manning, Hinrich Schütze, Foundations of Statistical Natural Language Processing")
                         
                )
    ))
))