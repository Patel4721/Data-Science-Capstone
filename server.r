# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tm)
source('predictbackoff.r')
source("library_setup.r")
attach('data/Model-BF-modelData.RData')

setupEnvironment()

shinyServer(function(input, output) {
  dataInput <- reactive({
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "predicting...", value = 3)
    
    predictbf(input=input$entry, modelID="BF3", maxResults = input$n)
    
  })
  output$text <- renderText({
    dataInput()
  })
  output$sent <- renderText({
    input$entry
  })
})
