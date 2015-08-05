# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tm)
source('predict.r')
#attach('Model-modelData.RData')

shinyServer(function(input, output) {
  dataInput <- reactive({
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "predicting", value = 3)
    if(input$radio == 1){
      predictWord(modelDir="Model", input$entry, maxResults = input$n)
    }else{
      predictWord(modelDir="Model", input$entry, maxResults = input$n)
    }
  })
  output$text <- renderText({
    dataInput()
  })
  output$sent <- renderText({
    input$entry
  })
})
