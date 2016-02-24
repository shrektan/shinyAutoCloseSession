
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


function(input, output, session) {
#   nowReact <- reactivePoll(intervalMillis = 1000, session, 
#                            function() Sys.time(),
#                            function() Sys.time())
  sessionAutoClose <- reactiveValues(
    lastInput = Sys.time(), 
    Now =  reactivePoll(intervalMillis = 1000, session, 
                        function() Sys.time(),
                        function() Sys.time())
  )
  observe({
    r <- as.double.difftime(difftime(
      sessionAutoClose$Now(), sessionAutoClose$lastInput, 
      units = "secs"), units = "secs")
    message("check", r) 
    if (r > 5) {
      shinyjs::runjs("window.setTimeout(window.close, 5000);")
      session$close()
    }
  })
  session$onInputReceived({
    function(...) {message("fire"); sessionAutoClose$lastInput <- Sys.time()}
  })
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
}
