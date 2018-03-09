server <- function(input, output, session){
  session$onSessionEnded(stopApp)
  
  ### Close App with a button ####################################################
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  }) # ends Session if Window is closed
}