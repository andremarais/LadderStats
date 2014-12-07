#source("external/doplot1.R", local = T )$value





shinyServer(function(input, output) {
  source("external/sidebar.R", local = T )
  source("external/mainpanel.R", local = T )
  source("external/doplot1.R", local = T )
  #source("external/reactives.R", local = T )
  output$Plotc <- renderPlot(source("external/doplot1.R", local = T )$value)

})