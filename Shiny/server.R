shinyServer(function(input, output) {
  source("external/sidebar.R", local = T )
  source("external/mainpanel.R", local = T )
  source("external/plot1.R", local = T )$value
  source("external/plot2.R", local = T )
  output$PlotC <- renderPlot(plot1(input$RankSlide[1], input$RankSlide[2], input$ClassSelect[1]))
  output$PlotR <- renderPlot(runeplot(input$Runes,array(input$ClassSelect)))

})