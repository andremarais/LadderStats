source("external/plot1.R", local = T )

output$Banner <- renderUI({
  img(src=paste("banners/",input$ClassSelect,".png", sep= ""), height=100, width = 640)
})




output$Texttest <- renderUI({
  p(input$RankSlide[2])
  
})

