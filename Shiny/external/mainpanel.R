source("external/plot1.R", local = T )$value

output$Banner <- renderUI({
  img(src=paste("banners/",input$ClassSelect,".png", sep= ""), height=100, width = 640)
})

output$Plot1 <- renderUI({
  plotOutput(plot1(1,100,"Monk"))


 
  
})

output$Texttest <- renderUI({
  p(input$RankSlide[2])
  
})