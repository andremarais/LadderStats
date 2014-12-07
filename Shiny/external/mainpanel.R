output$Banner <- renderUI({
  img(src=paste("banners/",input$ClassSelect,".png", sep= ""), height=100, width = 640)
  
})

#  img(src=paste("crest/",input$ClassSelect,".png", sep= ""), height = 200, width = 200)