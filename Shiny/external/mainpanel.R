source("external/plot1.R", local = T )$p1

output$Banner <- renderUI({
  img(src=paste("banners/",input$ClassSelect,".png", sep= ""), height=100, width = 640)
})



