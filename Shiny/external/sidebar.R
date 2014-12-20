# Class Selection Dropdown
output$ClassSelect <- renderUI({
  selectInput("ClassSelect",
              label = h4("Select Class"),
              choices = list("Barbarian",
                             "Crusader",
                             "Demon Hunter" = "DemonHunter",
                             "Monk",
                             "Witch Doctor"= "WitchDoctor",
                             "Wizard"),
              selected = "Barbarian")
})

# Crest for selected Class
output$ClassCrest <- renderUI({
  img(src=paste("crest/",input$ClassSelect,".png", sep= ""), height = 200, width = 200)
})

# Slider inout for GR range
output$RankSlide <- renderUI({
  sliderInput("RankSlide",
              label = h5("Greater Rift Rank Range"),
              min = 1,
              max = 1000,
              value = c(1,100))
})

output$Runes <- renderUI({

  selectizeInput("Runes",
              label = "Runes",
              choices = array(runes(input$RankSlide[1],input$RankSlide[2],input$ClassSelect)[,1]),
              selected = array(runes(input$RankSlide[1],input$RankSlide[2],input$ClassSelect)[c(1:4),1]),
              multiple = T,
              options = list(maxItems = 3)
              )
  

})
