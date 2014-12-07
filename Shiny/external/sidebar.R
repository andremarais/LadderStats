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
              label = h5("Rank Range"),
              min = 1,
              max = 1000,
              value = c(1,100))

  
  
})

