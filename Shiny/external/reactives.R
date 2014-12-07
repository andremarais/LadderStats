target <- reactive({
  if(input$ClassSelect == "Barbarian") {
    target <- "barb"
  } else if(input$ClassSelect == "Crusader") {
    target <- "crusader"
  } else if(input$ClassSelect == "DemonHunter") {
    target <- "dh"
  } else if(input$ClassSelect == "Monk") {
    target <- "monk"
  } else if(input$ClassSelect == "WitchDoctor") {
    target <- "wd"
  } else if(input$ClassSelect == "Wizard") {
    target <- "wizard"
  }
})

bcol <- reactive({
  if(input$ClassSelect == "Barbarian") {
    bcol ="#DB3232"
  } else if(input$ClassSelect == "Crusader") {
    target <- "crusader";bcol ="#F2EEB1"
  } else if(input$ClassSelect == "DemonHunter") {
    bcol ="#B058AE"
  } else if(input$ClassSelect == "Monk") {
    bcol ="#E3DE54"
  } else if(input$ClassSelect == "WitchDoctor") {
    bcol ="#279423"
  } else if(input$ClassSelect == "Wizard") {
    bcol ="#3BDCF5"
  }
})
  
  
  
  
  

# if(input$ClassSelect == "Barbarian") {
#   target <- "barb";bcol ="#DB3232"
# } else if(input$ClassSelect == "Crusader") {
#   target <- "crusader";bcol ="#F2EEB1"
# } else if(input$ClassSelect == "DemonHunter") {
#   target <- "dh";bcol ="#B058AE"
# } else if(input$ClassSelect == "Monk") {
#   target <- "monk";bcol ="#E3DE54"
# } else if(input$ClassSelect == "WitchDoctor") {
#   target <- "wd";bcol ="#279423"
# } else if(input$ClassSelect == "Wizard") {
#   target <- "wizard";bcol ="#3BDCF5"
# }