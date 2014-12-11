pkgs <- c("ggplot2" ,"png")
pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs)) install.packages(pkgs,repos="http://cran.cs.wwu.edu/")
require(ggplot2); require(png); 
source("external/plot1.R", local = T )

shinyUI(
  
  fluidPage(
    theme = "blackorange.css",
    
    sidebarLayout(
      sidebarPanel(
        uiOutput("ClassSelect"),
        wellPanel(
          uiOutput("ClassCrest"),
          uiOutput("RankSlide"),

          uiOutput("Runes")
          )
        ),
      mainPanel(
        uiOutput("Banner"),
        plotOutput("PlotC"),

        p(h4("Runes")),

        plotOutput("PlotR")
    )
  )
)
)

