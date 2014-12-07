pkgs <- c("ggplot2" ,"png")
pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs)) install.packages(pkgs,repos="http://cran.cs.wwu.edu/")
require(ggplot2); require(png); 





shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        uiOutput("ClassSelect"),
        wellPanel(
          uiOutput("ClassCrest"),
          uiOutput("RankSlide")
          )
        ),
      mainPanel(
        uiOutput("Banner"),
        uiOutput("Plot1"))
    )
  )
)
