multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


runes <- function(GRF, GRT, Class){
  if(Class == "Barbarian") {
    BData <- bscv
  } else if(Class == "Crusader") {
    BData <- cscv
  } else if(Class == "DemonHunter") {
    BData <- dhscv
  } else if(Class == "Monk") {
    BData <- mscv
  } else if(Class == "WitchDoctor") {
    BData <- wdscv
  } else if(Class == "Wizard") {
    BData <- wscv
  }
  
  BData.subset <- BData[c(GRF:(GRT*10)),]
  BData.subset.active <- BData.subset[which(BData.subset$Rune != "Passive"),]
  BData.subset.passive <- BData.subset[which(BData.subset$Rune == "Passive"),]
  
  t <- data.frame(t(table(BData.subset.active$Spell)))
  t$Var1 <- NULL
  colnames(t) <- (c("Spell", "Count"))
  t$Count <- as.integer(t$Count)
  t <- t[order(t$Count, decreasing = T),]
  t <- t[1:15,]

  
  return(t)
}




### Rune Graph Function
runeplot <- function(RuneSample, Class = "Barbarian", GRF, GRT) ({
  
  if(Class == "Barbarian") {
    BData <- bscv
  } else if(Class == "Crusader") {
    BData <- cscv
  } else if(Class == "DemonHunter") {
    BData <- dhscv
  } else if(Class == "Monk") {
    BData <- mscv
  } else if(Class == "WitchDoctor") {
    BData <- wdscv
  } else if(Class == "Wizard") {
    BData <- wscv
  }

  runes <- list(list())
  plots <- list()
  BData <- BData[c(GRF:(GRT*10)),]
  
  for (i in 1:length(RuneSample)) {
    
    
      runes.subset <- BData[which(BData$Spell == RuneSample[i]),c(2,3)]
      runes.subset <-droplevels(runes.subset)

      runes.subset <- data.frame(t(table(runes.subset)))[,c(1,3)]
      
      runes[i] <- list(runes.subset[order(runes.subset$Freq, decreasing = T),])
      runes[[i]][,3] <- RuneSample[i]
      colnames(runes[[i]]) <- c("Rune", "Frequency", "Spell")
      plots[[i]] <- ggplot(data = runes[[i]], aes(x = Spell, y = Frequency, fill = Rune, order = -Frequency)) + 
        geom_bar(stat = "identity", position = "stack")+ coord_flip() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              legend.title = element_blank(),
              legend.position = "bottom",
              plot.title = element_text(size = 10, colour = "#F0FFFF", face = "bold"),
              plot.background = element_rect(fill = '#222222', colour = '#222222'),
              panel.background = element_rect(fill = '#222222', colour = '#222222'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks = element_blank(),
              legend.background = element_rect(fil = '#222222', colour = '#222222'),
              legend.text = element_text(size = 9, colour = "#F0FFFF",vjust = 5)
        )+
        xlab("")+
        ylab("")+
        ggtitle(as.character(RuneSample[i]))+
        scale_fill_discrete(breaks=array((runes[[i]][,1])))
  }
  
  
if (length(RuneSample) == 1) mp <- multiplot(plots[[1]], cols = 1)
if (length(RuneSample) == 2) mp <- multiplot(plots[[1]], plots[[2]], cols = 1)
if (length(RuneSample) == 3) mp <- multiplot(plots[[1]], plots[[2]], plots[[3]], cols = 1)
if (length(RuneSample) == 4) mp <- multiplot(plots[[1]], plots[[2]], plots[[3]], plots[[4]], cols = 1)

  
  return(mp)
})







