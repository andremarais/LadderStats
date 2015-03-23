
bscv <- read.csv(paste("external/data/barb.csv", sep=""))
cscv <- read.csv(paste("external/data/crusader.csv", sep=""))
dhscv <- read.csv(paste("external/data/dh.csv", sep=""))
mscv <- read.csv(paste("external/data/monk.csv", sep=""))
wdscv <- read.csv(paste("external/data/wd.csv", sep=""))
wscv <- read.csv(paste("external/data/wizard.csv", sep=""))

bscvHC <- read.csv(paste("external/data/barbHC.csv", sep=""))
cscvHC <- read.csv(paste("external/data/crusaderHC.csv", sep=""))
dhscvHC <- read.csv(paste("external/data/dhHC.csv", sep=""))
mscvHC <- read.csv(paste("external/data/monkHC.csv", sep=""))
wdscvHC <- read.csv(paste("external/data/wdHC.csv", sep=""))
wscvHC <- read.csv(paste("external/data/wizardHC.csv", sep=""))

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


plot1 <- function(GRF, GRT, Class = "Barbarian"){
  
if(Class == "Barbarian") {
  bcol ="#DB3232"; if (input$hardcorecb  == F) BData <- bscv else BData <- bscvHC
  } else if(Class == "Crusader") {
    bcol ="#F2EEB1"; if (input$hardcorecb  == F) BData <- cscv else BData <- cscvHC
  } else if(Class == "DemonHunter") {
    bcol ="#B058AE"; if (input$hardcorecb  == F) BData <- dhscv else BData <- dhscvHC
  } else if(Class == "Monk") {
    bcol ="#E3DE54"; if (input$hardcorecb  == F) BData <- mscv else BData <- mscvHC
  } else if(Class == "WitchDoctor") {
    bcol ="#279423"; if (input$hardcorecb  == F) BData <- wdscv else BData <- wdscvHC
  } else if(Class == "Wizard") {
    ;bcol ="#3BDCF5"; if (input$hardcorecb  == F) BData <- wscv else BData <- wscvHC
  }

BData.subset <- BData[c(GRF:(GRT*10)),]
BData.subset.active <- BData.subset[which(BData.subset$Rune != "Passive"),]
BData.subset.passive <- BData.subset[which(BData.subset$Rune == "Passive"),]

t <- data.frame(t(table(BData.subset.active$Spell)))
t$Var1 <- NULL
colnames(t) <- (c("Spell", "Count"))
t$Count <- as.integer(t$Count)
t <- t[order(t$Count, decreasing = T),]
t <- t[1:10,]

s <- data.frame(t(table(BData.subset.passive$Spell)))
s$Var1 <- NULL
colnames(s) <- (c("Passive", "Count"))
s$Count <- as.integer(s$Count)
s <- s[order(s$Count, decreasing = T),]
s <- s[1:8,]


p1 <- ggplot(t, aes(x=reorder(Spell, -Count), y= Count))+ 
  geom_bar(stat = "identity", fill = bcol, col = "black")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12, colour = "#F0FFFF"),
        axis.title.x = element_text(size = 14, colour = "#F0FFFF", face = "bold"),
        axis.title.y = element_text(size = 14, colour = "#F0FFFF", face = "bold"),
        plot.background = element_rect(fill = '#222222', colour = '#222222'),
        panel.background = element_rect(fill = '#222222', colour = '#222222'),
        panel.grid.major = element_line(colour = "#C1CDCD"),
        panel.grid.minor = element_line(colour = "#838B8B"))+
  xlab("Spell")+
  ylab("Frequency")

p2 <- ggplot(s, aes(x=reorder(Passive, -Count), y= Count))+ 
  geom_bar(stat = "identity", fill = bcol, col = "black")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12, colour = "#F0FFFF"),
        axis.title.x = element_text(size = 14, colour = "#F0FFFF", face = "bold"),
        axis.title.y = element_text(size = 14, colour = "#F0FFFF", face = "bold"),
        plot.background = element_rect(fill = '#222222', colour = '#222222'),
        panel.background = element_rect(fill = '#222222', colour = '#222222'),
        panel.grid.major = element_line(colour = "#C1CDCD"),
        panel.grid.minor = element_line(colour = "#838B8B"))+
  xlab("Passive")+
  ylab("")



p <- multiplot(p1,p2, cols = 2)

return(p)
}
