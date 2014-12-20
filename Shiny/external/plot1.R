
bscv <- read.csv(paste("external/data/Barbarian.csv", sep=""))
cscv <- read.csv(paste("external/data/Crusader.csv", sep=""))
dhscv <- read.csv(paste("external/data/DemonHunter.csv", sep=""))
mscv <- read.csv(paste("external/data/Monk.csv", sep=""))
wdscv <- read.csv(paste("external/data/WitchDoctor.csv", sep=""))
wscv <- read.csv(paste("external/data/Wizard.csv", sep=""))

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
  bcol ="#DB3232"; BData <- bscv
  } else if(Class == "Crusader") {
    bcol ="#F2EEB1"; BData <- cscv
  } else if(Class == "DemonHunter") {
    bcol ="#B058AE"; BData <- dhscv
  } else if(Class == "Monk") {
    bcol ="#E3DE54"; BData <- mscv
  } else if(Class == "WitchDoctor") {
    bcol ="#279423"; BData <- wdscv
  } else if(Class == "Wizard") {
    ;bcol ="#3BDCF5"; BData <- wscv
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
