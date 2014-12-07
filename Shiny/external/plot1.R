


plot1 <- function(GRF, GRT, Class){


# sets parameters for each class
  poes<- Class
if(Class == "Barbarian") {
  class <- "barb";bcol ="#DB3232"
  } else if(Class == "Crusader") {
    class <- "crusader";bcol ="#F2EEB1"
  } else if(Class == "DemonHunter") {
    class <- "dh";bcol ="#B058AE"
  } else if(Class == "Monk") {
    class <- "monk";bcol ="#E3DE54"
  } else if(Class == "Witch Doctor") {
    class <- "wd";bcol ="#2799423"
  } else if(Class == "Wizard") {
    class <- "wizard";bcol ="#3BDCF5"
  }


BData <- read.csv(paste("C:/Users/Veldrin/Documents/GitHub/LadderStats/Shiny/external/data/",Class,".csv", sep=""))
BData.subset <- BData[c(as.integer(GRF):(as.integer(GRT)*10)),]
BData.subset.active <- BData.subset[which(BData.subset$Rune != "Passive"),]
BData.subset.passive <- BData.subset[which(BData.subset$Rune == "Passive"),]



t <- data.frame(t(table(BData.subset.active$Spell)))
t$Var1 <- NULL
colnames(t) <- (c("Spell", "Count"))
t$Count <- as.integer(t$Count)
t <- t[order(t$Count, decreasing = T),]
t <- t[1:25,]

# Exponential fit
# exptemp <- data.frame(y = t$Count, x = seq(t$Spell))
# mod <- nls(y ~ exp(a + b * x), data = exptemp, start = list(a = 0, b = 0))
# lines(exptemp$x, predict(mod, list(x = exptemp$x)))
# fittedline <- data.frame(predict(mod, list(x = exptemp$x)))

p1 <- ggplot(t, aes(x=reorder(Spell, -Count), y= Count))+ 
  geom_bar(stat = "identity", fill = bcol, col = "black")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Spell")+
  ylab("Frequency")
 # geom_line(data = fittedline, aes(y = predict.mod..list.x...exptemp.x.., group=1), col = bcol, lwd = 1.5, alpha = .75, linetype="dashed")

return(p1)
}
