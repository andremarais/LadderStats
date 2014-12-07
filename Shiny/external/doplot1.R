
# BData <- read.csv(paste("external/data/",input$ClassSelect,".csv", sep=""))
# BData.subset <- BData[c(input$sliderInput[1]:(input$sliderInput[2]*10)),]
BData <- read.csv(paste("external/data/","Monk",".csv", sep=""))
BData.subset <- BData[c(1:(1000*10)),]
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

ggplot(t, aes(x=reorder(Spell, -Count), y= Count))+ 
  geom_bar(stat = "identity", fill = "#F2EEB1", col = "black")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Spell")+
  ylab("Frequency")
 # geom_line(data = fittedline, aes(y = predict.mod..list.x...exptemp.x.., group=1), col = bcol, lwd = 1.5, alpha = .75, linetype="dashed")


