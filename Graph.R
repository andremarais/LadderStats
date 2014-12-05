wdspells

f <- 1
t <- 50





class.range.active <- wdspells[1:500,]
class.range.active <- class.range.active[which(class.range$Rune != "Passive"),]


c <- table(class.range.active$Spell)

c <- c[order(c, decreasing = T)]

r1 <- rownames(c)
r2 <- as.integer(c)

r3 <- rbind(r1,r2)
rownames(r3) <- c("Spell", "Count")


r3 <- data.frame(t(r3))

r3$Count <- as.integer(r3$Count)
sum(r3$Count)

r3 <- r3[order(r3$Count, decreasing = T),]
ggplot(r3, aes(x = reorder(r3$Spell,-r3$Count), y = r3$Count)) + geom_bar(stat= "identity")
