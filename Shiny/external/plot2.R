spells <- t[1:6,1]

i <- 1
runes <- list(list())

for (i in 1:6) {

runes.subset <- mscv[which(mscv$Spell == spells[i]),c(2,3)]


runes.subset[,2] <- factor(runes.subset[,2])

runes.subset <- data.frame(t(table(runes.subset[2])))[,2:3]

runes[i] <- list(runes.subset[order(runes.subset$Freq, decreasing = T),])
colnames(runes[[i]]) <- c("Rune", "Frequency")

}
f <- runes[[1]]
ggplot(data = f, aes(x = reorder(f$Rune, - f$Frequency), y = f$Frequency), fill = f$Rune) + geom_bar(stat = "identity", position = "stack") + coord_flip()
