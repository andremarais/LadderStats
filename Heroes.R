
herospells <- function(BTN, ID) {
herostats <- data.frame(matrix(NA))

#download profile data----
url <- paste("http://EU.battle.net/api/d3/profile/" , BTN, "/hero/",ID , sep = "")
hero <- httpGET(url,curl = getCurlHandle())


doc <- htmlParse(hero, asText=TRUE)
plain.text <- xpathSApply(doc, "//p", xmlValue)
hero<-capture.output(cat(paste(plain.text, collapse = "\n")))

#last stats is where (not the) gold is
s <- grep("skills\\\" : \\{", hero)
e <- grep("\\\"head\\\" : \\{", hero)

#elites <- hero.subset[grep("elites\\\" :", hero.subset )]
hero.subset  <- hero[s:e]


h <- hero.subset[grep("\\\"name\\\"", hero.subset)]
h <- gsub("        \\\"name\\\" : \\\"", "",h)
h <- gsub("\\\",","",h)

skills <- data.frame(matrix(NA))
skills[c(1:10),1] <- h[c(1+0:5*2,13:16)]
skills[c(1:6),2] <- h[c(1:6*2)]
skills[c(7:10),2] <- "Passive"

colnames(skills) <- c("Spell", "Rune")

return(skills)
}
