
herospells <- function(BTN, ID) {
  if (ID == "No such hero for this account") {
    NaMatrix <- matrix(nrow = 10, ncol = 2, NA)
    colnames(NaMatrix) <- c("Spell", "Rune")
    return(NaMatrix)}
herostats <- data.frame(matrix(NA))

#download profile data----
url <- paste("http://EU.battle.net/api/d3/profile/" , BTN, "/hero/",ID ,"?locale=en_GB&apikey=mnazmcekrbgx4knstqynhacjf4zcmvh5", sep = "")
hero <- httpGET(url,curl = getCurlHandle())


doc <- htmlParse(hero, asText=TRUE)
plain.text <- xpathSApply(doc, "//p", xmlValue)
hero<-capture.output(cat(paste(plain.text, collapse = "\n")))
#  \"items\" : {

#  \\\"items\\\" : {


#last stats is where (not the) gold is
s <- grep("skills\\\" : \\{", hero)


e <- grep("  \\\"items\\\" : \\{", hero)[1]

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
