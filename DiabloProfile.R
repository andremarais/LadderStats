
# require(RCurl)
# require(XML)
#setting up variables ----


HeroIDs <- function(BTN, class){

#download profile data----
url <- paste("eu.battle.net/api/d3/profile/" , BTN, "/", sep = "")
diablo <- httpGET(url,curl = getCurlHandle())

doc <- htmlParse(diablo, asText=TRUE)
plain.text <- xpathSApply(doc, "//p", xmlValue)
DProfile<-capture.output(cat(paste(plain.text, collapse = "\n")))


#find hero section:
heroesS <- grep("heroes", DProfile)
heroesE <- grep("\\]", DProfile[1:length(DProfile)])
heroes <- DProfile[heroesS:heroesE[1]]

# to find heroe names
hname <- heroes[grep("name", heroes)]
hname <- gsub("    \\\"name\\\" : \\\"", "",hname)
hname <- gsub("\\\",","",hname)

# to find hero IDs
hid <- heroes[grep("\"id\\\"", heroes)]
hid <- gsub("    \\\"id\\\" : ", "", hid)
hid <- gsub(",", "", hid)


# to find hero levels
hlevel <- heroes[grep("\"level\\\"", heroes)]
hlevel <- gsub("    \\\"level\\\" : ", "", hlevel)
hlevel <- gsub(",", "", hlevel)

# to find hero class
hclass <- heroes[grep("\"class\\\"", heroes)]
hclass <- gsub("    \\\"class\\\" : \\\"", "", hclass)
hclass <- gsub("\\\",", "", hclass)

# hardcore stat
hhardcore <- heroes[grep("\"hardcore\\\"", heroes)]
hhardcore <- gsub("    \\\"hardcore\\\" : ", "", hhardcore)
hhardcore <- gsub(",", "", hhardcore)



herolist <- data.frame(cbind(hname, hid,hlevel,hclass,hhardcore))
#herolist$hid <- levels(droplevels(herolist$hid))

validheroes <- herolist[which(herolist$hclass == class & herolist$hlevel == 70  & herolist$hhardcore == "false" ),]
validheroes$hid

return(validheroes$hid)
}
