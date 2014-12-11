library(shiny)
pkgs <- c("RCurl","XML","ggplot2")
pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs)) install.packages(pkgs,repos="http://cran.cs.wwu.edu/")
library(RCurl); library(XML); library(ggplot2);



#how many profiles


urlB <- "http://eu.battle.net/d3/en/rankings/era/1/rift-barbarian"
urlC <- "http://eu.battle.net/d3/en/rankings/era/1/rift-crusader"
urlDH <- "http://eu.battle.net/d3/en/rankings/era/1/rift-dh"
urlM <- "http://eu.battle.net/d3/en/rankings/era/1/rift-monk"
urlWD <- "http://eu.battle.net/d3/en/rankings/era/1/rift-wd"
urlW <- "http://eu.battle.net/d3/en/rankings/era/1/rift-wizard"

barbarian <- httpGET(urlB,curl = getCurlHandle())
crusader <- httpGET(urlC,curl = getCurlHandle())
dh <- httpGET(urlDH,curl = getCurlHandle())
monk <- httpGET(urlM,curl = getCurlHandle())
wd <- httpGET(urlWD,curl = getCurlHandle())
wizard <- httpGET(urlW,curl = getCurlHandle())


classlist <- c(barbarian, crusader, dh, monk, wd, wizard)

heroplace <- data.frame(matrix(NA))
heroplace[1,1] <- gregexpr(paste(">\n",1,"\\.", sep= ""), classlist[1])
heroplace[1,2] <- gregexpr(paste(">\n",1,"\\.", sep= ""), classlist[2])
heroplace[1,3] <- gregexpr(paste(">\n",1,"\\.", sep= ""), classlist[3])
heroplace[1,4] <- gregexpr(paste(">\n",1,"\\.", sep= ""), classlist[4])
heroplace[1,5] <- gregexpr(paste(">\n",1,"\\.", sep= ""), classlist[5])
heroplace[1,6] <- gregexpr(paste(">\n",1,"\\.", sep= ""), classlist[6])


## finds all the locations of where the hero names appear ----
for (j in 1:6) {
  for (i in 2:1000) {
  
    heroplace[i,j] <- as.integer(gregexpr(paste(">\n",i,"\\.", sep= ""), substring(classlist[j], heroplace[i-1,j], nchar(classlist[j])))) + heroplace[i-1,j] -1
  }
}


profilenames <- data.frame(matrix(NA))


#finds the profile names
for (j in 1:6) {
  for (i in 1:999) {
    proiflename.start <- gregexpr("/profile/", substring(classlist[j], heroplace[i,j], heroplace[i+1,j])) 
    profilename.end <- gregexpr("/\\\" title=", substring(classlist[j], heroplace[i,j], heroplace[i+1,j]))
    profilenames[i,j] <- substring(classlist[j], heroplace[i,j]+as.integer(proiflename.start) + as.integer(attributes(proiflename.start[[1]])[1])-1
                                 , heroplace[i,j] + as.integer(profilename.end) -2 )[1]
  }
}

colnames(profilenames) <- c("barbarian", "crusader", "demon-hunter", "monk", "witch-doctor", "wizard")



spells <- list()
jspell <- data.frame()

for (i in 1:999){

    b <- herospells(profilenames[i,1],HeroIDs(profilenames[i,1], colnames(profilenames)[1])[1])
    c <- herospells(profilenames[i,2],HeroIDs(profilenames[i,2], colnames(profilenames)[2])[1])
    dh <- herospells(profilenames[i,3],HeroIDs(profilenames[i,3], colnames(profilenames)[3])[1])
    m <- herospells(profilenames[i,4],HeroIDs(profilenames[i,4], colnames(profilenames)[4])[1])
    wd <- herospells(profilenames[i,5],HeroIDs(profilenames[i,5], colnames(profilenames)[5])[1])
    w <- herospells(profilenames[i,6],HeroIDs(profilenames[i,6], colnames(profilenames)[6])[[1]])

  spells[[i]] <- cbind(b,c,dh,m,wd,w)
  print(i)
}

getwd()
setwd("c:/Diablo")

write.csv(spells, file = "spells.csv")
# barbarian spells
bspells <- matrix(NA)
b <- lapply(spells, `[`,  c(1,2))
bspells <- b[[1]]
for(i in 2:length(spells)) bspells <- rbind(bspells,b[[i]])
write.csv(bspells, file = "barb.csv")

# crusaders spells
cspells <- matrix(NA)
c <- lapply(spells, `[`,  c(3,4))
cspells <- c[[1]]
for(i in 2:length(spells)) cspells <- rbind(cspells,c[[i]])
write.csv(cspells, file = "crusader.csv")

# Demom-Hunter spells
dhspells <- matrix(NA)
dh <- lapply(spells, `[`,  c(5,6))
dhspells <- dh[[1]]
for(i in 2:length(spells)) dhspells <- rbind(dhspells,dh[[i]])
write.csv(dhspells, file = "dh.csv")


# Monk Spells
mspells <- matrix(NA)
m <- lapply(spells, `[`,  c(7,8))
mspells <- m[[1]]
for(i in 2:length(spells)) mspells <- rbind(mspells,m[[i]])
write.csv(mspells, file = "monk.csv")

# Witch Doctor spells
wdspells <- matrix(NA)
wd <- lapply(spells, `[`,  c(9,10))
wdspells <- wd[[1]]
for(i in 2:length(spells)) wdspells <- rbind(wdspells,wd[[i]])
write.csv(wdspells, file = "wd.csv")

# Wizard spells
wspells <- matrix(NA)
w <- lapply(spells, `[`,  c(11,12))
wspells <- w[[1]]
for(i in 2:length(spells)) wspells <- rbind(wspells,w[[i]])
write.csv(wspells, file = "wizard.csv")




