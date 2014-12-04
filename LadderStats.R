library(shiny)
pkgs <- c("RCurl","XML","ggplot2", "plyr")
pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs)) install.packages(pkgs,repos="http://cran.cs.wwu.edu/")
library(RCurl); library(XML); library(ggplot2); library(plyr)

fastbind.ith.rows <- function(i) rbindlist(lapply(sample.list, "[", i, TRUE))




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
  for (i in 2:6) {
  
    heroplace[i,j] <- as.integer(gregexpr(paste(">\n",i,"\\.", sep= ""), substring(classlist[j], heroplace[i-1,j], nchar(classlist[j])))) + heroplace[i-1,j] -1
  }
}
herolist


profilenames <- data.frame(matrix(NA))


#finds the profile names
for (j in 1:6) {
  for (i in 1:5) {
    proiflename.start <- gregexpr("/profile/", substring(classlist[j], heroplace[i,j], heroplace[i+1,j])) 
    profilename.end <- gregexpr("/\\\" title=", substring(classlist[j], heroplace[i,j], heroplace[i+1,j]))
    profilenames[i,j] <- substring(classlist[j], heroplace[i,j]+as.integer(proiflename.start) + as.integer(attributes(proiflename.start[[1]])[1])-1
                                 , heroplace[i,j] + as.integer(profilename.end) -2 )[1]
  }
}

colnames(profilenames) <- c("barbarian", "crusader", "demon-hunter", "monk", "witch-doctor", "wizard")



spells <- list()
jspell <- data.frame()

for (i in 1:3){
  for (j in 1:6){
    b <- herospells(profilenames[i,1],HeroIDs(profilenames[i,1], colnames(profilenames)[1])[1])
    c <- herospells(profilenames[i,2],HeroIDs(profilenames[i,2], colnames(profilenames)[2])[1])
  }
  spells[[i]] <- cbind(b,c)
}




a <- lapply(spells, `[`,  1)







i <- 2



# 
# 
# spells <- list(list(matrix(data = NA, nrow = 10, ncol = 2)))
# 
# #list(herospells(profilenames[i,j],HeroIDs(profilenames[i,j], colnames(profilenames)[j])[1]))
# 
# 
# b1 <- herospells(profilenames[1,1],HeroIDs(profilenames[1,1], colnames(profilenames)[1])[1])
# b2 <- herospells(profilenames[2,1],HeroIDs(profilenames[2,1], colnames(profilenames)[1])[1])
# b3 <- herospells(profilenames[3,1],HeroIDs(profilenames[3,1], colnames(profilenames)[1])[1])
# 
# c1 <- herospells(profilenames[1,2],HeroIDs(profilenames[1,2], colnames(profilenames)[2])[1])
# c2 <- herospells(profilenames[2,2],HeroIDs(profilenames[2,2], colnames(profilenames)[2])[1])
# c3 <- herospells(profilenames[3,2],HeroIDs(profilenames[3,2], colnames(profilenames)[2])[1])
# 
# 
# #system.time(fastbound <- lapply(1:nr, fastbind.ith.rows))
# 
# 
# #lapply(fok, `[`,  1)
# #do.call(rbind,data)
# n1 <- lapply(fok, `[`,  1)
# n2 <- lapply(fok, `[`,  2)
# do.call(rbind,n1)
# ldply(n1)
# 
# fok <- list()
# fok[[1]] <- list(b1,c1)
# fok[[2]] <- list(b2,c2)
# fok[[3]] <- list(b3,c3)
# 
#                  
# a <- data.frame(y1,y2,y3)
# a$X1
# factor(a$X1)
# str(a)
