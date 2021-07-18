library(tidyverse)

data <- read.csv("~/Downloads/FanGraphs Leaderboard-45.csv")

team_sums <- read.csv("~/Downloads/FanGraphs Leaderboard-46.csv")
team_sums[31,1]<- "- - -"
team_sums[31,4:6]<- 0

no_team <- data[data$Team == "- - -",]

#adames
data[145, 2] <- "TOR"
data[145,5:7] <- c(5,16,15)
data[1190, 1:2] <- c("Willy Adames","MIL")
data[1190, 5:7] <- c(3,6,11)
#mckinney
data[255, 2] <- "MIL"
data[255,5:7] <- c(3,9,6)
data[1191, 1:2] <- c("Billy McKinney","NYM")
data[1191, 5:7] <- c(2,5,6)
#TAUCHMAN
data[276, 2] <- "NYY"
data[276,5:7] <- c(0,1,0)
data[1192, 1:2] <- c("Mike Tauchman","SFG")
data[1192, 5:7] <- c(3,17,12)
#KEVAN SMITH
data[334, 2] <- "TBR"
data[334,5:7] <- c(0,2,0)
data[1193, 1:2] <- c("Kevan Smith","ATL")
data[1193, 5:7] <- c(0,2,1)
#Nottingham
data[374, 2] <- "MIL"
data[374,5:7] <- c(2,2,4)
data[1194, 1:2] <- c("Jacob Nottingham","SEA")
data[1194, 5:7] <- c(1,3,2)
#Pujols
data[1109, 2] <- "LAA"
data[1109,5:7] <- c(5,9,12)
data[1195, 1:2] <- c("Albert Pujols","LAD")
data[1195, 5:7] <- c(4,5,12)
#TOM
data[1071, 2] <- "OAK"
data[1071,5:7] <- c(0,1,1)
data[1196, 1:2] <- c("Ka'ai Tom","PIT")
data[1196, 5:7] <- c(2,7,10)
#yoshi
data[1164, 2] <- "TBR"
data[1164,5:7] <- c(0,5,5)
data[1197, 1:2] <- c("Yoshi Tsutsugo","LAD")
data[1197, 5:7] <- c(0,2,2)
#gamel
data[1132, 2] <- "CLE"
data[1132,5:7] <- c(0,1,0)
data[1198, 1:2] <- c("Ben Gamel","PIT")
data[1198, 5:7] <- c(0,6,3)
#Ildemaro Vargas
data[1153, 2] <- "CHC"
data[1153,5:7] <- c(0,3,2)
data[1199, 1:2] <- c("Ildemaro Vargas","PIT")
data[1199, 5:7] <- c(0,0,1)
data[1200, 1:2] <- c("Ildemaro Vargas","ARI")
data[1200, 5:7] <- c(0,0,3)
#mayfield
data[1125, 2] <- "LAA"
data[1125,5:7] <- c(0,0,0)
data[1201, 1:2] <- c("Jack Mayfield","SEA")
data[1201, 5:7] <- c(0,2,3)
#Travis Blankenhorn
data[831, 2] <- "MIN"
data[831,5:7] <- c(0,1,0)
data[1202, 1:2] <- c("Travis Blankenhorn","NYM")
data[1202, 5:7] <- c(0,0,0)

data$tR <- NA

for (i in 1:nrow(data)) {
  team <- data[i, 2] 
  truns <- team_sums$R[which(team_sums$Team == team)]
  data[i, "tR"] <- truns
}

data$tRprop <- 100*(data$R + data$RBI - data$HR)/data$tR
