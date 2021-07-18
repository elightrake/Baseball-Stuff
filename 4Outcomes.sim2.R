library(tidyverse)

data <- read.csv("~/Downloads/FanGraphs Leaderboard-43.csv")

pHR <- 0.071
pRB <- 0.319
pSO <- 0.232
pIPO <- 0.378
sum(pHR, pRB, pSO, pIPO) == 1

chisq <- function(HR, RB, SO, IPO){
  a <- (HR - pHR)^2
  b <- (RB - pRB)^2
  c <- (SO - pSO)^2
  d <- (IPO - pIPO)^2
  return(round(a + b + c + d,5))
}

tab <- data %>%
  select(Name, PA, HR, K., OBP) %>%
  mutate(HRpct = round(HR / PA, 3),
         RBpct = round(OBP - HRpct, 3),
         SOpct = as.numeric(sub("%", "",data$K.,fixed=TRUE))/100,
         IPOpct = 1 - OBP - SOpct,
         ChiVal = chisq(HRpct, RBpct, SOpct, IPOpct)) %>%
  select(Name, ChiVal, HRpct, RBpct, SOpct, IPOpct) %>%
  arrange(ChiVal)

nums4 <- function(Player){
  row <- which(data$Name == Player)
  return(tab[row,3:6])
}

sim2 <- function(Player){
  nums <- nums4(Player)
  pHR <- nums[[1]]
  pRB <- nums[[2]]
  pSO <- nums[[3]]
  pIPO <- nums[[4]]
  sim <- data %>%
    select(Name, PA, HR, K., OBP) %>%
    mutate(HRpct = round(HR / PA, 3),
           RBpct = round(OBP - HRpct, 3),
           SOpct = as.numeric(sub("%", "",data$K.,fixed=TRUE))/100,
           IPOpct = 1 - OBP - SOpct,
           ChiVal = (HRpct - pHR)^2 + (RBpct - pRB)^2 + 
             (SOpct - pSO)^2 + (IPOpct - pIPO)^2) %>%
    select(Name, ChiVal, HRpct, RBpct, SOpct, IPOpct) %>%
    arrange(ChiVal)
  return(sim)
}

#MOST SIMILAR PLAYERS
min <- 1
for (i in 1:nrow(data)) {
  Player <- tab[[i,1]]
  sim <- sim2(Player)
  diff <- sort(sim$ChiVal)[2]
  if (diff < min) {
    min <- diff
    row <- i
  }
}
tab[row,1]
View(sim2(tab$Name[which]))

#MOST DIFFERENT PLAYERS
max <- 0
for (i in 1:nrow(data)) {
  Player <- tab[[i,1]]
  sim <- sim2(Player)
  diff <- sort(sim$ChiVal, decreasing = T)[1]
  if (diff > max) {
    max <- diff
    row <- i
  }
}
tab[row,1]
View(sim2(tab$Name[row]))


#MOST UNIQUE PLAYERS
max <- 0
for (i in 1:nrow(data)) {
  Player <- tab[[i,1]]
  sim <- sim2(Player)
  diff <- sort(sim$ChiVal)[2]
  if (diff > max) {
    max <- diff
    row <- i
  }
}
tab[row,1]
View(sim2(tab$Name[row]))




