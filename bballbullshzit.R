library(Lahman)
library(tidyverse)

bball <- batting %>%
  filter(yearID %in% 2005:2015) %>%
  group_by(yearID, teamID) %>%
  summarize(tOBP = mean(OBP), tBA = mean(BA), twOBA = mean(wOBA))


teams <- Teams %>%
  filter(yearID %in% 2005:2015) %>%
  select(yearID, teamID, W)

bball$W <- teams$W

ggplot(bball) +
  geom_point(aes(x = tOBP, y = W)) +
  geom_point(aes(x = tBA, y = W), col = 'blue') +
  geom_point(aes(x = twOBA, y = W), alpha = 0.3, col = 'red') +
  labs(title = "Wins against Team BA/OBP/wOBA", 
       subtitle = "BA - blue, OBP - black, wOBA - red") +
  xlab("")

cor(bball$tOBP, bball$W)
cor(bball$tBA, bball$W)
cor(bball$twOBA, bball$W)


fgweights <- read_csv("~/Downloads/FanGraphs Leaderboard-6.csv")
ggplot(fgweights) +
  geom_point(aes(x = Season, y = wBB), alpha = 0.5, col = 'red') +
  geom_point(aes(x = Season, y = wHBP), alpha = 0.5, col = 'orange') +
  geom_point(aes(x = Season, y = w1B), alpha = 0.5, col = 'yellow') +
  geom_point(aes(x = Season, y = w2B), alpha = 0.5, col = 'green') +
  geom_point(aes(x = Season, y = w3B), alpha = 0.5, col = 'blue') +
  geom_point(aes(x = Season, y = wHR), alpha = 0.5, col = 'purple') +
  #geom_point(aes(x = Season, y = wOBA)) +
  ylab("")


stats2020 <- read.csv("~/Downloads/FanGraphs Leaderboard-11.csv")
projs2020 <- read.csv("~/Downloads/FanGraphs Leaderboard-12.csv")

togo2020 <- projs2020 %>%
  filter(projs2020$Name %in% stats2020$Name) %>%
  arrange(Name) %>%
  select(AB, AVG)

sofar2020 <- stats2020 %>%
  arrange(Name) %>%
  mutate(BBP = as.numeric(strsplit(BB., "%"))/100,
         AB = round(PA*(1-BBP))) %>%
  select(Name, AB, AVG)

ovrBA <- function(goal, sfAB, sfBA, tgAB, skill){
  goal.hits <- goal*(sfAB + tgAB)
  tg.hits <- round(goal.hits - (sfBA * sfAB))
  h.vec <- seq(tg.hits, tgAB, by = 1)
  chance <- sum(dbinom(h.vec, tgAB, skill))
  return(chance)
}

projs <- cbind(sofar2020, togo2020)
colnames(projs) <- c("Name", "AB1", "AVG1", "AB2", "AVG2")
projs <- projs %>%
  mutate(pAVG = AVG1 * (AB1/(AB1 + AB2)) + AVG2 * (AB2/(AB1 + AB2)),
         ovr400 = sum(dbinom(seq(round(.4*(AB1 + AB2) - (AB1 * AVG1)), AB2, 1), AB2, AVG2)))


data2019 <- read.csv("~/Downloads/FanGraphs Leaderboard-13.csv")


teamhrs <- data2019 %>%
  group_by(Team) %>%
  mutate(tHRperc = HR / sum(HR))

datapitching <- Pitching %>%
  group_by(playerID) %>%
  summarize(W = sum(W), HR = sum(HR), rYear = max(yearID), true = ifelse(W > HR, 1, 0)) %>%
  filter(W > 49) %>%
  arrange(true, rYear)

sav <- read.csv("~/Downloads/savant_data (3).csv") %>%
  group_by(player_name) %>%
  mutate(count = n()) %>%
  filter(balls == 0, count > 1) %>%
  select(player_name, game_date)

hrs <- read.csv("~/Downloads/HR Tracker.csv")

hrdata <- hrs %>%
  group_by(BATTER) %>%
  summarize(HR = n(), DATE = max(GAME_DATE))
  
bp2020 <- read.csv("~/Downloads/FanGraphs Leaderboard (1).csv")

bpdata <- bp2020 %>%
  select(Team, IP, BABIP, ERA, FIP, xFIP, WAR) %>%
  mutate(sBABIP = standardize(BABIP),
         sERA = standardize(ERA),
         sFIP = standardize(FIP),
         sxFIP = standardize(xFIP),
         sWAR = standardize(WAR)) %>%
  select(Team, IP, BABIP, sBABIP, ERA, sERA, FIP, sFIP, xFIP, sxFIP, WAR, sWAR)


bat <- Batting %>%
  filter(yearID == 1993, lgID == "AL", AB > 199) %>%
  mutate(BA = H/AB) %>%
  select(playerID, BA, AB)

data8.31 <- read.csv("~/Downloads/FanGraphs Leaderboard-14.csv")

spec831 <- data8.31 %>%
  select(Name, AVG, OBP) %>%
  mutate(Respec = OBP - AVG)

rioruiz <- read.csv("~/Downloads/savant_data-6.csv")

rioruiz %>% select(xba, xwoba) %>% summarize(xb = mean(xba), xw = mean(xwoba))

chuck <- read.csv("~/Downloads/FanGraphs Leaderboard-18.csv")

newd <- chuck %>% select(Name, H, SO) %>% mutate(val = H/SO)

hrvelo <- read.csv("~/Downloads/savant_data-7.csv")

hrdata <- hrvelo %>%
  select(game_date, player_name, pitcher, batter, 
         launch_speed, launch_angle, hit_distance_sc) %>%
  group_by(batter) %>%
  summarize(HR = n(),
            avgEV = mean(launch_speed),
            avgLA = mean(launch_angle),
            avgDist = mean(hit_distance_sc))
  

sep19 <- read.csv("~/Downloads/FanGraphs Leaderboard-30.csv")

pHR <- 0.21
pRB <- 0.46
pSO <- 0.12
pIPO <- 0.21

chisq <- function(HR, RB, SO, IPO){
  a <- (HR - pHR)^2
  b <- (RB - pRB)^2
  c <- (SO - pSO)^2
  d <- (IPO - pIPO)^2
  return(round(a + b + c + d,3))
}

tab <- sep19 %>%
  select(Name, PA, HR, K., OBP) %>%
  mutate(HRpct = round(HR / PA, 3),
         RBpct = round(OBP - HRpct, 3),
         SOpct = as.numeric(sub("%", "",sep9$K.,fixed=TRUE))/100,
         IPOpct = 1 - OBP - SOpct,
         ChiVal = chisq(HRpct, RBpct, SOpct, IPOpct)) %>%
  select(Name, ChiVal, HRpct, RBpct, SOpct, IPOpct)

nums4 <- function(Player){
  row <- which(sep19$Name == Player)
  return(tab[row,3:6])
}

sim2 <- function(Player){
  nums <- nums4(Player)
  pHR <- nums[[1]]
  pRB <- nums[[2]]
  pSO <- nums[[3]]
  pIPO <- nums[[4]]
  sim <- sep19 %>%
    select(Name, PA, HR, K., OBP) %>%
    mutate(HRpct = round(HR / PA, 3),
           RBpct = round(OBP - HRpct, 3),
           SOpct = as.numeric(sub("%", "",sep9$K.,fixed=TRUE))/100,
           IPOpct = 1 - OBP - SOpct,
           ChiVal = (HRpct - pHR)^2 + (RBpct - pRB)^2 + 
                    (SOpct - pSO)^2 + (IPOpct - pIPO)^2) %>%
    select(Name, ChiVal, HRpct, RBpct, SOpct, IPOpct)
  return(sim)
}

min <- 1
for (i in 1:153) {
  Player <- tab[[i,1]]
  sim <- sim2(Player)
  diff <- sort(sim$ChiVal)[2]
  if (diff < min) {
    min <- diff
    row <- i
  }
}
View(sim2(tab$Name[row]))


vec <- NA
sim <- 20
for (j in 1:sim) {
  vec[j] <- (j+1)^2/j^2
}
vec[sim]

swings <- read.csv("~/Downloads/savant_data-11.csv")
total <- read.csv("~/Downloads/savant_data-12.csv")

swingperc <- total %>%
  select(player_name, swings, takes) %>%
  mutate(total = swings + takes, swingp = swings/total) %>%
  filter(total > 99)

hr.rate <- 7/188
n.sim <- 10^5
hr.vec <- NA
for (i in 1:n.sim) {
  hrs <- sum(runif(4) <= hr.rate)
  hr.vec[i] <- hrs
}
100*table(hr.vec)/n.sim


tw19 <- read.csv("~/Downloads/FanGraphs Leaderboard-34.csv")

players2 <- tw19 %>%
  select(Name, PA, HR, BB) %>%
  mutate(HRminBB = HR - BB) %>%
  arrange(-HRminBB)

fieldingss <- read.csv("~/Downloads/FanGraphs Leaderboard-37.csv")

fieldingss <- fieldingss %>%
  mutate(DRSperIP = DRS/Inn)

