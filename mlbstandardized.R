library(tidyverse)
library(Lahman)
library(ggplot2)
library(ggthemes)
library(gridExtra)

batting <- as_tibble(Batting)
### BATTING
batting <- batting %>%
  replace_na(list(IBB = 0, HBP = 0, SH = 0, SF = 0, GIDP = 0)) %>%
  mutate(PA = AB + BB + HBP + SH + SF,
         uBB = BB - IBB,
         X1B = H - X2B - X3B - HR,
         BA = H / AB,
         OBP = (H + BB + HBP)/(AB + BB + HBP + SF),
         SLG = (X1B + 2*X2B + 3*X3B + 4*HR)/AB,
         OPS = OBP + SLG,
         wOBA = (0.687*uBB + 0.718*HBP + 0.881*X1B + 
                  1.256*X2B + 1.594*X3B + 2.065*HR)/
                  (AB + uBB + SF + HBP)) %>%
  filter(PA > 501, lgID == "AL" | lgID == "NL") %>%
  select(playerID, yearID, lgID, teamID, PA, BA, OBP, OPS, wOBA, HR) 


standardize <- function(x){
  (x - mean(x, na.rm = T))/sd(x, na.rm = T)
  }
batting <- batting %>%
  mutate(zBA_all = standardize(BA),
         zOBP_all = standardize(OBP),
         zOPS_all = standardize(OPS),
         zwOBA_all = standardize(wOBA))

batting <- batting %>%
  group_by(yearID) %>%
  mutate(zBA_year = standardize(BA),
         zOBP_year = standardize(OBP),
         zOPS_year = standardize(OPS),
         zwOBA_year = standardize(wOBA))

batting <- batting %>%
  ungroup() %>% 
  group_by(yearID) %>%
  mutate(zBA_year_lg = standardize(BA),
         zOBP_year_lg = standardize(OBP),
         zOPS_year_lg = standardize(OPS),
         zwOBA_year_lg = standardize(wOBA))

batting <- batting %>% 
  ungroup() %>%
  mutate(HIST_ERA = case_when(
    1871 <= yearID & yearID <= 1892 ~ "Pioneer",
    1893 <= yearID & yearID <= 1919 ~ "Spitball",
    1920 <= yearID & yearID <= 1946 ~ "Landis",
    1947 <= yearID & yearID <= 1968 ~ "Baby Boomer",
    1969 <= yearID & yearID <= 1992 ~ "Artifical Turf",
    1993 <= yearID ~ "Camden Yards")) %>%
  group_by(HIST_ERA) %>%
  mutate(zBA_hist = standardize(BA),
         zOBP_hist = standardize(OBP),
         zOPS_hist = standardize(OPS),
         zwOBA_hist = standardize(wOBA)) %>%
  ungroup()



pitching <- as_tibble(Pitching)
### PITCHING

pitching <- pitching %>%
  replace_na(list(IBB = 0, BAOpp = 0, SH = 0, SF = 0, GIDP = 0)) %>%
  filter(IPouts > 485, lgID == "AL" | lgID == "NL") %>%
  select(playerID, yearID, lgID, teamID, IPouts, HR, BB, SO, BAOpp, ERA, SHO) %>%
  mutate(zIPouts_all = standardize(IPouts),
         zHR_all = standardize(HR),
         zBB_all = standardize(BB),
         zSO_all = standardize(SO),
         zBAOpp_all = standardize(BAOpp),
         zERA_all = standardize(ERA),
         zSHO_all = standardize(SHO)) %>%
  group_by(yearID) %>%
  mutate(zIPouts_year = standardize(IPouts),
         zHR_year = standardize(HR),
         zBB_year = standardize(BB),
         zSO_year = standardize(SO),
         zBAOpp_year = standardize(BAOpp),
         zERA_year = standardize(ERA),
         zSHO_year = standardize(SHO)) %>%
  ungroup() %>%
  mutate(HIST_ERA = case_when(
    1871 <= yearID & yearID <= 1892 ~ "Pioneer",
    1893 <= yearID & yearID <= 1919 ~ "Spitball",
    1920 <= yearID & yearID <= 1946 ~ "Landis",
    1947 <= yearID & yearID <= 1968 ~ "Baby Boomer",
    1969 <= yearID & yearID <= 1992 ~ "Artifical Turf",
    1993 <= yearID ~ "Camden Yards")) %>%
  group_by(HIST_ERA) %>%
  mutate(zIPouts_hist = standardize(IPouts),
         zHR_hist = standardize(HR),
         zBB_hist = standardize(BB),
         zSO_hist = standardize(SO),
         zBAOpp_hist = standardize(BAOpp),
         zERA_hist = standardize(ERA),
         zSHO_hist = standardize(SHO)) %>%
  ungroup()







             
relative <- function(x){
  med <- median(x, na.rm = T)
  return(x/med)
}

mlb_payrolls <- read_csv("~/Downloads/mlb_payrolls.csv")

mlb_payrolls <- mlb_payrolls %>%
  group_by(Year) %>%
  mutate(Relative_Payroll = relative(Team_Payroll))

ggplot(mlb_payrolls) +
  geom_point(aes(x = Relative_Payroll, y = Winning_Percentage)) +
  labs(x = "Relative $", y = "Winning %")

payroll_avg <- mlb_payrolls %>%
  summarise(avg.team.payroll = mean(Team_Payroll),
            avg.rel.payroll = mean(Relative_Payroll))

tp <- ggplot(mlb_payrolls) +
  geom_point(aes(x = Year, y = Team_Payroll)) +
  labs(title = "Team Payroll Over Time",
       y = "Team Payroll (millions)") +
  scale_y_continuous(labels = c(0, 1, 2, 3)) +
  geom_line(data = payroll_avg, 
            aes(x = Year, y = avg.team.payroll), 
            col = 'red')
rp <- ggplot(mlb_payrolls) +
  geom_point(aes(x = Year, y = Relative_Payroll)) +
  labs(title = "Relative Payroll Over Time",
       y = "Relative Payroll") +
  scale_y_continuous(labels = c(0, 1, 2, 3)) +
  geom_line(data = payroll_avg, 
            aes(x = Year, y = avg.rel.payroll), 
            col = 'blue')
grid.arrange(tp, rp, nrow = 2, ncol = 1)

hist(batting$OBP, col = 'blue')

cors <- mlb_payrolls %>%
  summarise(cor = cor(Relative_Payroll, Winning_Percentage))

batting_2011_2015 <- batting %>%
  filter(yearID %in% 2011:2015) %>%
  group_by(playerID) %>%
  filter(n() == 5) %>%
  select(playerID, yearID, BA) %>%
  arrange(playerID)
  
pred15 <- batting_2011_2015 %>%
  spread(yearID, BA) %>%
  rename(BA11 = `2011`, BA12 = `2012`, BA13 = `2013`, BA14 = `2014`, BA15 = `2015`)

balm <- lm(BA15 ~ .-playerID, pred15)
summary(balm)
pred15$Preds <- predict(balm) 
pred15 <- pred15 %>%
  mutate(Err = abs(BA15 - Preds))
mean(pred15$Err)
sd(pred15$Err)
cor(pred15$BA15, pred15$Preds)
ggplot(pred15) +
  geom_point(aes(x = Preds, y = BA15)) +
  theme_minimal()
