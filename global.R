######GLOBAL
# Declare all packages and global variables here
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidyr)
library(dplyr)
library(DT)
library(shinyWidgets)
library(plotly)
library(reshape2)
library(readxl)
library(writexl)
library(rsconnect)
library(shinyBS)
#deployApp()
#To redeploy under new name
#deployApp(appName="NFL_Spread_OU_Model")
options(scipen=999)

pred_week<-17

Hist_Model_Past <-read_xlsx("OU_forecast_horiz49_2013_2023_HIST_PRED_week17_auto.xlsx") %>% 
  filter(!(season %in% c("2024")&week %in% c(pred_week))) %>% 
  mutate(Result= case_when(Check %in% c("Yes")~"Correct",
                           Check %in% c("Push")~"Push",
                           TRUE~"Incorrect")) %>% 
  select(-Check,-My_Pred,-Actual_Result) %>% 
  rename(home_off=home_team_Off,home_def=home_team_Def,away_off=away_team_Off,away_def=away_team_Def,matchup=Matchup,model=Model)

Pred_Model_Past <-read_xlsx("OU_forecast_horiz1_2013_2023_week17_12282024_auto_70_30.xlsx") %>%
  filter(season %in% c("2024")&week %in% c(pred_week))%>%
  rename(home_off=home_team_Off,home_def=home_team_Def,away_off=away_team_Off,away_def=away_team_Def,matchup=Matchup,model=Model) 

Hist_Model_Spread_Past <-read_xlsx("Spread_Forecast_horiz25_2013_2023_week17_12282024_auto_70_30.xlsx")%>% 
  filter(!(season %in% c("2024")&week %in% c(pred_week))) %>% 
  mutate(Result= case_when(Check %in% c("Yes")~"Correct",
                           Check %in% c("Push")~"Push",
                           TRUE~"Incorrect")) %>% 
  select(-Check,-My_Pred,-Actual_Result)%>% 
  rename(home_off=home_team_Off,home_def=home_team_Def,away_off=away_team_Off,away_def=away_team_Def,matchup=Matchup,model=Model)

Pred_Model_Spread_Past <-read_xlsx("Spread_Forecast_horiz25_2013_2023_week17_12282024_auto_70_30.xlsx") %>%
  filter(season %in% c("2024")&week %in% c(pred_week)) %>%
  rename(home_off=home_team_Off,home_def=home_team_Def,away_off=away_team_Off,away_def=away_team_Def,matchup=Matchup,model=Model) 



spread_hit_rate <- Hist_Model_Spread_Past %>%
  mutate(
    away_cover = ifelse((spread_line < 0 & result <= spread_line) |  # Away team favored and covers
                          (spread_line > 0 & result < spread_line),    # Home team favored but away team covers
                        1, 0),
    home_cover = ifelse((spread_line > 0 & result >= spread_line) |  # Home team favored and covers
                          (spread_line < 0 & result > spread_line),    # Away team favored but home team covers
                        1, 0)
  ) %>%
  summarise(
    away_cover_count = sum(away_cover, na.rm = TRUE),
    home_cover_count = sum(home_cover, na.rm = TRUE),
    total_games = n()
  ) %>%
  mutate(
    away_hit_rate = (away_cover_count / total_games)*100,
    home_hit_rate = (home_cover_count / total_games)*100)

american_to_decimal <- function(american_odds) {
  if (american_odds >= 0) {
    return((american_odds / 100) + 1)
  } else {
    return((100 / abs(american_odds)) + 1)
  }
}


generate_odds_choices <- function() {
  odds_range <- c(100, seq(-100, -501, by = -1))
  odds_labels <- sapply(odds_range, function(odds) {
    decimal_odds <- american_to_decimal(odds)
    label <- ifelse(odds > 0, paste("+", odds, sep=""), as.character(odds))
    value <- format(decimal_odds, digits = 4)
    setNames(value, label)
  })
  return(odds_labels)
}


OU_hit_rate <- Hist_Model_Past %>% 
  filter(!Result %in% c("Push")) %>% 
  mutate(Over_count = ifelse(total > total_line, 1, 0),
         Under_count = ifelse(total < total_line, 1, 0)) %>% 
  summarise(over_count = sum(Over_count),
            under_count = sum(Under_count),
            total_count = n()) %>% 
  mutate(under_hit_rate = (under_count / total_count)*100,
         over_hit_rate = (over_count / total_count)*100)

model<-unique(Hist_Model_Past$model) %>% sort()
away_team<-unique(Hist_Model_Past$away_team) %>% sort()
home_team<-unique(Hist_Model_Past$home_team) %>% sort()
matchup_23_22_21<-unique(Hist_Model_Past$matchup) %>% sort()
matchup_24<-unique(Pred_Model_Past$matchup) %>% sort()
week<-unique(Hist_Model_Past$week) %>% sort()

pred_spread_reduced<-Pred_Model_Spread_Past %>%
  filter(season %in% c("2024")&week %in% c(pred_week)) %>%
  group_by(season,matchup,away_team,home_team) %>%
  summarise(spread_line=mean(spread_line),away_cover_pred=mean(away_cov_Pred),home_cover_pred=mean(home_cov_Pred)) %>%
  mutate(away_line=spread_line,home_line=-1*spread_line) %>%
  select(-spread_line)

pred_ou_reduced<-Pred_Model_Past %>%
  filter(season %in% c("2024")&week %in% c(pred_week)) %>%
  group_by(season,matchup,away_team,home_team) %>%
  summarise(total_line=mean(total_line),over_pred=mean(Over_Pred),under_pred=mean(Under_Pred)) 

Mcom_picks<-read_xlsx("Marks_Picks_week16.xlsx")

df_both<-pred_spread_reduced %>%
  left_join(pred_ou_reduced,by=c("season","away_team","home_team","matchup")) %>%
  select(season,matchup,away_team,home_team,under_pred,over_pred,away_cover_pred,home_cover_pred,total_line,away_line,home_line) %>% 
  mutate(model_pref_spread=ifelse(away_cover_pred>=.5,away_team,home_team)) %>% 
  mutate(model_pref_ou=ifelse(over_pred>=.5,"Over","Under"))  %>% 
  left_join(Mcom_picks) %>% 
  mutate(Mark_vs_Model_spread=ifelse(away_cover_pred >=.595 & model_pref_spread==marks_pref_spread,paste0("Model and I both like ","",model_pref_spread),
                                     ifelse(home_cover_pred >=.57 & model_pref_spread==marks_pref_spread,paste0("Model and I both like ","",model_pref_spread),
                                            ifelse(marks_pref_spread == "No Pick"&away_cover_pred >=.595 & model_pref_spread!=marks_pref_spread,paste0("Model likes ","",model_pref_spread, ", I have no pick"),
                                                   ifelse(marks_pref_spread == "No Pick"&home_cover_pred >=.57 & model_pref_spread!=marks_pref_spread,paste0("Model likes ","",model_pref_spread, ", I have no pick"),
                                                          ifelse(marks_pref_spread != "No Pick"&away_cover_pred >=.595 & model_pref_spread!=marks_pref_spread,paste0("Model and I disagree on this pick, I like ","",marks_pref_spread),
                                                                 ifelse(marks_pref_spread != "No Pick"&home_cover_pred >=.57 & model_pref_spread!=marks_pref_spread,paste0("Model and I disagree on this pick, I like ","",marks_pref_spread),
                                                                        ifelse(marks_pref_spread != "No Pick"&away_cover_pred <.595, paste0("Model isn't confident enough or doesn't pick game but I like ","", marks_pref_spread),
                                                                               ifelse(marks_pref_spread != "No Pick"&home_cover_pred <.57,paste0("Model isn't confident enough or doesn't pick game but I like ","", marks_pref_spread),
                                                                                      "Neither have high enough confidence on this spread"))))))))) %>% 
  
  mutate(Mark_vs_Model_ou=ifelse(under_pred >=.595 & model_pref_ou==marks_pref_ou,paste0("Model and I both like ","",model_pref_ou),
                                 ifelse(over_pred >=.57 & model_pref_ou==marks_pref_ou,paste0("Model and I both like ","",model_pref_ou),
                                        ifelse(marks_pref_ou == "No Pick"&under_pred >=.595 & model_pref_ou!=marks_pref_ou,paste0("Model likes ","",model_pref_ou, ", I have no pick"),
                                               ifelse(marks_pref_ou == "No Pick"&over_pred >=.57 & model_pref_ou!=marks_pref_ou,paste0("Model likes ","",model_pref_ou, ", I have no pick"),
                                                      ifelse(marks_pref_ou != "No Pick"&under_pred >=.595 & model_pref_ou!=marks_pref_ou,paste0("Model and I disagree on this total, I like ","",marks_pref_ou),
                                                             ifelse(marks_pref_ou != "No Pick"&over_pred >=.57 & model_pref_ou!=marks_pref_ou,paste0("Model and I disagree on this total, I like ","",marks_pref_ou),
                                                                    ifelse(marks_pref_ou != "No Pick"&under_pred <.595, paste0("Model isn't confident enough or doesn't pick total but I like ","", marks_pref_ou),
                                                                           ifelse(marks_pref_ou != "No Pick"&over_pred <.57,paste0("Model isn't confident enough or doesn't pick total but I like ","", marks_pref_ou),
                                                                                  "Neither have high enough confidence on this total"))))))))) %>% 
  ungroup()%>% 
  select(away_team,home_team,total_line,away_line,home_line,Mark_vs_Model_ou,Mark_vs_Model_spread)

player_prop1 <- "Joe Mixon 70+ rushing yards"
player_prop2 <- "Trey McBride anytime Touchdown" 
player_prop3 <- "Joe Burrow over 250+ passing yards"
player_prop4 <- "Bucky Irving 50+ rushing yards" 
player_prop5 <- "Hunter Henry 40+ receiving yards" 
player_prop6 <- "Chubba Hubbard anytime Touchdown"
player_prop7 <- "Lamar Jackson over 1.5 passing touchdowns"
player_prop8 <- "Aaron Jones 50+ rushing and receving yards"

our_picks1 <- "Tampa Bay -3.5 at Dallas"
our_picks2 <- "Denver Broncos +8 at the Chargers parlayed with Under 44.5 in that game"
our_picks3 <- "Pittsburgh Steelers +8 at Baltimore"
our_picks4 <- "Arizona Cardinals Moneyline at Panthers"
our_picks5 <- "LA Rams Moneyline at New York Jets"
our_picks6 <- "Detriot Lions -6.5 at Chicago"
our_picks7 <- "Bengals -3.5 vs. Browns"
our_picks8 <- "Minnesota Vikings Moneyline at Seattle"
our_picks9 <- "Teaser spreads we are playing: Texans, Broncos, Lions, Tampa Bay, Niners, Bengals, Steelers"
our_picks10 <- "Teaser totals we are playing: Under in Texans/Chiefs, Under in Buffalo/Patriots, Under Broncos/Chargers, Over Browns/Bengals, Over Cardinals/Panthers, Over Rams/Jets"

tup1 <- "The magic wand of the Kansas City Chiefs"
tup2 <- "The Detriot Lions team to beat in the NFL"
tup3 <- "Baltimore Ravens defense is coming back to life a little bit"
tup4 <- "Cold weather football!"
tup5 <- "Eagles defense, Saquon Barkley finding their form and are contenders"
tup6 <- "Josh Allen and the Buffalo Bills!"
tup7 <- "Green Bay Packers, Pittsburgh, Tampa, Minnesota, LA Rams becoming very viable dark horses"
tup8 <- "Baker Mayfield and Tampa!!"

tdown1 <- "Las Vegas Raiders and Antonio Pierce"
tdown2 <- "San Francisco 49ers window closed seemingly"
tdown3 <- "Jags and Giants are downright horrible this year"
tdown4 <- "Dallas Cowboys odds of winning a super bowl for Jerry Jones and their fanbase in this century"
tdown5 <- "Cincy Bengals wasted another year of a great Joe Burrow"
tdown6 <- "Atlanta Falcons look really weak on defense, no pass rush at all, and now they officially do have a Kirk Cousins problem"
tdown7 <- "Chicago Bears sadly (Fire Ryan Poles?)"
tdown8 <- "Detroit Lions injuries"