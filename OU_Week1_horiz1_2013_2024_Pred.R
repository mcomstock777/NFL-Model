#Stadium difficulty ? Top tier/ Mid tier
library(nflreadr)
library(tidyverse)
library(modeltime)
library(kernlab)
library(tidymodels)
library(readr)
library(ggplot2)
library(forecast)
library(timetk)
library(fpp2)
library(dplyr)
library(MLmetrics)
library(readxl)
library(writexl)
options(scipen=999)
library(ranger)
library(patchwork)
library(splines)
library(lubridate)
library(stringi)
library(glmnet)
library(nflfastR)
library(pROC)
library(strex)
library(stringr)
library(vip)


old_year_week<-""
old_year_week<-"2013-01-16"
season<-2025
week_ahead<-2

nflfast_2013<-load_pbp(2013) %>% group_by(game_id,week,season,roof,away_team,home_team,weather,stadium) %>% 
  summarise(wind=mean(wind),temp=mean(temp)) 

nflfast_2014<-load_pbp(2014) %>% group_by(game_id,week,season,roof,away_team,home_team,weather,stadium) %>% 
  summarise(wind=mean(wind),temp=mean(temp)) 

nflfast_2015<-load_pbp(2015) %>% group_by(game_id,week,season,roof,away_team,home_team,weather,stadium) %>% 
  summarise(wind=mean(wind),temp=mean(temp)) 

nflfast_2016<-load_pbp(2016) %>% group_by(game_id,week,season,roof,away_team,home_team,weather,stadium) %>% 
  summarise(wind=mean(wind),temp=mean(temp)) 

nflfast_2017<-load_pbp(2017) %>% group_by(game_id,week,season,roof,away_team,home_team,weather,stadium) %>% 
  summarise(wind=mean(wind),temp=mean(temp)) 

nflfast_2018<-load_pbp(2018) %>% group_by(game_id,week,season,roof,away_team,home_team,weather,stadium) %>% 
  summarise(wind=mean(wind),temp=mean(temp)) 

nflfast_2019<-load_pbp(2019) %>% group_by(game_id,week,season,roof,away_team,home_team,weather,stadium) %>% 
  summarise(wind=mean(wind),temp=mean(temp))  


nflfast_2020<-load_pbp(2020) %>% group_by(game_id,week,season,roof,away_team,home_team,weather,stadium) %>% 
  summarise(wind=mean(wind),temp=mean(temp))  

nflfast_2021<-load_pbp(2021) %>% group_by(game_id,week,season,roof,away_team,home_team,weather,stadium) %>% 
  summarise(wind=mean(wind),temp=mean(temp)) 

nflfast_2022<-load_pbp(2022) %>% group_by(game_id,week,season,roof,away_team,home_team,weather,stadium) %>% 
  summarise(wind=mean(wind),temp=mean(temp)) 

nflfast_2023<-load_pbp(2023)%>% group_by(game_id,week,season,roof,away_team,home_team,weather,stadium) %>%
  summarise(wind=mean(wind),temp=mean(temp))

nflfast_2024<-load_pbp(2024)%>% group_by(game_id,week,season,roof,away_team,home_team,weather,stadium) %>%
  summarise(wind=mean(wind),temp=mean(temp))

nflfast_2025<-load_pbp(2025)%>% group_by(game_id,week,season,roof,away_team,home_team,weather,stadium) %>%
  summarise(wind=mean(wind),temp=mean(temp))


nfl_fast<-rbind(nflfast_2013,nflfast_2014,nflfast_2015,nflfast_2016,nflfast_2017,nflfast_2018,nflfast_2019,nflfast_2020,nflfast_2021,nflfast_2022,nflfast_2023,nflfast_2024
                #,nfl_fast_2025
) %>% 
  filter(week >=1 & week <=17)


nfl<-nfl_fast %>% 
  mutate(temp=sub(".*Temp: ", "", weather)) %>% 
  mutate(temp=sub(" .*F,", "", temp)) %>% 
  mutate(temp=substr(temp,1,2)) %>% 
  # mutate(weather=ifelse(wind>0,wind,weather)) %>% 
  mutate(wind1=sub(".*Wind: ","", weather))

nfl$wind1<-extract_numeric(nfl$wind1)
nfl$temp<-extract_numeric(nfl$temp)

nfl<-nfl %>% 
  mutate(wind=ifelse(is.na(wind1),wind,wind1)) %>% 
  ungroup() %>% 
  mutate(wind=ifelse(wind > 100 & wind <=999,substr(wind,1,1),
                     ifelse(wind >=1000, substr(wind,1,2),
                            ifelse(wind < 0, wind*-1,
                                   wind))))

#nfl<-nfl %>%  mutate(wind=ifelse(wind > 100,substr(wind,1,1),wind))


nfl$temp<-as.numeric(nfl$temp)
nfl$wind<-as.numeric(nfl$wind)

nfl1<-nfl %>% 
  mutate(temp=ifelse(roof %in% c("closed","dome")|(stadium %in% c("State Farm Stadium","University of Phoenix Stadium","Ford Field",
                                                                  "Mercedes-Benz Stadium","Mercedes-Benz Superdome","NRG","NRG Stadium",
                                                                  "Lucas Oil Stadium","Lucas Oil","Caesars Superdome","Allegiant Stadium",
                                                                  "SoFi Stadium","U.S. Bank Stadium","AT&T Stadium","AT&T") &(temp>85|temp<55)),70,temp))%>% 
  mutate(wind=ifelse(roof %in% c("closed","dome")|(stadium %in% c("State Farm Stadium","University of Phoenix Stadium","Ford Field",
                                                                  "Mercedes-Benz Stadium","Mercedes-Benz Superdome","NRG","NRG Stadium",
                                                                  "Lucas Oil Stadium","Lucas Oil","Caesars Superdome","Allegiant Stadium",
                                                                  "SoFi Stadium","U.S. Bank Stadium","AT&T Stadium","AT&T") & wind >5),0,wind)) 



var_look<-load_schedules(seasons = TRUE)%>% 
  filter(season >=2013) %>% 
  filter(game_type %in% c("REG")) %>% 
  mutate(away_team=case_when(away_team %in% c("OAK")~"LV",
                             away_team %in% c("SD")~"LAC",
                             away_team %in% c("STL")~"LA",
                             TRUE~as.character(away_team)))%>% 
  mutate(home_team=case_when(home_team %in% c("OAK")~"LV",
                             home_team %in% c("SD")~"LAC",
                             home_team %in% c("STL")~"LA",
                             TRUE~as.character(home_team))) 

Projected_Setup<-var_look %>% 
  filter(season %in% c("2025")) %>% 
  filter(week %in% c(week_ahead-1)) %>% 
  select(season,week,home_team,away_team,total_line,spread_line) %>% 
  mutate(away_team_projected=ifelse(spread_line<0,(total_line+abs(spread_line))/2,(total_line-abs(spread_line))/2))%>% 
  mutate(home_team_projected=ifelse(spread_line<0,(total_line-abs(spread_line))/2,(total_line+abs(spread_line))/2)) %>% 
  select(-spread_line,-total_line)

Away_Projected<-Projected_Setup %>% 
  select(-home_team,-week) %>% 
  rename(points_forced_projected=away_team_projected,points_allowed_projected=home_team_projected) %>% 
  mutate(home_team=away_team)

Home_Projected<-Projected_Setup %>% 
  select(-away_team,-week) %>% 
  rename(points_forced_projected=home_team_projected,points_allowed_projected=away_team_projected) %>% 
  mutate(away_team=home_team)

Projected<-rbind(Away_Projected,Home_Projected)

Tier_Setup_away<-var_look %>% 
  group_by(away_team,season) %>% 
  summarise(points_allowed=mean(home_score,na.rm=T),points_forced=mean(away_score,na.rm=T)) %>% 
  rename(team=away_team)

Tier_Setup_home<-var_look %>% 
  group_by(home_team,season) %>% 
  summarise(points_allowed=mean(away_score,na.rm=T),points_forced=mean(home_score,na.rm=T)) %>% 
  rename(team=home_team)

add_new_year_data <- function(data, week_ahead, new_data) {
  if (week_ahead == 2) {
    return(bind_rows(data, new_data))
  } else {
    return(data)
  }
}

# Fill_in_new_year<-rbind(Tier_Setup_away,Tier_Setup_home) %>%
#   filter(season %in% c("2023")) %>%
#   mutate(season = case_when(season == 2023~2024)) %>%
#   group_by(team,season) %>%
#   summarise(points_allowed_historic=mean(points_allowed,na.rm=T),points_forced_historic=mean(points_forced,na.rm=T)) %>%
#   rename(away_team=team) %>%
#   mutate(home_team=away_team) %>%
#   select(home_team,away_team,season,points_forced_historic,points_allowed_historic)

Tier_Buildup<-read_xlsx("C:/Users/mcoms/OneDrive/Documents/Sports_Betting_Model/Historical_Analysis/Ad_Hoc/Early_Season_2025_Tier_Buildup.xlsx",sheet="Final_Tier_OU")

Tier_Setup<-rbind(Tier_Setup_away,Tier_Setup_home) %>%
  filter(!(week_ahead == 2 & season == 2025)) %>% 
  group_by(team,season) %>% 
  summarise(points_allowed_historic=mean(points_allowed,na.rm=T),points_forced_historic=mean(points_forced,na.rm=T)) %>% 
  rename(away_team=team) %>% 
  mutate(home_team=away_team) %>% 
  select(home_team,away_team,season,points_forced_historic,points_allowed_historic) %>% 
  #{ add_new_year_data(., week_ahead = 2, Fill_in_new_year) } %>%
  ungroup()%>% 
  left_join(Projected,by=c("home_team", "away_team", "season"))%>% 
  #left_join(Fill_in_new_year,by=c("home_team", "away_team", "season"))%>%  
  mutate(points_forced=ifelse(season %in% c("2024")&!is.na(points_forced_projected), points_forced_historic*0.7 + points_forced_projected*0.3, points_forced_historic)) %>% 
  mutate(points_allowed=ifelse(season %in% c("2024")&!is.na(points_allowed_projected), points_allowed_historic*0.7 + points_allowed_projected*0.3, points_allowed_historic)) %>%
  select(-points_allowed_projected,-points_forced_projected,-points_forced_historic,-points_allowed_historic) %>% 
  mutate(away_team_Off=case_when(points_forced >= 28 ~ "Very_High_Tier",
                                 points_forced >= 24.5 & points_forced < 28 ~ "High_Tier",
                                 points_forced >= 21.5 & points_forced < 24.5 ~ "Mid_Tier",
                                 points_forced >= 18.5 & points_forced < 21.5 ~ "Low_Tier",
                                 points_forced < 18.5 ~ "Bottom_Tier")) %>% 
  mutate(away_team_Def=case_when(points_allowed >= 26 ~ "Bottom_Tier",
                                 points_allowed >= 24 & points_allowed < 26 ~"Low_Tier",
                                 points_allowed >= 21.75 & points_allowed < 24 ~"Mid_Tier",
                                 points_allowed > 19.5 & points_allowed < 21.75 ~"High_Tier",
                                 points_allowed <= 19.5 ~"Very_High_Tier")) %>% 
  select(-points_allowed,-points_forced) %>% 
  rbind(Tier_Buildup)%>% 
  ###Manual override for early weeks
  mutate(home_team_Off=away_team_Off,home_team_Def=away_team_Def)


Tiers_Home<-Tier_Setup %>% 
  ungroup() %>% 
  select(-away_team,-away_team_Off,-away_team_Def) %>% 
  mutate(across(where(is.numeric), ~ as.factor(.))) 

Tiers_Away<-Tier_Setup %>% 
  ungroup() %>% 
  select(-home_team,-home_team_Def,-home_team_Off) %>% 
  mutate(across(where(is.numeric), ~ as.factor(.))) 


rm(Tier_Setup_home,Tier_Setup_away,Projected_Setup,Projected,Tier_Setup,Away_Projected,Home_Projected,nflfast_2016,nflfast_2017,nflfast_2018,
   nflfast_2019,nflfast_2020,nflfast_2021,nflfast_2022,nflfast_2023)


sched<-load_schedules(seasons = TRUE) %>% 
  filter(season >=2013) %>% 
  filter(game_type %in% c("REG")) %>% 
  select(game_id,total_line,total,season,week,gameday,roof,div_game,away_team,home_team,stadium,surface,away_rest,home_rest)

nfl_join<-nfl1 %>% 
  ungroup() %>% 
  select(game_id,week,season,wind,temp)

sched1<-sched %>%  
  left_join(nfl_join)


dat<-sched1 %>% 
  mutate(home_team=substr(game_id,12,18)) %>% 
  mutate(home_team=gsub("_","",home_team)) %>% 
  mutate(away_team=substr(game_id,9,11)) %>% 
  mutate(away_team=gsub("_","",away_team)) %>% 
  filter(!is.na(gameday)) %>% 
  filter(!is.na(game_id)) 

#saveRDS(dat,"C:/Users/mcoms/OneDrive/Documents/Sports_Betting_Model/historic df in case of error.rds")


dat$div_game<-as.factor(dat$div_game)
dat$total_line<-as.numeric(dat$total_line)
dat$total<-as.numeric(dat$total)
dat$season<-as.numeric(dat$season)
dat$week<-as.numeric(dat$week)

dat1<-dat %>% 
  mutate(season1=season) %>%
  mutate(season1=str_pad(season1,5,pad="-","right")) %>%  mutate(season1=str_pad(season1,6,pad="0","right")) %>% 
  mutate(season1=str_pad(season1,7,pad="1","right")) %>%
  mutate(season1=str_pad(season1,8,pad="-","right")) %>%
  mutate(week1=week) %>%
  mutate(week1=ifelse(week1<10,str_pad(week1,2,pad="0","left"),str_pad(week1,1,pad="0","left"))) %>%
  unite(year_week,season1:week1,sep="") %>%
  select(-gameday) 



dat2<-dat1 %>% 
  filter(!week %in% c(17,18)) %>% 
  mutate(temp=ifelse(roof %in% c("closed","dome")|(stadium %in% c("State Farm Stadium","University of Phoenix Stadium","Ford Field",
                                                                  "Mercedes-Benz Stadium","Mercedes-Benz Superdome","NRG","NRG Stadium",
                                                                  "Lucas Oil Stadium","Lucas Oil","Caesars Superdome","Allegiant Stadium",
                                                                  "SoFi Stadium","U.S. Bank Stadium","AT&T Stadium","AT&T") &(temp>85|temp<55)),70,temp))%>% 
  mutate(wind=ifelse(roof %in% c("closed","dome")|(stadium %in% c("State Farm Stadium","University of Phoenix Stadium","Ford Field",
                                                                  "Mercedes-Benz Stadium","Mercedes-Benz Superdome","NRG","NRG Stadium",
                                                                  "Lucas Oil Stadium","Lucas Oil","Caesars Superdome","Allegiant Stadium",
                                                                  "SoFi Stadium","U.S. Bank Stadium","AT&T Stadium","AT&T")
                                                   &wind>5),0,wind)) %>% 
  mutate(temp = case_when(temp <25~"Below_25",
                          temp>=25&temp<55~"25_to_55",
                          temp>=55&temp<85~"55_to_85",
                          temp>=85~"Above_85")) %>% 
  mutate(wind = case_when(wind >= 0&wind<10~"0_to_10",
                          wind>=10&wind<15~"10_to_15",
                          wind>=15&wind<20~"15_to_20",
                          wind>=20~"Above_20")) %>% 
  filter(!(season %in% c(2025) & week >=week_ahead)) %>% 
  # mutate(temp = case_when(
  #   game_id %in% c("2024_01_BAL_KC") ~ "55_to_85",  # Temperature 85?F
  #   game_id %in% c("2024_01_GB_PHI") ~ "55_to_85",  # Temperature 56?F (corrected)
  #   game_id %in% c("2024_01_TEN_CHI") ~ "55_to_85",  # Temperature 70?F
  #   game_id %in% c("2024_01_ARI_BUF") ~ "55_to_85",  # Temperature 56?F (corrected)
  #   game_id %in% c("2024_01_NE_CIN") ~ "55_to_85",  # Temperature 69?F
  #   game_id %in% c("2024_01_JAX_MIA") ~ "Above_85",  # Temperature 88?F
  #   game_id %in% c("2024_01_MIN_NYG") ~ "55_to_85",  # Temperature 70?F
  #   game_id %in% c("2024_01_DEN_SEA") ~ "55_to_85",  # Temperature 74?F
  #   game_id %in% c("2024_01_DAL_CLE") ~ "55_to_85",  # Temperature 68?F
  #   game_id %in% c("2024_01_WAS_TB") ~ "Above_85",  # Temperature 86?F
  #   game_id %in% c("2024_01_NYJ_SF") ~ "55_to_85",  # Temperature 76?F
  #   TRUE ~ temp)) %>%
  # mutate(wind = case_when(
  #   game_id %in% c("2024_01_BAL_KC") ~ "0_to_10",  # Wind 7 mph NW
  #   game_id %in% c("2024_01_GB_PHI") ~ "0_to_10",  # Wind 7 mph NE
  #   game_id %in% c("2024_01_TEN_CHI") ~ "0_to_10",  # Wind 8 mph NW
  #   game_id %in% c("2024_01_ARI_BUF") ~ "10_to_15",  # Wind 15 mph W
  #   game_id %in% c("2024_01_NE_CIN") ~ "0_to_10",  # Wind 3 mph NW
  #   game_id %in% c("2024_01_JAX_MIA") ~ "0_to_10",  # Wind 9 mph SE
  #   game_id %in% c("2024_01_MIN_NYG") ~ "0_to_10",  # Wind 9 mph NW
  #   game_id %in% c("2024_01_DEN_SEA") ~ "0_to_10",  # Wind 6 mph SW
  #   game_id %in% c("2024_01_DAL_CLE") ~ "10_to_15",  # Wind 13 mph W
  #   game_id %in% c("2024_01_WAS_TB") ~ "0_to_10",  # Wind 5 mph NW
  #   game_id %in% c("2024_01_NYJ_SF") ~ "0_to_10",  # Wind 9 mph NW
  #   TRUE ~ wind)) %>%
  mutate(wind = ifelse(is.na(wind), "0_to_10", wind)) %>%
  mutate(temp = ifelse(is.na(temp), "55_to_85", temp))


###THIS GETS BROUGHT BACK IN LATER
total_join<-dat2 %>% 
  select(season,week,home_team,away_team,total_line,total,wind,temp,roof,div_game) 
total_join$week<-as.numeric(total_join$week)
total_join$season<-as.factor(total_join$season)
total_join<-total_join %>% mutate(across(where(is.character), ~ as.factor(.)))


####BROUGHT BACK IN LATER FOR JOIN


dat2<-dat2 %>% 
  mutate(OU=ifelse(total>=total_line,1,0))


dat2$week<-as.factor(dat2$week)
dat2$season<-as.factor(dat2$season)
dat2<-dat2 %>% mutate(across(where(is.character), ~ as.factor(.)))
dat2$year_week<-as.Date(dat2$year_week)


dat3<-dat2 %>% select(-total,-game_id) %>% 
  mutate(away_team=case_when(away_team %in% c("OAK")~"LV",
                             away_team %in% c("SD")~"LAC",
                             away_team %in% c("STL")~"LA",
                             TRUE~as.character(away_team)))%>% 
  mutate(home_team=case_when(home_team %in% c("OAK")~"LV",
                             home_team %in% c("SD")~"LAC",
                             home_team %in% c("STL")~"LA",
                             TRUE~as.character(home_team))) 


dat4<-dat3 %>% 
  left_join(Tiers_Home,by=c("home_team","season")) %>% 
  left_join(Tiers_Away,by=c("away_team","season")) %>% 
  mutate(week = case_when(week %in% c("1","2","3","4","5","6")~"Early_Season",
                          week %in% c("7","8","9","10","11","12")~"Mid_Season",
                          week %in% c("13","14","15","16")~"Late_Season"))


dat4$year_week<-as.character(dat4$year_week)

data_prepared_tbl<-dat4 %>% 
  filter(!year_week %in% c(old_year_week)) %>% 
  arrange(season) %>% 
  select(-stadium)

data_prepared_tbl$OU<-as.factor(data_prepared_tbl$OU)
data_prepared_tbl$year_week<-as.Date(data_prepared_tbl$year_week)

FORECAST_HORIZON<-1

splits<-data_prepared_tbl %>% 
  time_series_split(
    date_var=year_week,
    assess=FORECAST_HORIZON,
    cumulative=TRUE
    
  )
splits

train<-training(splits)
test<-testing(splits) 


rec1<-recipe(OU~total_line+temp+wind+roof+div_game+away_team_Off+home_team_Off+away_team_Def+home_team_Def+year_week
             #+ away_cumu_rest+home_cumu_rest
             ,
             data=train) %>% 
  step_date(year_week) %>% 
  step_rm(year_week) %>% 
  step_zv(all_predictors()) %>%
  step_naomit(everything(),skip=TRUE) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())

rec1


set.seed(100)

lr_mod<-logistic_reg(penalty=.01,mixture=1) %>% 
  set_engine("glmnet")

lr_workflow<-workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(rec1) 

lr_fit <- 
  lr_workflow%>% 
  fit(data = train)

lr_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

predict(lr_fit, test)


lr_aug <- 
  augment(lr_fit, test) %>% 
  mutate(My_Pred=ifelse(.pred_1>=.5001,1,0)) 

lr_aug %>% 
  roc_curve(truth = OU, My_Pred) %>% 
  autoplot()


lr_aug %>% 
  roc_auc(truth = OU, My_Pred)


lr_aug$My_Pred<-as.factor(lr_aug$My_Pred)

lr_aug %>% 
  #filter(!season %in%c(2022)) %>%   # test set predictions
  metrics(truth = OU, My_Pred)



lr_final<-lr_aug %>% 
  mutate(Matchup=paste0(away_team,"vs",home_team)) %>% 
  mutate(week=str_sub(year_week,-2,-1)) %>% 
  mutate(week = as.numeric(week))%>% 
  left_join(total_join) %>% 
  mutate(Check=case_when(total_line==total~"Push",
                         My_Pred %in% c(1) & OU %in% c(1)~"Yes",
                         My_Pred %in% c(0)& OU %in% c(0)~"Yes",
                         My_Pred %in% c(1)& OU %in% c(NA)~"NA",
                         My_Pred %in% c(0)& OU %in% c(NA)~"NA",
                         TRUE~"No")) %>% 
  rename(Actual_Result=OU,Under_Pred=.pred_0,Over_Pred=.pred_1) %>% 
  mutate(My_Pred = ifelse(My_Pred=='1',1,0))%>% 
  mutate(Actual_Result = ifelse(Actual_Result=='1',1,0))%>%
  select(Matchup,home_team,away_team,season,week,roof,div_game,away_team_Off,home_team_Off,away_team_Def,home_team_Def,wind,temp,
         total_line,total,Actual_Result,My_Pred,Check,Under_Pred,Over_Pred
         #,away_cumu_rest,home_cumu_rest
  ) %>% 
  mutate(Model="lr")


lr_final %>%
  #filter(season %in% c(2024)) %>% 
  group_by(Check) %>%
  summarise(count=n())



# write_xlsx(lr_final,"C:/Users/mcoms/OneDrive/Documents/Sports_Betting_Model/Sports-Betting-Model/Excel_Output/lr_OU_07_29_2024.xlsx")



cores <- parallel::detectCores()
cores

rf_mod <- 
  rand_forest(mtry = tune(), 
              min_n = tune(), 
              trees = 1500) %>% 
  set_engine("ranger", 
             num.threads = cores) %>% 
  set_mode("classification")

rf_workflow<-workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rec1)


extract_parameter_set_dials(rf_mod)


set.seed(351)

val_set<-validation_split(train,
                          strata="OU",
                          prop=.75)
val_set


rf_res<-rf_workflow %>% 
  tune_grid(val_set,
            grid=25,
            control=control_grid(save_pred=TRUE),
            metrics=metric_set(roc_auc))

rf_res %>% show_best(metric="roc_auc")

autoplot(rf_res)



rf_best<-rf_res %>% 
  select_best(metric="roc_auc")
rf_best




rf_res %>% collect_predictions()




rf_auc<-rf_res %>% collect_predictions(paramaters=rf_best)%>% 
  roc_curve(OU,.pred_1) %>% 
  mutate(model="Random Forest")

rf_auc_pred<-rf_res %>% 
  collect_predictions(parameters=rf_best)




sim_roc_rf<-roc(response=rf_auc_pred$OU,
                predictor=rf_auc_pred$.pred_0,
                levels=c(1,0))




# rf_auc %>% 
#   ggplot(aes(x=1-specificity,y=sensitivity,col=model))+
#   geom_path(lwd=1.5,alpha=.8)+
#   geom_abline(lty=3)+
#   coord_equal()+
#   scale_color_viridis_d(option="plasma",end=.6)+
#   annotate('text',x=.5,y=.9,label=paste0('AUC_RF: ',round(auc(sim_roc_rf),digits=3)))

mtry<-rf_best$mtry %>% as.numeric()
minn<-rf_best$min_n %>% as.numeric()

# the last model
last_rf_mod <- 
  rand_forest(mtry = mtry, min_n = minn, trees = 2000) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("classification")

# the last workflow
last_rf_workflow <- 
  rf_workflow %>% 
  update_model(last_rf_mod)


# the last fit
set.seed(351)
last_rf_fit <- 
  last_rf_workflow %>% 
  last_fit(splits)

last_rf_fit

last_rf_fit %>% 
  collect_metrics()




last_rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 20)




last_rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(OU, .pred_1) %>% 
  autoplot()




final_tree <- extract_workflow(last_rf_fit)
final_tree




rf1<-last_rf_fit %>% 
  collect_predictions()%>% 
  bind_cols(test %>% select(season,week,home_team,away_team,total_line,wind,temp,roof,div_game,away_team_Off,home_team_Off,away_team_Def,home_team_Def,year_week
                            #,away_cumu_rest,home_cumu_rest
  )) %>% 
  select(-id,-.row,-.config) %>% 
  rename(My_Pred=.pred_class) 


rf1$My_Pred<-as.factor(rf1$My_Pred)

rf1 %>% 
  #filter(!season %in%c(2022)) %>%   # test set predictions
  metrics(truth = OU, My_Pred)



rf_final<- rf1 %>% 
  mutate(Matchup=paste0(away_team,"vs",home_team)) %>% 
  mutate(week=str_sub(year_week,-2,-1)) %>% 
  mutate(week = as.numeric(week))%>% 
  left_join(total_join) %>% 
  mutate(Check=case_when(total_line==total~"Push",
                         My_Pred %in% c(1) & OU %in% c(1)~"Yes",
                         My_Pred %in% c(0)& OU %in% c(0)~"Yes",
                         My_Pred %in% c(1)& OU %in% c(NA)~"NA",
                         My_Pred %in% c(0)& OU %in% c(NA)~"NA",
                         TRUE~"No")) %>% 
  rename(Actual_Result=OU,Under_Pred=.pred_0,Over_Pred=.pred_1) %>% 
  mutate(My_Pred = ifelse(My_Pred=='1',1,0))%>% 
  mutate(Actual_Result = ifelse(Actual_Result=='1',1,0))%>%
  select(Matchup,home_team,away_team,season,week,roof,div_game,away_team_Off,home_team_Off,away_team_Def,home_team_Def,wind,temp,
         total_line,total,Actual_Result,My_Pred,Check,Under_Pred,Over_Pred
         #,away_cumu_rest,home_cumu_rest
  )%>% 
  mutate(Model="rf")

rf_final %>%
  group_by(Check) %>%
  #filter(season %in%c(2024)) %>%   # test set predictions
  summarise(count=n())


# write_xlsx(rf_final,"C:/Users/mcoms/OneDrive/Documents/Sports_Betting_Model/Sports-Betting-Model/Excel_Output/rf_OU_07_29_2024.xlsx")



val_set<-validation_split(train,
                          strata="OU",
                          prop=.75)
val_set


set.seed(412)

nfl_folds1<-vfold_cv(train,strata=OU)
nfl_folds1


xg_mod<-boost_tree(
  tree_depth=tune(),
  min_n=tune(),
  loss_reduction=tune(),
  sample_size=tune(),
  trees=1000) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")


xgb_grid<-grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size=sample_prop(),
  finalize(mtry(),train),
  learn_rate(),
  size=25
)

xgb_wf<-workflow() %>% 
  add_recipe(rec1) %>% 
  add_model(xg_mod)


set.seed(412)
xg_res<-
  xgb_wf %>% 
  tune_grid(
    #val_set,
    resamples=nfl_folds1,
    grid=25,
    control=control_grid(save_pred=TRUE),
    metrics=metric_set(roc_auc))


xg_res %>% 
  show_best(metric="roc_auc")

autoplot(xg_res)


xg_best<-xg_res %>% 
  select_best(metric="roc_auc")

xg_best

xg_res %>% collect_predictions()


xg_auc<-xg_res %>% 
  collect_predictions(parameters=xg_best) %>% 
  roc_curve(OU,.pred_1) %>% 
  mutate(model="xgboost")


xg_auc_pred<-
  xg_res %>% 
  collect_predictions(parameters=xg_best)

sim_roc_xg<-roc(response=xg_auc_pred$OU,
                predictor=xg_auc_pred$.pred_1,
                levels=c(1,0))


# xg_auc %>% 
#   ggplot(aes(x=1-specificity,y=sensitivity,col=model))+
#   geom_path(lwd=1.5,alpha=.8)+
#   geom_abline(lty=3)+
#   coord_equal()+
#   scale_color_viridis_d(option="plasma",end=.6)+
#   annotate('text',x=.5,y=.9,label=paste0('AUC_XG: ',round(auc(sim_roc_xg),digits=3)))


show_best(xg_res,"roc_auc")

best_auc<-select_best(xg_res,"roc_auc")
best_auc


final_xgb<-finalize_workflow(
  xgb_wf,
  best_auc
)

final_xgb


final_xgb %>% 
  fit(data=train) %>% 
  pull_workflow_fit() %>% 
  vip(geom="point")


final_res <- last_fit(final_xgb, splits)

collect_metrics(final_res)



# final_res %>%
#   collect_predictions() %>%
#   roc_curve(OU, .pred_1) %>%
#   ggplot(aes(x = 1 - specificity, y = sensitivity)) +
#   geom_line(size = 1.5, color = "midnightblue") +
#   geom_abline(
#     lty = 2, alpha = 0.5,
#     color = "gray50",
#     size = 1.2
#   )

xg1<-final_res %>% 
  collect_predictions() %>% 
  bind_cols(test %>% select(season,home_team,away_team,total_line,wind,temp,roof,div_game,away_team_Off,home_team_Off,away_team_Def,home_team_Def,year_week
                            #,away_cumu_rest,home_cumu_rest
  )) %>% 
  select(-id,-.row,-.config) %>% 
  rename(My_Pred=.pred_class) 


xg1$My_Pred<-as.factor(xg1$My_Pred)

xg1 %>% 
  #filter(!season %in%c(2022)) %>%   # test set predictions
  metrics(truth = OU, My_Pred)

xg_final<-xg1 %>%  
  mutate(Matchup=paste0(away_team,"vs",home_team)) %>% 
  mutate(week=str_sub(year_week,-2,-1)) %>% 
  mutate(week = as.numeric(week))%>% 
  left_join(total_join) %>% 
  mutate(Check=case_when(total_line==total~"Push",
                         My_Pred %in% c(1) & OU %in% c(1)~"Yes",
                         My_Pred %in% c(0)& OU %in% c(0)~"Yes",
                         My_Pred %in% c(1)& OU %in% c(NA)~"NA",
                         My_Pred %in% c(0)& OU %in% c(NA)~"NA",
                         TRUE~"No")) %>% 
  rename(Actual_Result=OU,Under_Pred=.pred_0,Over_Pred=.pred_1) %>% 
  mutate(My_Pred = ifelse(My_Pred=='1',1,0))%>% 
  mutate(Actual_Result = ifelse(Actual_Result=='1',1,0))%>%
  select(Matchup,home_team,away_team,season,week,roof,div_game,away_team_Off,home_team_Off,away_team_Def,home_team_Def,wind,temp,
         total_line,total,Actual_Result,My_Pred,Check,Under_Pred,Over_Pred
         #,away_cumu_rest,home_cumu_rest
  ) %>% 
  mutate(Model="xg")


xg_final %>%
  group_by(Check) %>%
  filter(season %in%c(2024)) %>%   # test set predictions
  summarise(count=n())


# write_xlsx(xg_final,"C:/Users/mcoms/OneDrive/Documents/Sports_Betting_Model/Sports-Betting-Model/Excel_Output/xg_OU_07_29_2024.xlsx")


all_models_final<-rbind(lr_final,rf_final,xg_final)

# write_xlsx(all_models_final,"C:/Users/mcoms/OneDrive/Documents/Sports_Betting_Model/Sports-Betting-Model/OU_forecast_horiz1_2013_2023_week1_09062024.xlsx")
# write_xlsx(all_models_final,"C:/Users/mcoms/OneDrive/Documents/Sports_Betting_Model/Sports-Betting-Model/Shiny_App_Posit/OU_forecast_horiz1_2013_2023_week1_09062024.xlsx")
