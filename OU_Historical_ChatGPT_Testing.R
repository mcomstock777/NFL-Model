
library(tidyverse)
library(nflreadr)
library(nflfastR)
library(lubridate)
library(fastshap)
library(stacks)
library(vip)
library(pROC)
library(writexl)
library(glmnet)
library(ranger)
library(xgboost)
library(patchwork)
library(splines)
library(stringi)
library(strex)
library(parsnip)
library(workflows)
library(recipes)
library(tune)
library(yardstick)
library(rsample)
library(dials)
library(infer)
library(modeldata)
library(timetk)
library(devtools)
library(rstanarm)
library(nnet)
library(catboost)

options(scipen=999)

old_year_week<-""
old_year_week<-"2013-01-16"
season<-2025
week_ahead<-1
plus<-
  
  seasons <- 2013:2024
nflfast_list <- map(seasons, function(year) {
  load_pbp(year) %>%
    group_by(game_id, week, season, roof, away_team, home_team, weather, stadium) %>%
    summarise(wind = mean(wind, na.rm = TRUE), temp = mean(temp, na.rm = TRUE), .groups = "drop")
})
nfl_fast <- bind_rows(nflfast_list) %>%
  filter(week >= 1 & week <= 17)


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
  if (week_ahead == 17) {
    return(bind_rows(data, new_data))
  } else {
    return(data)
  }
}

Fill_in_new_year<-rbind(Tier_Setup_away,Tier_Setup_home) %>%
  filter(season %in% c("2025")) %>%
  group_by(team,season) %>%
  summarise(points_allowed_historic=mean(points_allowed,na.rm=T),points_forced_historic=mean(points_forced,na.rm=T)) %>%
  rename(away_team=team) %>%
  mutate(home_team=away_team) %>%
  select(home_team,away_team,season,points_forced_historic,points_allowed_historic)

# Tier_Buildup<-read_xlsx("C:/Users/mcoms/OneDrive/Documents/Sports_Betting_Model/Historical_Analysis/Ad_Hoc/Early_Season_2024_Tier_Buildup_working.xlsx",
#                         sheet="Final_Tier_OU")

Tier_Setup<-rbind(Tier_Setup_away,Tier_Setup_home) %>%
  filter(!(week_ahead == 1 & season == 2025)) %>% 
  group_by(team,season) %>% 
  summarise(points_allowed_historic=mean(points_allowed,na.rm=T),points_forced_historic=mean(points_forced,na.rm=T)) %>% 
  rename(away_team=team) %>% 
  mutate(home_team=away_team) %>% 
  select(home_team,away_team,season,points_forced_historic,points_allowed_historic) %>% 
  { add_new_year_data(., week_ahead = 1, Fill_in_new_year) } %>%
  ungroup()%>% 
  left_join(Projected,by=c("home_team", "away_team", "season"))%>% 
  #left_join(Fill_in_new_year,by=c("home_team", "away_team", "season"))%>%  
  mutate(points_forced=ifelse(season %in% c("2024")&!is.na(points_forced_projected), points_forced_historic*0.7 + points_forced_projected*0.3, points_forced_historic)) %>% 
  mutate(points_allowed=ifelse(season %in% c("2024")&!is.na(points_forced_projected), points_allowed_historic*0.7 + points_allowed_projected*0.3, points_allowed_historic)) %>%
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
  # rbind(Tier_Buildup)%>% 
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
   nflfast_2019,nflfast_2020,nflfast_2021,nflfast_2022,nflfast_2023,nflfast_2024)


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
  #   game_id == "2024_17_KC_PIT" ~ "25_to_55",   # 38?F
  #   game_id == "2024_17_SEA_CHI" ~ "25_to_55",  # 49?F
  #   game_id == "2024_17_LAC_NE" ~ "25_to_55",   # 40?F
  #   game_id == "2024_17_DEN_CIN" ~ "55_to_85",  # 61?F
  #   game_id == "2024_17_IND_NYG" ~ "55_to_85",  # 54?F
  #   game_id == "2024_17_NYJ_BUF" ~ "25_to_55",  # 54?F
  #   game_id == "2024_17_CAR_TB" ~ "55_to_85",   # 74?F
  #   game_id == "2024_17_DAL_PHI" ~ "55_to_85",  # 61?F
  #   game_id == "2024_17_TEN_JAX" ~ "55_to_85",  # 74?F
  #   game_id == "2024_17_MIA_CLE" ~ "25_to_55",  # 57?F
#   game_id == "2024_17_GB_MIN" ~ "25_to_55",   # 40?F
#   game_id == "2024_17_ATL_WAS" ~ "55_to_85",  # 59?F
#   game_id == "2024_17_DET_SF" ~ "25_to_55",   # 52?F
#   TRUE ~ temp
# )) %>%
# mutate(wind = case_when(
#   game_id == "2024_17_KC_PIT" ~ "0_to_10",    # 5 mph NE
#   game_id == "2024_17_SEA_CHI" ~ "0_to_10",   # 7 mph SE
#   game_id == "2024_17_LAC_NE" ~ "0_to_10",    # 5 mph S
#   game_id == "2024_17_DEN_CIN" ~ "0_to_10",   # 7 mph SE
#   game_id == "2024_17_IND_NYG" ~ "0_to_10",   # 8 mph SE
#   game_id == "2024_17_NYJ_BUF" ~ "Above_20",  # 23 mph SE
#   game_id == "2024_17_CAR_TB" ~ "0_to_10",    # 10 mph S
#   game_id == "2024_17_DAL_PHI" ~ "10_to_15",  # 12 mph SE
#   game_id == "2024_17_TEN_JAX" ~ "10_to_15",  # 15 mph SW
#   game_id == "2024_17_MIA_CLE" ~ "15_to_20",  # 17 mph SE
#   game_id == "2024_17_ATL_WAS" ~ "10_to_15",   # 10 mph S
#   game_id == "2024_17_DET_SF" ~ "0_to_10",    # 8 mph NW
#   TRUE ~ wind
# )) %>%
mutate(wind = ifelse(is.na(wind), "0_to_10", wind)) %>%
  mutate(temp = ifelse(is.na(temp), "55_to_85", temp))

#2023 fill in
#%>% 
#mutate(temp = case_when(game_id %in% c("2023_01_")~x,
#game_id %in% c("2023_01_")~x)) %>% 
#mutate(wind = case_when(game_id %in% c("2023_01_")~x,
#game_id %in% c("2023_01_")~x))


###THIS GETS BROUGHT BACK IN LATER
total_join<-dat2 %>% 
  select(season,week,home_team,away_team,total_line,total,wind,temp,roof,div_game) 
total_join$week<-as.numeric(total_join$week)
total_join$season<-as.factor(total_join$season)
total_join<-total_join %>% mutate(across(where(is.character), ~ as.factor(.)))


####BROUGHT BACK IN LATER FOR JOIN


dat3 <- dat2 %>% 
  mutate(OU=ifelse(total>=total_line,1,0)) %>% 
  select(-total,-game_id) %>% 
  mutate(away_team=case_when(away_team %in% c("OAK")~"LV",
                             away_team %in% c("SD")~"LAC",
                             away_team %in% c("STL")~"LA",
                             TRUE~as.character(away_team)))%>% 
  mutate(home_team=case_when(home_team %in% c("OAK")~"LV",
                             home_team %in% c("SD")~"LAC",
                             home_team %in% c("STL")~"LA",
                             TRUE~as.character(home_team))) %>% 
  mutate(across(where(is.character), ~ as.factor(.)))


dat3$week<-as.factor(dat3$week)
dat3$season<-as.factor(dat3$season)
dat3$year_week<-as.Date(dat3$year_week)
dat3$year_week<-as.character(dat3$year_week)



data_prepared_tbl <- dat3 %>% 
  left_join(Tiers_Home,by=c("home_team","season")) %>% 
  left_join(Tiers_Away,by=c("away_team","season")) %>% 
  mutate(week = case_when(
    week %in% c("1","2","3","4","5","6")~"Early_Season",
    week %in% c("7","8","9","10","11","12")~"Mid_Season",
    week %in% c("13","14","15","16")~"Late_Season")) %>%
  mutate(season_index = as.numeric(as.character(season)) - 2012) %>%
  mutate(tier_combo = paste0(away_team_Off, "_", home_team_Def)) %>%
  mutate(across(where(is.character), ~ as.factor(.))) %>%
  filter(!year_week %in% c(old_year_week)) %>%
  arrange(season) %>%
  select(-stadium)

data_prepared_tbl$OU <- as.factor(data_prepared_tbl$OU)

data_prepared_tbl$year_week <- as.Date(data_prepared_tbl$year_week)

# ========================
# TRAIN/TEST SPLIT
# ========================
FORECAST_HORIZON <- 1
splits <- time_series_split(
  data_prepared_tbl,
  date_var = year_week,
  assess = FORECAST_HORIZON,
  cumulative = TRUE
)
train <- training(splits)
test  <- testing(splits)

# ========================
# RECIPE
# ========================
rec1 <- recipe(OU ~ total_line+temp+wind+roof+div_game+away_team_Off+home_team_Off+away_team_Def+home_team_Def+year_week,
               data = train) %>%
  step_date(year_week) %>%
  step_rm(year_week) %>%
  step_zv(all_predictors()) %>%
  step_naomit(everything(), skip = TRUE) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())


# MODELS: Logistic Regression
# ========================


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
  #  filter(season %in% c(2025)) %>% 
  group_by(Check) %>%
  summarise(count=n())

# write_xlsx(lr_final,"C:/Users/mcoms/OneDrive/Documents/Sports_Betting_Model/Sports-Betting-Model/Excel_Output/lr_OU_07_29_2024.xlsx")


#RANDOM FOREST
################ 

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
  #filter(season %in%c(2023)) %>%   # test set predictions
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

show_best(xg_res,"roc_auc")

best_auc <- select_best(xg_res, metric = "roc_auc")
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




xg1<-final_res %>% 
  collect_predictions() %>% 
  bind_cols(test %>% select(season,home_team,away_team,total_line,wind,temp,roof,div_game,away_team_Off,home_team_Off,away_team_Def,home_team_Def,year_week
                            #,away_cumu_rest,home_cumu_rest
  )) %>% 
  select(-id,-.row,-.config) %>% 
  rename(My_Pred=.pred_class) 


xg1$My_Pred<-as.factor(xg1$My_Pred)

xg1 %>% 
  #filter(!season %in%c(2024)) %>%   # test set predictions
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



# ========================
# -- Your original logistic_reg, rf_mod, xg_mod + tuning code stays the same --
# -- After final_xgb created, we add SHAP analysis below --

# ========================
# SHAP VALUES FOR XGBOOST
# ========================
xgb_fit_final <- final_xgb %>%
  fit(data = train) %>%
  pull_workflow_fit()

X <- bake(prep(rec1), new_data = train) %>% select(-OU)

X_df <- as.data.frame(X)

shap_values <- fastshap::explain(
  object = xgb_fit_final$fit,
  X = X_df,
  pred_wrapper = function(object, newdata) {
    predict(object, newdata = as.matrix(newdata))  # ← returns probabilities for binary
  },
  nsim = 100
)

# Save SHAP global importance plot
autoplot(shap_values) + ggtitle("SHAP Variable Importance (XGBoost)")

# ========================
# STACKED ENSEMBLE MODEL
# ========================
# Assumes rf_res, xg_res are result objects; lr_aug holds predictions


set.seed(2025)
resamples <- vfold_cv(train, v = 5, strata = OU)


rf_wflow_en <- workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(rec1)


xg_wflow_en <- workflow() %>%
  add_model(xg_mod) %>%
  add_recipe(rec1)

rf_res_en <- tune_grid(
  rf_wflow_en,  # ✅ was mistakenly written as rf_wflow
  resamples = resamples,
  grid = 10,
  control = control_stack_resamples()
)

xg_res_en <- tune_grid(
  xg_wflow_en,  # ✅ was mistakenly written as xg_wflow
  resamples = resamples,
  grid = 10,
  control = control_stack_resamples()
)

stack <- stacks() %>%
  add_candidates(rf_res_en) %>%
  add_candidates(xg_res_en) %>%
  blend_predictions() %>%
  fit_members()

# ========================
# ENSEMBLE PREDICTIONS
# ========================


cat_features <- which(sapply(bake(prep(rec1), new_data = train), is.factor))
train_matrix <- bake(prep(rec1), new_data = train) %>% select(-OU)

cat_train_matrix <- bake(prep(rec1), new_data = train) %>%
  select(-OU) %>%
  mutate(across(where(is.character), as.factor))  # ← Convert character to factor

train_labels <- as.integer(train$OU) - 1

dtrain <- catboost.load_pool(data = cat_train_matrix, label = train_labels)
 
dparams <- list(
  loss_function = "Logloss",
  eval_metric = "AUC",
  iterations = 1000,
  learning_rate = 0.05,
  depth = 6,
  l2_leaf_reg = 3  # optional: adds regularization
)

cat_model <- catboost.train(dtrain, params = dparams)

# Predict on test set
 X_test <- bake(prep(rec1), new_data = test)

  dtest <- catboost.load_pool(data = X_test, cat_features = cat_features)
 cat_pred <- catboost.predict(cat_model, dtest, prediction_type = "Probability")

 cat_final <- tibble(My_Pred = ifelse(cat_pred >= 0.5001, 1, 0),
                     OU = as.numeric(as.character(test$OU)),
                     Model = "catboost") 
 
# ---- Bayesian Logistic Regression ----
bayes_mod <- stan_glm(OU ~ ., data = bake(prep(rec1), new_data = train) %>% mutate(OU = train$OU),
                      family = binomial(link = "logit"), refresh = 0, chains = 2, iter = 1000)

bayes_pred <- posterior_linpred(bayes_mod, newdata = X_test, transform = TRUE)
bayes_mean_pred <- colMeans(bayes_pred)

# ---- Neural Net Model ----
nn_mod <- nnet(OU ~ ., data = bake(prep(rec1), new_data = train) %>% mutate(OU = as.numeric(train$OU) - 1),
               size = 5, decay = 0.01, maxit = 200, trace = FALSE)

nn_pred <- predict(nn_mod, newdata = X_test)

# ========================
# FINAL OUTPUT CONSOLIDATION
# ========================
ensemble_preds <- predict(stack, new_data = test, type = "prob") %>%
  bind_cols(test %>% select(season,home_team,away_team,total_line,wind,temp,roof,
                            div_game,away_team_Off,home_team_Off,away_team_Def,home_team_Def,year_week,OU
  )) %>% 
  mutate(My_Pred = ifelse(.pred_1 >= 0.5001, 1, 0),
         OU = as.numeric(as.character(OU)))


bayes_final <- tibble(
  Over_Pred = bayes_mean_pred,
  Under_Pred = 1 - bayes_mean_pred,
  My_Pred = ifelse(bayes_mean_pred >= 0.5001, 1, 0),
  Actual_Result = as.numeric(as.character(test$OU))
) %>%
  bind_cols(test %>% select(season, home_team, away_team, total_line, wind, temp, roof, 
                            div_game, away_team_Off, home_team_Off, away_team_Def, home_team_Def, year_week)) %>%
  left_join(total_join, by = c("season", "home_team", "away_team", "total_line", "wind", "temp", "roof", "div_game")) %>%
  mutate(
    Matchup = paste0(away_team, "vs", home_team),
    week = as.numeric(str_sub(year_week, -2, -1))
  ) %>%
  mutate(
    Check = case_when(
      total_line == total ~ "Push",
      My_Pred == 1 & Actual_Result == 1 ~ "Yes",
      My_Pred == 0 & Actual_Result == 0 ~ "Yes",
      is.na(Actual_Result) ~ "NA",
      TRUE ~ "No"
    ),
    My_Pred = as.integer(My_Pred),
    Actual_Result = as.integer(Actual_Result),
    Model = "bayesian"
  ) %>%
  select(Matchup, home_team, away_team, season, week, roof, div_game, away_team_Off, home_team_Off, away_team_Def, home_team_Def,
         wind, temp, total_line, total, Actual_Result, My_Pred, Check, Under_Pred, Over_Pred, Model)

ensemble_final <- ensemble_preds %>%
  mutate(My_Pred = ifelse(.pred_1 >= 0.5001, 1, 0)) %>%
  left_join(total_join, by = c("season", "home_team", "away_team", "total_line", "wind", "temp", "roof", "div_game")) %>%
  mutate(Matchup=paste0(away_team,"vs",home_team)) %>% 
  mutate(week=str_sub(year_week,-2,-1)) %>% 
  mutate(week = as.numeric(week))%>% 
  mutate(Check=case_when(total_line==total~"Push",
                         My_Pred %in% c(1) & OU %in% c(1)~"Yes",
                         My_Pred %in% c(0)& OU %in% c(0)~"Yes",
                         My_Pred %in% c(1)& OU %in% c(NA)~"NA",
                         My_Pred %in% c(0)& OU %in% c(NA)~"NA",
                         TRUE~"No")) %>% 
  rename(Actual_Result=OU,
         Under_Pred=.pred_0,
         Over_Pred=.pred_1) %>% 
  mutate(My_Pred = ifelse(My_Pred=='1',1,0))%>% 
  mutate(Actual_Result = ifelse(Actual_Result=='1',1,0))%>%
  select(Matchup,home_team,away_team,season,week,roof,div_game,away_team_Off,home_team_Off,away_team_Def,home_team_Def,wind,temp,
         total_line,total,Actual_Result,My_Pred,Check,Under_Pred,Over_Pred
  ) %>% 
  mutate(Model="ensemble")


nn_final <- tibble(
  Over_Pred = nn_pred,
  Under_Pred = 1 - nn_pred,
  My_Pred = ifelse(nn_pred >= 0.5001, 1, 0),
  OU = as.numeric(as.character(test$OU))
) %>% 
  bind_cols(test %>% select(season, home_team, away_team, total_line, wind, temp, roof, 
                            div_game, away_team_Off, home_team_Off, away_team_Def, home_team_Def, year_week)) %>%
  rename(Actual_Result = OU) %>%  # <- rename here BEFORE it's used
  left_join(total_join, by = c("season", "home_team", "away_team", "total_line", "wind", "temp", "roof", "div_game")) %>%
  mutate(
    Matchup = paste0(away_team, "vs", home_team),
    week = as.numeric(str_sub(year_week, -2, -1)),
    Check = case_when(
      total_line == total ~ "Push",
      My_Pred == 1 & Actual_Result == 1 ~ "Yes",
      My_Pred == 0 & Actual_Result == 0 ~ "Yes",
      is.na(Actual_Result) ~ "NA",
      TRUE ~ "No"
    ),
    My_Pred = as.integer(My_Pred),
    Actual_Result = as.integer(Actual_Result),
  ) %>%
  select(Matchup, home_team, away_team, season, week, roof, div_game,
         away_team_Off, home_team_Off, away_team_Def, home_team_Def, wind, temp,
         total_line, total, Actual_Result, My_Pred, Check, Under_Pred, Over_Pred) %>% 
  mutate(Model= "nnet") 







all_models_final <- bind_rows(
  lr_final,
  rf_final ,
  xg_final,
  ensemble_final,
  #cat_final,
  bayes_final,
  nn_final
) 

all_models_test<-all_models_final %>% 
  mutate(Check=case_when(My_Pred %in% c(1) & Actual_Result %in% c(1)~"Yes",
                         My_Pred %in% c(0)& Actual_Result %in% c(0)~"Yes",
                         My_Pred %in% c(1)& Actual_Result %in% c(NA)~"NA",
                         My_Pred %in% c(0)& Actual_Result %in% c(NA)~"NA",
                         TRUE~"No")) %>% 
  group_by(Check,Model) %>% 
  summarise(count=n()) %>% 
  pivot_wider(names_from="Check",values_from=c("count")) %>% 
  mutate(Pct=(Yes/(Yes+No)))

all_models_test1 <-all_models_final %>% 
  group_by(Model) %>% 
  summarise(Over_Pred=mean(Over_Pred),Under_Pred=mean(Under_Pred)) %>% 
  left_join(all_models_test)


write_xlsx(all_models_test1, "OU_forecast_models_extended_08042025.xlsx")

write_xlsx(all_models_final, "Full_Data_OU_models_extended_08042025.xlsx")




lr_final1 <- lr_final 
rf_final1 <- rf_final 
xg_final1 <- xg_final 


generate_calibration_summary <- function(df, model_name = "model", prob_col = "Over_Pred", pred_col = "My_Pred", actual_col = "Actual_Result") {
  df %>%
    filter(!is.na(.data[[prob_col]]), !is.na(.data[[actual_col]])) %>%
    mutate(conf_bin_over = cut(Over_Pred, breaks = seq(0.5, 1, by = 0.05), include.lowest = FALSE)) %>%
    mutate(conf_bin_under = cut(Under_Pred, breaks = seq(0.5, 1, by = 0.05), include.lowest = FALSE)) %>%
    group_by(conf_bin_under,conf_bin_over) %>%
    summarise(
      n = n(),
      accuracy = mean(.data[[pred_col]] == .data[[actual_col]], na.rm = TRUE),
      avg_conf_over = mean(Over_Pred, na.rm = TRUE),
      avg_conf_under = mean(Under_Pred, na.rm = TRUE),
      model = model_name,
      .groups = "drop"
    )
}

# Run across models
model_calibrations <- list(
  generate_calibration_summary(lr_final1, model_name = "lr"),
  generate_calibration_summary(rf_final1, model_name = "rf"),
  generate_calibration_summary(xg_final1, model_name = "xg"),
  generate_calibration_summary(ensemble_final, model_name = "ensemble"),
  generate_calibration_summary(bayes_final, model_name = "bayesian"),
  generate_calibration_summary(nn_final, model_name = "nnet")
) %>%
  bind_rows() %>%
  arrange(desc(conf_bin_over),desc(conf_bin_under))

print(model_calibrations)
# write_csv(model_calibrations, "OU_model_calibration_bins.csv")

# ========================
# CONTINUE WITH FINAL EXPORTS
# ========================

model_cal_over<-model_calibrations %>% 
  select(model,conf_bin_over,n,accuracy,avg_conf_over) %>% 
  filter(!is.na(conf_bin_over)) %>% 
  arrange(model,desc(n))

model_cal_under<-model_calibrations %>% 
  select(model,conf_bin_under,n,accuracy,avg_conf_under) %>% 
  filter(!is.na(conf_bin_under)) %>% 
  arrange(model,desc(n))

write_xlsx(model_cal_over, "Over_forecast_models_extended_bins_08042025.xlsx")
write_xlsx(model_cal_under, "Under_forecast_models_extended_bins_08042025.xlsx")