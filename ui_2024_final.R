# UI Header

header<-  dashboardHeader(
  title = "NFL Betting Model: Over/Under and Spread Analytics - BEAR DOWN!!!!!!",
  titleWidth = 750
)


# UI SidebarRound 
sidebar <- dashboardSidebar(
  
  # This is where you define the layout of the tabs
  sidebarMenu(
    # first tab item
    menuItem(
      "Intro/How To",
      tabName = "Intro_How_To",
      icon = icon("football-ball")
    ),
    
    menuItem(
      "Stats/Info",
      tabName = "Performance",
      icon = icon("football-ball")
    ),
    
    menuItem(
      "Round Robin Calculator",
      tabName = "Round_Robin_Calc",
      icon = icon("dollar-sign")
    ),
    
    
    menuItem(
      "Historical O/U Agg Models",
      tabName = "Historical_Analytics_Agg_OU",
      icon = icon("football-ball")
    ),
    
    
    menuItem(
      "Prediction O/U Full Detail",
      tabName = "Prediction_Analytics_Past",
      icon = icon("football-ball")
    ),
    menuItem(
      "Prediction O/U Agg Models",
      tabName = "Prediction_Analytics_Agg_Model_OU",
      icon = icon("football-ball")
    ),
    
    menuItem(
      "Historical Spread Agg Models",
      tabName = "Historical_Analytics_Agg_Spread",
      icon = icon("football-ball")
    ),
    
    menuItem(
      "Prediction Spread Full Detail",
      tabName = "Prediction_Analytics_Past1",
      icon = icon("football-ball")
    ),
    menuItem(
      "Prediction Spread Agg Models",
      tabName = "Prediction_Analytics_Agg_Model_Spread",
      icon = icon("football-ball")
    ),
    menuItem(
      "Appendix",
      tabName = "Appendix",
      icon = icon("football-ball")
    )
    
    
  )
  
)


# UI Body
body <- dashboardBody(
  # This is where you define the content of each tab
  tags$style(HTML("
                  
                  
                  
                   /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #CC5500;
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #CC5500;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #CC5500;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #CC5500;
                                }
                                
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #0B0B45;
                                }
                  
                  /* body */
                                .content-wrapper, .right-side {
                                background-color: #0B0B45;
                                }
                                
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #CC5500;
                                color:#FFFFFF ;
                                }
                  
         /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #0B0B45;
                                }
        
                  
                  .box.box-solid.box-primary>.box-header {
                  color:#0B0B45; 
                  background:#CC5500
                  }
                  
                  .box.box-solid.box-primary{
                  border-bottom-color:#CC5500;
                  border-left-color:#CC5500;
                  border-right-color:#CC5500;
                  border-top-color:#CC5500;
                  background:#ffffff
                  }
                  
                  ")),
  
  
  
  
  
  
  tabItems(
    ####Over Under
    #########################
    
#    tags$head(
#   tags$style(HTML("
#     .custom-dropdown-button .btn {
#       background-color: #CC5500 !important;
#       color: white !important;
#       border-color: #CC5500 !important;
#     }
#   "))
# ),

# New Tab (first tab)

tabItem(
  tabName = "Intro_How_To",
  style = "color:#CC5500",
  fluidPage(
    tags$style(HTML("
  .dropdown-menu {
    max-height: 400px; /* Adjust based on your needs */
    overflow-y: auto;
  }
")),
    fluidRow(
      column(12, align = "center",
             h2("If you don't have time to look through all the bells and whistles of the app and just want the picks please visit 
             tab 6 (Prediction O/U Aggregated Models) and tab 9 (Prediction Spread Aggregated Models)
                for the picks. Picks in our wheelhouse will be highlighted green. Additional information in the View Instructions tab below.
                The appendix will have additional model info, picks we're playing this week, 
                picks I agree/disagree with the model on for O/U and spread, player prop locks of the week and what's trending up/trending down in the NFL.", 
                style = "font-size: 22px; text-align: center;"),
    ),
    ),
    fluidRow(
      column(6, align = "center",
             h2("About Model/How To Use Model:", 
                style = "font-size: 24px; text-align: center;"),
                 dropdownButton(
                   label = "View Instructions", 
                   icon = icon("football-ball"),
                   status = "primary", 
                   circle = FALSE,
                   width = "100%",  # Removed trailing comma here
                   p("First and foremost, the only tabs you really need to use are the Prediction O/U Aggregated Models (tab 6) and Prediction Spread Aggregated Models (tab 9). 
This is where you go if you want to spend the least time in this app and just get the picks as noted above.
All picks that have a yes on the 'Mod All Agree High Conf' are in our wheelhouse (Should be highlighted green) and most of these are picks we will be choosing (We don't use all picks). 
You can use the up and down arrows on each column in the table to sort things how you would like and there are filters if you would like to get certain cuts of data.
The lines we are pulling into the model are based on Draftkings, so if you use a different app/see a different line just keep that in mind that you may need to use an alternate line to matchup with when we ran the model.
Also, lines do move quite often throughout the week and they are pretty volatile within 24 hours of the game.",
                     style = "font-size: 18px; text-align: center;"),
                   p("The yes in column `Mod All Agree High Conf` are picks where all 3 models agree on an outcome and they have high enough confidence.
The `High Conf` section is just where, in aggregate, the models average to high enough confidence, but at least one doesn't agree on an outcome. 
And `Mod All Agree` is where all 3 models agree, but the confidence isn't as high as we'd like. 
For under's and away team covering we have the high conf threshold at 59.5%+, so you should notice those being highlighted green as long as all 3 models also agree.
For over's and home team cover we lowered the threshold to 58%+ for home team covers and 57% for over's because of how the model leans towards unders and away team cover.
  We found that lowering that confidence threshold for home team and over allows us to play more games and actually improves accuracy overall.",
                     style = "font-size: 18px; text-align: center;"),
                   p("To highlight some of the alpha we found in Mod All Agree High Conf, take a look at tab 4 (Hist OU Agg Models) or tab 7 (Hist Spread Agg Models) and hit update. 
                   You'll notice the regular win % is quite a bit lower than the win % for Mod All Agree High Conf. Pretty staggering difference, isn't it? 
                   We found this little sweet spot before the 2023 season and it lead to a 60% overall accuracy for the model in it's rookie year. Hedge funds would kill for that!!",
                     style = "font-size: 18px; text-align: center;"),
                   p("Now you might notice that it's about 70% for OU and Spread on that Mod All Agree, High Conf. instead of the 60% we actually acheived.
That's because the tier ranking on those teams used a completed year tier for offense and defense rather than a live, week by week tier which is what the prediction picks will be using. 
So the historical has really good information on these teams. We want to use full year tiers in the historical data to bolster informative training data for the model.
Despite this, using the week by week tiers still yielded 60%+ accuracy and unfortunately we do not have the benefit of a completed year to assess the tiers week by week.
So point being, don't fall in love with that number as it's inflated, but it's more so to highlight the difference between regular picks win% and the alpha we found in Mod All Agree, High Conf." ,
                     style = "font-size: 18px; text-align: center;"),
                   p("People are also usually better at using their own intuition on spreads than they are on a game total, so use this model with your football insight as an advantage. 
You are still the end user, so don't be afraid to just use it to affirm your gut, sway you out of an on the fence play, etc. 
  We don't blindly play all these; we still have stay-away games that the model will like. More on this in the appendix.",
                     style = "font-size: 18px; text-align: center;"),
                   p("If you want information on the tiers, injuries, bet types, etc, please look in the appendix for more information. 
                   We tend to use the round robin betting style for our bets. 
                   The explanation of how round robins work can be seen in the 'Round Robin Calculator' tab and the appendix.
  P.S. This is not financial/betting advice nor am I responsible for any losses. Rooting for everyone to be a winner here!! 
  Also, please judge the model in 4 week increments opposed to week by week and in the case where things go cold, I don't want to hear complaining. You are the end user.",
                     style = "font-size: 18px; text-align: center;")
                 )
             
      ),


       # fluidRow(
          column(6, align = "center",
                 h2("About 3 Algorithms Used:", style = "font-size: 24px; text-align: center;"),
                 dropdownButton(
                   label = "View Algorithms", 
                   icon = icon("football-ball"),
                   status = "primary", 
                   circle = FALSE,
                   width = "100%",
                   h3("XGBoost", style = "font-size: 24px; text-align: center;"),
                   p("Best performing individual model for spread. XGBoost is like having a group of experts who learn from each other's mistakes to get better and better. 
Imagine a classroom where a teacher gives a test, and every time a student makes a mistake, the teacher gives extra help on that specific problem. 
XGBoost builds a series of decision trees, where each new tree focuses on the mistakes made by the previous ones. By doing this, it creates a very strong model that makes highly accurate predictions.",
                     style = "font-size: 18px; text-align: center;"),
               #    br(),
                   h3("Random Forest", style = "font-size: 24px; text-align: center;"),
                   p("Not the best performer at either spread or over/under but nonetheless has good enough accuracy to be included. Random Forest is like asking a group of experts for their opinions to make a decision. 
Each expert is given different information and comes up with their own conclusion. Then, they all vote, and the majority vote is the final decision. 
In Random Forest, each expert is actually a decision tree, which makes decisions by asking a series of yes/no questions.",
                     style = "font-size: 18px; text-align: center;"),
                   #br(),
                   h3("Logistic Regression", style = "font-size: 24px; text-align: center;"),
                   p("Best performing individual model for over/under. Logistic regression is like a smart way of drawing a line that separates two groups. Imagine you have a bunch of marbles—some are red, and some are blue. 
Logistic regression tries to divide the space so that most of the red marbles end up on one side and most of the blue marbles on the other.",
                     style = "font-size: 18px; text-align: center;")
                 ),
               
          
 
    ),
),
),
),
    
    
    
    tabItem(
      tabName="Performance",
      style="color:#CC5500",
      fluidPage(
        fluidRow(
          column(12, align = "center",
                 h2("Hit Rates Overview", style = "font-size: 30px; text-align: center;"),
                 p("Here are the historical hit rates for overs/unders and away team/home team covering since 2021 (Actual outcome's not model %'s):", 
                   style = "font-size: 22px; text-align: center;")
          )
        ),
        # fluidRow(
        column(3,
               h4("Away Team Cover Rate"),
               verbatimTextOutput("away_hit_rate_output")
        ),
        # ),
        #fluidRow(
        column(3,
               h4("Home Team Cover Rate"),
               verbatimTextOutput("home_hit_rate_output")
        ),
        #  ),
        # fluidRow(
        column(3,
               h4("Under Hit Rate"),
               verbatimTextOutput("under_hit_rate_output")
        ),
        # ),
        #fluidRow(
        column(3,
               h4("Over Hit Rate"),
               verbatimTextOutput("over_hit_rate_output")
        ),
        fluidRow(
          column(12, align = "center",
                 h2("Note when looking through the model:", style = "font-size: 30px; text-align: center;"),
                 dropdownButton(
                   label = "View Instructions", 
                   icon = icon("football-ball"),
                   status = "primary", 
                   circle = FALSE,
                   width = "100%",  # Removed trailing comma here
                 p("You might notice this model leans towards the under and towards away teams covering. 
                     I'd say that's a good thing because as you can see above, under's hit more than over's...even though they are less fun to root for. 
                     And the away team covers slightly more than the home team, doesn't mean they win more necessarily but they cover more (Guess home field ain't all that?!)", 
                   style = "font-size: 20px; text-align: center;")
          ),
          ),
        ),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        #),
        fluidRow(
          column(6, align = "center",
                 h4("Lines we LOVE staying under (if the model picks an over, try to tease/stay below these key numbers)", 
                    style = "font-size: 22px; text-align: center;"),
                 selectInput("ou_number", "Choose a number:", 
                             choices = c(37.5,38.5, 41.5,44.5,47.5, 48.5, 51.5)),
                 tags$style(HTML("
            #ou_number 
            {
                font-size: 16px;
                text-align: center;
            }
         ")),
                 verbatimTextOutput("ou_blurb_output")
          ),
          column(6, align = "center",
                 h4("Lines we LIKE staying under (if the model picks an over, try to tease/stay below these key numbers)", 
                    style = "font-size: 22px; text-align: center;"),
                 selectInput("ou_number1", "Choose a number:", 
                             choices = c(37,38,40.5,41,44,45.5,47,48,51,54.5)),
                 tags$style(HTML("
            #ou_number1 {
                font-size: 20px;
                text-align: center;
            }
         ")),
                 verbatimTextOutput("ou_blurb_output1")
          )
        ),
        
        
        fluidRow(
          column(6, align = "center",
                 h4("Spreads we LOVE playing", 
                    style = "font-size: 22px; text-align: center;"),
                 selectInput("spread_number", "Choose a number:", 
                             choices = c('+3.5','+7.5','+10.5','-2.5','-6.5')),
                 tags$style(HTML("
            #ou_number 
            {
                font-size: 16px;
                text-align: center;
            }
         ")),
                 verbatimTextOutput("spread_blurb_output")
          ),
          column(6, align = "center",
                 h4("Spreads we LIKE playing", 
                    style = "font-size: 22px; text-align: center;"),
                 selectInput("spread_number1", "Choose a number:", 
                             choices = c('+3','+4','+4.5','+7','+8','+8.5','+10','+11.5','+14.5','-3','-9.5')),
                 tags$style(HTML("
            #ou_number1 {
                font-size: 20px;
                text-align: center;
            }
         ")),
                 verbatimTextOutput("spread_blurb_output1")
          )
        ),
        
        
        
        
      )
    ),
        
        
        tabItem(
          tabName="Round_Robin_Calc",
          style="color:#CC5500",
          fluidPage(
        fluidRow(
          column(12, align = "center",
                 div(class = "info-box",
                     h4("Round Robin Expected Value/Profit Calculator: Below is an expected value/expected profit table breakdown for the Round Robin strategy.",
                        style = "font-size: 24px; text-align: center;"),
              p("Total number of legs is the amount of games you will be playing in total. The default will be 6 legs as that's what we typically play the most. When the model has a bunch of picks we also like to play 8 legs",
                       style = "color:#CC5500; font-size: 18px; text-align: center;"),  
                     p(" Minimum winnings legs is the minimum amount of legs to hit one combo of games. Just because you hit the minimum amount of legs doesn't always mean the bet will be profitable. You can test that below. 
               The higher the minimum winning legs the bigger the payout but the higher risk of losing everything.  
               To make this simpler, think of how a slot machine works, if you are minimum betting you will win less on the big bonus round spins or on a jackpot, 
               but if you max bet or bet higher than the minimum you would win more.",
                       style = "color:#CC5500; font-size: 18px; text-align: center;"),  
               
                     p("The total bet amount is how much you are willing to spend in total on the round robin bet or your total bet amount.
              When actually placing the round robin on the app of your choice, you need to be careful. In the app of your choice you'll see an number such as 15x for a 6 leg, 4 pick round robin.
              So if you wanted to place $2500 in total, you would actually want to do $166.67 as the amount as it's on a per outcome group basis.
              Since in this example there are 15 different combination groups by allocating $166.67 to each group, you would land on a total of $2500. i.e. $166.67 * 15 = ~$2500.",
                       style = "color:#CC5500; font-size: 18px; text-align: center;"),  
                     p("Win probability is basically scenario testing on the model's accuracy so you can do doomsday scenarios, mid-tier scenarios, aggressive scenarios, etc.
                       The default is 60% here because that is what we're hoping to achieve as far as model accuracy for the 2024 season!",
                       style = "color:#CC5500; font-size: 18px; text-align: center;"),  
               p("The 'Select Odds' are how much return you get on the dollar. These would be average odds.. 
               so it would average all your individual legs and spit out an average odds to keep things simple in this tool.
                       Below are listed odds from +100 to -500. Let's look at a few examples. 
                       Example 1: -110 odds, that would be 100/110 which is 0.91 cents on the dollar. 
                       Example 2: +100 or -100 odds, that would be 100/100 which is 1 dollar to 1 dollar. 
                       Example 3: -500 odds, that would be 100/500 which is 0.20 cents on the dollar.
                       Often times the standard line/or most seen line by say a Draftkings would be -110, which is why it's the default selection here. Books are greedy.",
                       style = "color:#CC5500; font-size: 18px; text-align: center;"),  
                     numericInput("total_legs", "Total Number of Legs:", value = 6, min = 3),
                     numericInput("win_legs", "Minimum Winning Legs:", value = 4, min = 1, max = 6),
                     numericInput("weekly_bet", "Total Bet Amount ($):", value = 2500, min = 1000),
                     numericInput("win_prob", "Win Probability (%):", value = 60, min = 0, max = 100, step = 0.1),
              selectInput("odds", "Select Odds:", choices = generate_odds_choices(), selected = 1.9090),
                     DTOutput("round_robin_table")
                 )
          )
        ),
      ),
    ),
    
#  ),

# numericInput("win_prob", "Win Probability (%):", value = 60, min = 0, max = 100, step = 0.1),
# selectInput("odds", "Select Odds:",choices = c('+100'=2,'-105'=1.9523,'-110' = 1.909, '-115' = 1.869, '-120' = 1.833,'-125'=1.8),selected=1.909),     
    
    tabItem(
      tabName = "Prediction_Analytics_Past",
      style="color:#CC5500",
      fluidPage(
        titlePanel("Prediction O/U Full Detail (shows individual model detail, wind/temp, etc.)"),
        
        
        fluidRow(
          
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "Model1",
                        label = "Model Type?:",
                        choices= c("lr","xg","rf"),
                        selected = c("lr","xg","rf"),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          #Idea to make historical tab and prediction tab with year
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "season1",
                        label = "Season:",
                        choices= c("2024"),
                        selected = c("2024"),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "Matchup1",
                        label = "Matchup:",
                        choices= matchup_24,
                        selected = matchup_24,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5",
                          `live-search`=TRUE
                        ), 
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "away_team1",
                        label = "Away Team:",
                        choices= away_team,
                        selected = away_team,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5"
                        ), 
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "home_team1",
                        label = "Home Team:",
                        choices= home_team,
                        selected = home_team,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5"
                        ), 
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          
          
          
          
        ),
        
        
        
        
        
        column(12,
               actionButton(inputId = "UPDATE1",
                            label = HTML(
                              (paste("<font color='#0B0B45'><font size='5'><b>",
                                     "UPDATE",
                                     "</font color='#0B0B45'></font size='5'></b>"))
                            ),
                            icon = icon("sync-alt"),
                            width = "200px",
                            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45; padding: 2px; font-size: 160%; text-align: center; font-family: arial"
                            # style = "display:inline; height: 150px; width: 150px; border-radius: 150%;  color: #FFFFFF; background-color: #CB177D; border-color: #FFFFFF; padding: 1px; font-size: 120%; text-align: center; font-family: arial",
               )
               , 
               align = "center"
               
        ),
        
        fluidPage(
          # Add custom style for the infoBox
          tags$head(
            tags$style(HTML("
      .custom-infobox .info-box {
        background-color: #CC5500 !important;
        color: white !important;
      }
    "))
          ),
          
          fluidRow(
            # Custom styled Info Box for how to use the grouping variables
            div(class = "custom-infobox",
                infoBox(
                  title = "How to Use Grouping Variables",
                  subtitle = "Default or all selected is the entire prediction data set. You'll be able to see all the detail you want when picking week 1 games.
                  If you uncheck a grouping variable and hit update, the data will be 'rolled over' that level of detail. 
                  Unchecking all will show the high level stats for the week such as average under/over prediction and not show any individual games.
                  Mess around with a few examples and you should get the hang of how it works.
                  ",
                  icon = icon("info-circle"),
                  color = "blue",  # This color will be overridden by CSS
                  width = 12, 
                  fill = TRUE
                )
            )
          )
        ),
        
        fluidRow(
          # Grouping Variables
          HTML(paste("<center><font color='#0B0B45'><font size='5'><font family='arial'><b>",
                     #  paste0("Grouping Variables"),
                     "</center></font color='#0B0B45'></font size='5'></font family='arial'></b>")),
          # Group By 
          box(
            checkboxGroupInput(inputId = "GROUP_BY1",
                               label = HTML(
                                 (paste("<font color='#0B0B45'><font size='4'><i>",
                                        "**Selecting your grouping variables, none will be at All Team, All Model, All Week, All Season level**",
                                        "</font color='#0B0B45'></font size='4'></i>"))
                               ),
                               choices = c("model","away_team","away_off","away_def","home_team","home_off","home_def"
                                           ,"wind","temp"),
                               
                               selected = c("model","away_team","away_off","away_def","home_team","home_off","home_def"
                                            ,"wind","temp"),
                               inline = TRUE),
            solidHeader = T, 
            collapsible = T, 
            collapsed = F,
            title = span(textOutput("title3"),
                         style="color:navy"),
            status = "primary",
            width = 12
          ),
        ),
        
        
        
        mainPanel(width=12,
                  htmlOutput("num_model1"),
                  htmlOutput("num_matchup1"),
                  htmlOutput("num_home_team1"),
                  htmlOutput("num_away_team1"),
                  htmlOutput("num_season1"),
                  fluidRow(
                    box(DTOutput("compare_table1"), solidHeader = T, collapsible = T, collapsed = F, 
                        title = span(textOutput("title4"),
                                     style="color:navy"), status = "primary",width = 14))
                  
        )
        
        
      )
      
    ),
    
    tabItem(
      tabName = "Prediction_Analytics_Agg_Model_OU",
      style="color:#CC5500",
      fluidPage(
        titlePanel("Prediction O/U Agg Models"),
  
        
        fluidRow(
          
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "MODEL_COMBO_OU_PRED",
                        label = "MODEL COMBO?:",
                        choices= c("All","LRXG","LRRF","RFXG","LR","RF","XG"),
                        selected = c("All"),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = FALSE
            ),
            
            width=3
          ),
          
          #Idea to make historical tab and prediction tab with year
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "season11",
                        label = "Season:",
                        choices= c("2024"),
                        selected = c("2024"),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "Matchup11",
                        label = "Matchup:",
                        choices= matchup_24,
                        selected = matchup_24,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5",
                          `live-search`=TRUE
                        ), 
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "away_team11",
                        label = "Away Team:",
                        choices= away_team,
                        selected = away_team,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5"
                        ), 
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "home_team11",
                        label = "Home Team:",
                        choices= home_team,
                        selected = home_team,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5"
                        ), 
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          
          
          
          
        ),
        
        
        
        
        
        column(12,
               actionButton(inputId = "UPDATE11",
                            label = HTML(
                              (paste("<font color='#0B0B45'><font size='5'><b>",
                                     "UPDATE",
                                     "</font color='#0B0B45'></font size='5'></b>"))
                            ),
                            icon = icon("sync-alt"),
                            width = "200px",
                            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45; padding: 2px; font-size: 160%; text-align: center; font-family: arial"
                            # style = "display:inline; height: 150px; width: 150px; border-radius: 150%;  color: #FFFFFF; background-color: #CB177D; border-color: #FFFFFF; padding: 1px; font-size: 120%; text-align: center; font-family: arial",
               )
               , 
               align = "center"
               
        ),
        
        # fluidRow(
        #   # Grouping Variables
        #   HTML(paste("<center><font color='#0B0B45'><font size='5'><font family='arial'><b>",
        #              #  paste0("Grouping Variables"),
        #              "</center></font color='#0B0B45'></font size='5'></font family='arial'></b>")),
        #   # Group By
        #   box(
        #     checkboxGroupInput(inputId = "GROUP_BY11",
        #                        label = HTML(
        #                          (paste("<font color='#0B0B45'><font size='4'><i>",
        #                                 "**Selecting your grouping variables, none will be at All Team, All Model, Week level**",
        #                                 "</font color='#0B0B45'></font size='4'></i>"))
        #                        ),
        #                        choices = c("away_team","away_off","away_def","home_team","home_off","home_def"),
        # 
        #                        selected = c("away_team","away_off","away_def","home_team","home_off","home_def"),
        #                        inline = TRUE),
        #     solidHeader = T,
        #     collapsible = T,
        #     collapsed = F,
        #     title = span(textOutput("title33"),
        #                  style="color:navy"),
        #     status = "primary",
        #     width = 12
        #   ),
        # ),
        
        
        
        mainPanel(width=12,
                  # htmlOutput("num_model11"),
                  htmlOutput("num_matchup11"),
                  htmlOutput("num_home_team11"),
                  htmlOutput("num_away_team11"),
                  htmlOutput("num_season11"),
                  fluidRow(
                    box(DTOutput("compare_table11"), solidHeader = T, collapsible = T, collapsed = F, 
                        title = span(textOutput("title44"),
                                     style="color:navy"), status = "primary",width = 14))
                  
        )
        
        
      )
      
    ),
    
    
    
    
    
    tabItem(
      tabName = "Historical_Analytics_Agg_OU",
      style="color:#CC5500",
      fluidPage(
        titlePanel("Agg all 3 Model OU Past"),
        
        fluidRow(
          
          
          
          #Idea to make historical tab and prediction tab with year
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "MODEL_COMBO_OU",
                        label = "MODEL COMBO?:",
                        choices= c("All","LRXG","LRRF","RFXG","LR","RF","XG"),
                        selected = c("All"),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = FALSE
            ),
            
            width=3
          ),
          
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "season2",
                        label = "Season:",
                        choices= c("2021","2022","2023","2024"),
                        selected = c("2021","2022","2023","2024"),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = TRUE
            ),
            
            width=3
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "week2",
                        label = "Week:",
                        choices= c(week),
                        selected = c(week),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = TRUE
            ),
            
            width=3
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "Result2",
                        label = "Result?:",
                        choices= c("Correct","Incorrect"),
                        selected = c("Correct","Incorrect"),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = TRUE
            ),
            
            width=3
          ),
          
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "Matchup2",
                        label = "Matchup:",
                        choices= matchup_23_22_21,
                        selected = matchup_23_22_21,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5",
                          `live-search`=TRUE
                        ), 
                        multiple = TRUE
            ),
            
            width=4
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "away_team2",
                        label = "Away Team:",
                        choices= away_team,
                        selected = away_team,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5"
                        ), 
                        multiple = TRUE
            ),
            
            width=4
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "home_team2",
                        label = "Home Team:",
                        choices= home_team,
                        selected = home_team,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5"
                        ), 
                        multiple = TRUE
            ),
            
            width=4
          ),
          
          
          
          
        ),
        
        
        
        
        column(12,
               actionButton(inputId = "UPDATE2",
                            label = HTML(
                              (paste("<font color='#0B0B45'><font size='5'><b>",
                                     "UPDATE",
                                     "</font color='#0B0B45'></font size='5'></b>"))
                            ),
                            icon = icon("sync-alt"),
                            width = "200px",
                            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45; padding: 2px; font-size: 160%; text-align: center; font-family: arial"
                            # style = "display:inline; height: 150px; width: 150px; border-radius: 150%;  color: #FFFFFF; background-color: #CB177D; border-color: #FFFFFF; padding: 1px; font-size: 120%; text-align: center; font-family: arial",
               )
               , 
               align = "center"
               
        ),
        
        fluidPage(
          # Add custom style for the infoBox.
          tags$head(
            tags$style(HTML("
      .custom-infobox .info-box {
        background-color: #CC5500 !important;
        color: white !important;
      }
    "))
          ),
        
          fluidRow(
            # Custom styled Info Box for how to use the grouping variables
            div(class = "custom-infobox",
                infoBox(
                  title = "How to Use Grouping Variables",
                  subtitle = "Default or none selected is just rolling up data over season, week, away team and home team. This should give you very high level stats when you hit Update. 
                     If you click on check the season box and hit update, you will get season level detail. If you hit season and week, you will get season and week level detail.
                     If you check all, you will get the full detail for this tab.",
                  icon = icon("info-circle"),
                  color = "yellow",  # This color will be overridden by CSS
                  width = 12, 
                  fill = TRUE
                )
            )
          )
        ),
        
        fluidRow(
          # Grouping Variables
          HTML(paste("<center><font color='#0B0B45'><font size='5'><font family='arial'><b>",
                     #  paste0("Grouping Variables"),
                     "</center></font color='#0B0B45'></font size='5'></font family='arial'></b>")),
          # Group By 
          box(
            checkboxGroupInput(inputId = "GROUP_BY2",
                               label = HTML(
                                 (paste("<font color='#0B0B45'><font size='4'><i>",
                                        "**Selecting your grouping variables, none will be at All Team, All Model, All Week, All Season level**",
                                        "</font color='#0B0B45'></font size='4'></i>"))
                               ),
                               choices = c("season","week","away_team","home_team"
                                           
                               ),
                               
                               # selected = c("season","week","away_team","home_team"
                               # ),
                               inline = TRUE),
            solidHeader = T, 
            collapsible = T, 
            collapsed = F,
            title = span(textOutput("title5"),
                         style="color:navy"),
            status = "primary",
            width = 12
          ),
        ),
        
        
        
        mainPanel(width=12,
                  htmlOutput("num_Result2"),
                  #  htmlOutput("num_model2"),
                  htmlOutput("num_matchup2"),
                  htmlOutput("num_home_team2"),
                  htmlOutput("num_away_team2"),
                  htmlOutput("num_season2"),
                  htmlOutput("num_week2"),
                  fluidRow(
                    box(DTOutput("compare_table2"), solidHeader = T, collapsible = T, collapsed = F, 
                        title = span(textOutput("title6"),
                                     style="color:navy"), status = "primary",width = 14))
                  
        )
        
        
      )
      
    ),
    
    #####Spread  
    
    ###Spread
    #################
    ########################################
    
    
    
    
    tabItem(
      tabName = "Prediction_Analytics_Past1",
      style="color:#CC5500",
      fluidPage(
        titlePanel("Prediction Spread Full Detail (shows individual model detail, wind/temp, etc.)"),
        
        
        fluidRow(
          
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "Model101",
                        label = "Model Type?:",
                        choices= c("lr","xg","rf"),
                        selected = c("lr","xg","rf"),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          #Idea to make historical tab and prediction tab with year
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "season101",
                        label = "Season:",
                        choices= c("2024"),
                        selected = c("2024"),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "Matchup101",
                        label = "Matchup:",
                        choices= matchup_24,
                        selected = matchup_24,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5",
                          `live-search`=TRUE
                        ), 
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "away_team101",
                        label = "Away Team:",
                        choices= away_team,
                        selected = away_team,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5"
                        ), 
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "home_team101",
                        label = "Home Team:",
                        choices= home_team,
                        selected = home_team,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5"
                        ), 
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          
          
          
          
        ),
        
        
        
        
        
        column(12,
               actionButton(inputId = "UPDATE101",
                            label = HTML(
                              (paste("<font color='#0B0B45'><font size='5'><b>",
                                     "UPDATE",
                                     "</font color='#0B0B45'></font size='5'></b>"))
                            ),
                            icon = icon("sync-alt"),
                            width = "200px",
                            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45; padding: 2px; font-size: 160%; text-align: center; font-family: arial"
                            # style = "display:inline; height: 150px; width: 150px; border-radius: 150%;  color: #FFFFFF; background-color: #CB177D; border-color: #FFFFFF; padding: 1px; font-size: 120%; text-align: center; font-family: arial",
               )
               , 
               align = "center"
               
        ),
        
        fluidPage(
          # Add custom style for the infoBox
          tags$head(
            tags$style(HTML("
      .custom-infobox .info-box {
        background-color: #CC5500 !important;
        color: white !important;
      }
    "))
          ),
          
          fluidRow(
            # Custom styled Info Box for how to use the grouping variables
            div(class = "custom-infobox",
                infoBox(
                  title = "How to Use Grouping Variables",
                  subtitle = "Default or all selected is the entire prediction data set. You'll be able to see all the detail you want when picking week 1 games.
                  If you uncheck a grouping variable and hit update, the data will be 'rolled over' that level of detail. 
                  Unchecking all will show the high level stats for the week such as average away cover/home cover prediction and not show any individual games.
                  Mess around with a few examples and you should get the hang of how it works.
                  ",
                  icon = icon("info-circle"),
                  color = "yellow",  # This color will be overridden by CSS
                  width = 12, 
                  fill = TRUE
                )
            )
          )
        ),
        
        fluidRow(
          # Grouping Variables
          HTML(paste("<center><font color='#0B0B45'><font size='5'><font family='arial'><b>",
                     #  paste0("Grouping Variables"),
                     "</center></font color='#0B0B45'></font size='5'></font family='arial'></b>")),
          # Group By 
          box(
            checkboxGroupInput(inputId = "GROUP_BY101",
                               label = HTML(
                                 (paste("<font color='#0B0B45'><font size='4'><i>",
                                        "**Selecting your grouping variables, none will be at All Team, All Model, All Week, All Season level**",
                                        "</font color='#0B0B45'></font size='4'></i>"))
                               ),
                               choices = c("model","away_team","away_off","away_def","home_team","home_off","home_def"
                                           ,"wind","temp"),
                               
                               selected = c("model","away_team","away_off","away_def","home_team","home_off","home_def"
                                            ,"wind","temp"),
                               inline = TRUE),
            solidHeader = T, 
            collapsible = T, 
            collapsed = F,
            title = span(textOutput("title103"),
                         style="color:navy"),
            status = "primary",
            width = 12
          ),
        ),
        
        
        
        mainPanel(width=12,
                  htmlOutput("num_model101"),
                  htmlOutput("num_matchup101"),
                  htmlOutput("num_home_team101"),
                  htmlOutput("num_away_team101"),
                  htmlOutput("num_season101"),
                  fluidRow(
                    box(DTOutput("compare_table101"), solidHeader = T, collapsible = T, collapsed = F, 
                        title = span(textOutput("title104"),
                                     style="color:navy"), status = "primary",width = 14))
                  
        )
        
        
      )
      
    ),
    
    
    
    tabItem(
      tabName = "Prediction_Analytics_Agg_Model_Spread",
      style="color:#CC5500",
      fluidPage(
        titlePanel("Pred Spread Agg Model"),
        
        
        fluidRow(
          
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "MODEL_COMBO_SPREAD_PRED",
                        label = "MODEL COMBO?:",
                        choices= c("All","LRXG","LRRF","RFXG","LR","RF","XG"),
                        selected = c("All"),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = FALSE
            ),
            
            width=3
          ),
          
          #Idea to make historical tab and prediction tab with year
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "season1011",
                        label = "Season:",
                        choices= c("2024"),
                        selected = c("2024"),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "Matchup1011",
                        label = "Matchup:",
                        choices= matchup_24,
                        selected = matchup_24,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5",
                          `live-search`=TRUE
                        ), 
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "away_team1011",
                        label = "Away Team:",
                        choices= away_team,
                        selected = away_team,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5"
                        ), 
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "home_team1011",
                        label = "Home Team:",
                        choices= home_team,
                        selected = home_team,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5"
                        ), 
                        multiple = TRUE
            ),
            
            width=2
          ),
          
          
          
          
          
        ),
        
        
        
        
        
        column(12,
               actionButton(inputId = "UPDATE1011",
                            label = HTML(
                              (paste("<font color='#0B0B45'><font size='5'><b>",
                                     "UPDATE",
                                     "</font color='#0B0B45'></font size='5'></b>"))
                            ),
                            icon = icon("sync-alt"),
                            width = "200px",
                            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45; padding: 2px; font-size: 160%; text-align: center; font-family: arial"
                            # style = "display:inline; height: 150px; width: 150px; border-radius: 150%;  color: #FFFFFF; background-color: #CB177D; border-color: #FFFFFF; padding: 1px; font-size: 120%; text-align: center; font-family: arial",
               )
               , 
               align = "center"
               
        ),
        
        # fluidRow(
        #   # Grouping Variables
        #   HTML(paste("<center><font color='#0B0B45'><font size='5'><font family='arial'><b>",
        #              #  paste0("Grouping Variables"),
        #              "</center></font color='#0B0B45'></font size='5'></font family='arial'></b>")),
        #   # Group By 
        #   box(
        #     checkboxGroupInput(inputId = "GROUP_BY1011",
        #                        label = HTML(
        #                          (paste("<font color='#0B0B45'><font size='4'><i>",
        #                                 "**Selecting your grouping variables, none will be at All Team, All Model, Week level**",
        #                                 "</font color='#0B0B45'></font size='4'></i>"))
        #                        ),
        #                        choices = c("model","away_team","away_off","away_def","home_team","home_off","home_def"
        #                                    ,"wind","temp"),
        #                        
        #                        selected = c("model","away_team","away_off","away_def","home_team","home_off","home_def"
        #                                     ,"wind","temp"),
        #                        inline = TRUE),
        #     solidHeader = T, 
        #     collapsible = T, 
        #     collapsed = F,
        #     title = span(textOutput("title1033"),
        #                  style="color:navy"),
        #     status = "primary",
        #     width = 12
        #   ),
        # ),
        
        
        
        mainPanel(width=12,
                  #htmlOutput("num_model1011"),
                  htmlOutput("num_matchup1011"),
                  htmlOutput("num_home_team1011"),
                  htmlOutput("num_away_team1011"),
                  htmlOutput("num_season1011"),
                  fluidRow(
                    box(DTOutput("compare_table1011"), solidHeader = T, collapsible = T, collapsed = F, 
                        title = span(textOutput("title1044"),
                                     style="color:navy"), status = "primary",width = 14))
                  
        )
        
        
      )
      
    ),
    
    
    
    tabItem(
      tabName = "Historical_Analytics_Agg_Spread",
      style="color:#CC5500",
      fluidPage(
        titlePanel("Historical Analytics Spread Agg Models"),
        
        fluidRow(
          
          
          #Idea to make historical tab and prediction tab with year
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "MODEL_COMBO_SPREAD",
                        label = "MODEL COMBO?:",
                        choices= c("All","LRXG","LRRF","RFXG","LR","RF","XG"),
                        selected = c("All"),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = FALSE
            ),
            
            width=3
          ),
          
          #Idea to make historical tab and prediction tab with year
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "season102",
                        label = "Season:",
                        choices= c("2021","2022","2023","2024"),
                        selected = c("2021","2022","2023","2024"),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = TRUE
            ),
            
            width=3
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "week102",
                        label = "Week:",
                        choices= c(week),
                        selected = c(week),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = TRUE
            ),
            
            width=3
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "Result102",
                        label = "Result?:",
                        choices= c("Correct","Incorrect"),
                        selected = c("Correct","Incorrect"),
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 5"
                        ),
                        multiple = TRUE
            ),
            
            width=3
          ),
          
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "Matchup102",
                        label = "Matchup:",
                        choices= matchup_23_22_21,
                        selected = matchup_23_22_21,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5",
                          `live-search`=TRUE
                        ), 
                        multiple = TRUE
            ),
            
            width=4
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "away_team102",
                        label = "Away Team:",
                        choices= away_team,
                        selected = away_team,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5"
                        ), 
                        multiple = TRUE
            ),
            
            width=4
          ),
          
          box(
            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45",
            background = "navy",
            pickerInput(inputId = "home_team102",
                        label = "Home Team:",
                        choices= home_team,
                        selected = home_team,
                        options = list(
                          `actions-box` = TRUE, 
                          `selected-text-format` = "count > 5"
                        ), 
                        multiple = TRUE
            ),
            
            width=4
          ),
          
          
          
          
        ),
        
        
        
        
        column(12,
               actionButton(inputId = "UPDATE102",
                            label = HTML(
                              (paste("<font color='#0B0B45'><font size='5'><b>",
                                     "UPDATE",
                                     "</font color='#0B0B45'></font size='5'></b>"))
                            ),
                            icon = icon("sync-alt"),
                            width = "200px",
                            style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45; padding: 2px; font-size: 160%; text-align: center; font-family: arial"
                            # style = "display:inline; height: 150px; width: 150px; border-radius: 150%;  color: #FFFFFF; background-color: #CB177D; border-color: #FFFFFF; padding: 1px; font-size: 120%; text-align: center; font-family: arial",
               )
               , 
               align = "center"
               
        ),
        
        fluidPage(
          # Add custom style for the infoBox
          tags$head(
            tags$style(HTML("
      .custom-infobox .info-box {
        background-color: #CC5500 !important;
        color: white !important;
      }
    "))
          ),
          
          fluidRow(
            # Custom styled Info Box for how to use the grouping variables
            div(class = "custom-infobox",
                infoBox(
                  title = "How to Use Grouping Variables",
                  subtitle = "Default or none selected is just rolling up data over season, week, away team and home team. This should give you very high level stats when you hit Update. 
                     If you click on check the season box and hit update, you will get season level detail. If you hit season and week, you will get season and week level detail.
                     If you check all, you will get the full detail for this tab.",
                  icon = icon("info-circle"),
                  color = "yellow",  # This color will be overridden by CSS
                  width = 12, 
                  fill = TRUE
                )
            )
          )
        ),
        
        fluidRow(
          # Grouping Variables
          HTML(paste("<center><font color='#0B0B45'><font size='5'><font family='arial'><b>",
                     #  paste0("Grouping Variables"),
                     "</center></font color='#0B0B45'></font size='5'></font family='arial'></b>")),
          # Group By 
          box(
            checkboxGroupInput(inputId = "GROUP_BY102",
                               label = HTML(
                                 (paste("<font color='#0B0B45'><font size='4'><i>",
                                        "**Selecting your grouping variables, none will be at All Team, All Model, All Week, All Season level**",
                                        "</font color='#0B0B45'></font size='4'></i>"))
                               ),
                               choices = c("season","week","away_team","home_team"
                               ),
                               
                               # selected = c("season","week","away_team","home_team"
                               # ),
                               inline = TRUE),
            solidHeader = T, 
            collapsible = T, 
            collapsed = F,
            title = span(textOutput("title105"),
                         style="color:navy"),
            status = "primary",
            width = 12
          ),
        ),
        
        
        
        mainPanel(width=12,
                  htmlOutput("num_Result102"),
                  #  htmlOutput("num_model102"),
                  htmlOutput("num_matchup102"),
                  htmlOutput("num_home_team102"),
                  htmlOutput("num_away_team102"),
                  htmlOutput("num_season102"),
                  htmlOutput("num_week102"),
                  fluidRow(
                    box(DTOutput("compare_table102"), solidHeader = T, collapsible = T, collapsed = F, 
                        title = span(textOutput("title106"),
                                     style="color:navy"), status = "primary",width = 14))
                  
        )
        
        
      )
      
      
    ),
    
    
  


tabItem(
  tabName = "Appendix",
  style = "color:#CC5500",
  fluidPage(
    tags$head(
      tags$style(HTML("
      #main-title {
        font-size: 30px;
        color: #CC5000;
        text-align: center;
          margin: 10 px;
      }
      .big-text {
        font-size: 20px;
        font-weight: bold;
        color: #CC5000;
          margin: auto;
      }
      .centered-tab {
        margin: auto;
        width: 100%;  /* Adjust width as needed */
      }
    "))
    ),
    fluidRow(
      column(12, align = "center",
             h3("Other Items to Note:", style = "font-size: 30px; text-align: center;"),
             dropdownButton(
               label = "View Appendix/Other Notable Items",
               icon = icon("football-ball"),
               status = "primary",
               circle = FALSE,
               width = "100%",
               p("Bet types: We usually bet using 4 pick 6 leg round robins, so that means we pick 6 games and 4 out of 6 have to hit for us to make money.
               If 5 out of 6 hit, it triggers some more combinations, and.. we get a big payday! If 6 out of 6 hit.. we are partying hard!! This happened last year! 
                 More on round robins on tab 3 and you can mess around with expected profits.
               It's hard to recommend to tell you how to bet, that's up to your personal style. I could see two or three parlay leg type bets being a good option for return. 
               I could see round robin being a good option if you are willing to put up good $. I could see teasers having some good potential too. Or just play 3 straight bets per week and shoot for going 2 out of 3. 
               I am not a betting portfolio manager unfortunately so that part will be up to you.",
                 style = "font-size: 20px; text-align: center;"),
               p("Things to watch out for: I will be monitoring the kickoff rule change. I've read that there is a 1% increase in points scored for every extra 1 yard in starting field position. 
                      Last year starting field position was around the 25. This is projected to be at the 29-30 yard line this year with the new kickoff rule. 
                      That's a 4-5% point adjustment on the total and will favor teams with strong special teams.
                      I'm not sure if DraftKings/FanDuel have fully baked this in because it's difficult to project due to the lack of historical impact on this rule.
                      We'll adjust after 4 weeks if necessary. 
                 An example of an adjustment would be an offense in the mid-tier being bumped up to high-tier because the bump in points projected now makes them a tier higher.",
                      style = "font-size: 20px; text-align: center;"),
               p("Stay away games/Injuries: Injuries are another thing to watch out for. For example, Jordan Love just got hurt for the Packers and will miss some time. 
                      I would highly recommend staying away from games like that for at least a week so we can see how things play out, that includes the over/under too, not just spread. 
                      It would be really noisy at the moment to bake in all these injuries but down the road it might be beneficial to create like an injury factor that's weighted by position. 
                      This most likely won't get added this year and is something in general that may be hard to model out. 
                      Getting back to the main point, I would still probably entertain playing a model picked game if say the number 2 wide receiver is out for a team.
                      The other thing is that the actual spread or over/under line should have some level of injury impact this baked in by the books. 
                      I wouldn't manually adjust the tiers for a less impactful injury as everything else should be reasonably picked up by the model via the current spread or over/under line.",
                      style = "font-size: 20px; text-align: center;"),
               p("Weather: Wind and temperature is a variable accounted for in our model and you'll see it as a range in the 'Full Detail' prediction tabs.
                      For temperature, we use ranges Below 25 degrees, 25 to 55 degrees, 55 to 85 degrees and above 85 degrees. For wind we use ranges 0 to 10 mph, 10 to 15 mph, 15 to 20 mph, and above 20 mph. 
                      This would just be wind speed, not gusts. These ranges were decided after reading through an article on covers.com that ran analytics on wind, temp, snow, rain.
                  We currently don't account for rain or snow, it has been shown that rain has low impact on a game total, but it does slightly decrease passing production but not anything that materially changes the total. 
                 Snow on the other hand does have an impact on totals and might be a worthwhile variable to include as it drops the field goal conversion rate by 7%. 
                 Snow and rain can stop and go during a game where temp and somewhat wind are more consistent, so I'd rather have the user just be aware of snow games/really rainy games and make there own decisions whether to play that game or not. Will test snow and rain variables separately and together in the model and see if it improves accuracy during this year. If it does not improve accuracy, we will avoid creating more noise for the model and leave those variables out. Including wind and temperature though has shown to improve model accuracy (as we'd expect).",
                      style = "font-size: 20px; text-align: center;"),
               p("Tier information: If you disagree with the tiers, that's fine. They are more judgement calls weeks 1-4, but they are still based on points per game (projected first 4 weeks) for an offense and points allowed (projected first 4 weeks).
For example if a team averages >= 27.5 points a game, they are in the very high tier. If they score less than 20 points per game they are in the bottom tier. Everything else is in between that.
For defense, if a team gives up less than 19.5 points per game, they are in the very high tier. If the allow more than 24 points per game, they are in the bottom tier. 
Everything else in between. Both spread and over/under tiers are created that way. 
                     These ranges were chosen after looking at historical points per game and points allowed over the last 5 years, they are somewhat arbitrary.
                     The tiers then use emerging data after week 4, creating a weighted average on their points per game or points allowed between Vegas projected scored points/projected
                     allowed points and actual points they have scored or given up within the first four weeks.
So safe to say, Denver giving up 70 points to the Dolphins in 2023 really can create a nasty outlier on points allowed. 
It's important to catch on to those types of things if possible as it would skew their tier but please do not think more than you have to you.",
                 style = "font-size: 20px; text-align: center;")
             )
      )
    ),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    tags$h2(id="main-title", "Mark's Picks of the Week (vs. Model and Player Prop(s) of the Week)"),
    div(class="centered-tab",  # Wrapper to center the tabBox
        fluidRow(
          tabBox(
            id = "matchup_tabbox",
            tabPanel("Picks We're Playing This Week",
                     tags$h3(id = "title", "Our Picks"),
                     wellPanel(
                       div(class = "big-text",
                           tags$ul(
                             tags$li(our_picks1),
                             tags$li(our_picks2),
                             tags$li(our_picks3),
                             tags$li(our_picks4),
                             tags$li(our_picks5),
                             tags$li(our_picks6),
                             tags$li(our_picks7),
                             tags$li(our_picks8),
                             tags$li(our_picks9),
                             tags$li(our_picks10)
                           )
                       )
                     )
            ),
            tabPanel("Mark's 7 up trends of the week",
                     tags$h3(id = "title", "Who/What's Trending Up"),
                     wellPanel(
                       div(class = "big-text",
                           tags$ul(
                             tags$li(tup1),
                             tags$li(tup2),
                             tags$li(tup3),
                             tags$li(tup4),
                             tags$li(tup5),
                             tags$li(tup6),
                             tags$li(tup7),
                             tags$li(tup8)
                           )
                       )
                     )
            ),
            tabPanel("Mark's 7 down trends of the week",
                     tags$h3(id = "title", "Who/What's Trending Down"),
                     wellPanel(
                       div(class = "big-text",
                           tags$ul(
                             tags$li(tdown1),
                             tags$li(tdown2),
                             tags$li(tdown3),
                             tags$li(tdown4),
                             tags$li(tdown5),
                             tags$li(tdown6),
                             tags$li(tdown7),
                             tags$li(tdown8)
                             
                           )
                       )
                     )
            ),
            tabPanel("Favorite Player Prop(s) of the Week",
                     tags$h3(id = "title", "Player Prop Locks of the Week"),
                     wellPanel(
                       div(class = "big-text",
                           tags$ul(
                             tags$li(player_prop1),
                             tags$li(player_prop2),
                             tags$li(player_prop3),
                             tags$li(player_prop4),
                             tags$li(player_prop5),
                             tags$li(player_prop6)
                             # ,
                             # tags$li(player_prop7),
                             # tags$li(player_prop8)
                             
                           )
                       )
                     )
            ),
            tabPanel("Spread Picks", DTOutput("spread_picks")),
            tabPanel("O/U Picks", DTOutput("ou_picks")),
            width = 24
          ),
          style = "padding: 0;"  # Keeps the tabBox tightly wrapped
        )
    )
    
    
      )
    )
  )
)
  


  



# build the UI
ui <- dashboardPage(header, sidebar, body)
