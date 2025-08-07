######SERVER
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$title1 <- renderText({"Grouping Table"})
  output$title2 <- renderText({"Analysis Table"})
  output$title3 <- renderText({"Grouping Table"})
  output$title33 <- renderText({"Grouping Table"})
  output$title4 <- renderText({"Analysis Table"})
  output$title44 <- renderText({"Analysis Table"})
  output$title5 <- renderText({"Grouping Table"})
  output$title6 <- renderText({"Analysis Table"})
  
  output$title101 <- renderText({"Grouping Table"})
  output$title102 <- renderText({"Analysis Table"})
  output$title103 <- renderText({"Grouping Table"})
  output$title1033 <- renderText({"Grouping Table"})
  output$title104 <- renderText({"Analysis Table"})
  output$title1044 <- renderText({"Analysis Table"})
  output$title105 <- renderText({"Grouping Table"})
  output$title106 <- renderText({"Analysis Table"})
  
  
  
  # Calculate hit rates for the new tab
  output$away_hit_rate_output <- renderText({
    paste0("Away Hit Rate: 51.53%"
           #,round(spread_hit_rate$away_hit_rate, 2), "%"
    )
  })
  
  output$home_hit_rate_output <- renderText({
    paste0("Home Hit Rate: 48.47% "
           #,round(spread_hit_rate$home_hit_rate, 2), "%"
    )
  })
  
  output$under_hit_rate_output <- renderText({
    paste0("Under Hit Rate: 54.92% "
           # , round(OU_hit_rate$under_hit_rate, 2), "%"
    )
  })
  
  output$over_hit_rate_output <- renderText({
    paste0("Over Hit Rate: 45.08% "
           # , round(OU_hit_rate$over_hit_rate, 2), "%"
    )
  })
  
  # Blurb output based on the selected number
  output$ou_blurb_output <- renderText({
    if (input$ou_number == 37.5) {
      "We love playing under 37.5 because of common scores such as 20-17, 23-14."
    } else if (input$ou_number == 38.5) {
      "We love playing under 38.5 because of common scores such as 21-17, 24-14, 20-17, 23-14."
    } else if (input$ou_number == 41.5) {
      "We love playing under 41.5 because of common scores such as 21-20, 23-17, 24-17."
      
    } else if (input$ou_number == 44.5) {
      "We love playing under 44.5 because of common scores such as 24-20, 23-20, 27-17."
    } else if (input$ou_number == 47.5) {
      "We love playing under 47.5 because of common scores such as 27-20, 24-23."
    } else if (input$ou_number == 48.5) {
      "We love playing under 48.5 because of common scores such as 24-23, 28-20, 27-21."
    } else if (input$ou_number == 51.5) {
      "We love playing under 51.5 because of common scores such as 27-24, 28-23, 31-20, 28-21."
    } else {
      "No specific strategy for this number."
    }
  })
  
  # Blurb output based on the selected number
  output$ou_blurb_output1 <- renderText({
    if (input$ou_number1 == 37) {
      "We like this number, same reasons as 37.5 but don't love it because of push potential."
    } else if (input$ou_number1 == 38) {
      "We like this number, same reasons as 38.5 but don't love it because of push potential."
    } else if (input$ou_number1 == 40.5) {
      "We like playing under 40.5 because of common scores like 23-17, 24-16."
    } else if (input$ou_number1 == 41) {
      "We like this number, same reasons as 41.5 but don't love it because of push potential."
      
    } else if (input$ou_number1 == 44) {
      "We like this number, same reasons as 44.5 but don't love it because of push potential."
    } else if (input$ou_number1 == 45.5) {
      "We like playing under 45.5 because of common scores like 24-21."
    } else if (input$ou_number1 == 47) {
      "We like this number, same reasons as 47.5 but don't love it because of push potential."
    } else if (input$ou_number1 == 48) {
      "We like this number, same reasons as 48.5 but don't love it because of push potential."
    } else if (input$ou_number1 == 51) {
      "We like this number, same reasons as 51.5 but don't love it because of push potential."
    } else if (input$ou_number1 == 54.5) {
      "We like playing under 54.5 because of common scores like 30-24, 31-23, 28-24, 27-24."
    } else {
      "No specific strategy for this number."
    }
  })
  
  
  
  # Blurb output based on the selected number
  output$spread_blurb_output <- renderText({
    if (input$spread_number == '+3.5') {
      "We love playing +3.5 because a team can win outright or lose by a field goal and we win our bet, it's a key number in what should be a close game."
    } else if (input$spread_number == '+7.5') {
      "We love playing +7.5 because a team can win outright or lose by a touchdown and we win our bet."
    } else if (input$spread_number == '+10.5') {
      "We love playing +10.5 because a team can win outright or lose by a touchdown and a field goal and we win our bet."
      
    } else if (input$spread_number == '-2.5') {
      "We love playing -2.5 because the favored team can win by a field goal and we win our bet, it's a key number in what should be a close game."
    } else if (input$spread_number == '-6.5') {
      "We love playing -6.5 because the favored team needs to win by a touchdown and we win our bet."
    } else {
      "No specific strategy for this number."
    }
  })
  
  # Blurb output based on the selected number
  output$spread_blurb_output1 <- renderText({
    if (input$spread_number1 == '+3') {
      "We like playing +3 because a team can win or lose by less than a field goal and we win our bet or they lose by a field goal and we push our bet."
    } else if (input$spread_number1 == '+4') {
      "We like playing +7 because a team can win outright or lose by less than 4 points which is a key number and we win our bet, also a key number that gets a lot of pushes."
    } else if (input$spread_number1 == '+4.5') {
      "We like it for the same reasons as +4 but now we win our bet instead of push if the team we bet on +4.5 loses by 4 points."
    } else if (input$spread_number1 == '+7') {
      "We like playing +7 because a team can win outright or lose by less than a touchdown and we win our bet or they lose by a touchdown and we push our bet."
    } else if (input$spread_number1 == '+8') {
      "We like playing +7 because a team can win outright or lose by less than a one score game (touchdown + 2 point conversion) and we win our bet or they lose by a one score game (touchdown + 2 point conversion) and we push our bet."
    } else if (input$spread_number1 == '+8.5') {
      "We like it for the same reasons as +8 but now we win our bet instead of push if the team we bet on +8.5 loses by 8 points."
    } else if (input$spread_number1 == '+10') {
      "We like playing +10.5 because a team can lose by less than a touchdown and a field goal and we win our bet, or if they lose by a touchdown and a field goal we win our bet."
    } else if (input$spread_number1 == '-3') {
      "We like playing -3 because the favored team can win by more than a field goal and we win our bet, and if they just win by a field goal we push our bet."
    } else if (input$spread_number1 == '-9.5') {
      "We like playing -9.5 because a largely favored team needs to win by a touchdown and a field goal and we win our bet."
    } else {
      "No specific strategy for this number."
    }
  })
  
  # Example data frame for Round Robin EV
  expected_values <- reactive({
    total_legs <- as.numeric(input$total_legs)
    min_winning_legs <- as.numeric(input$win_legs)
    total_bet <- as.numeric(input$weekly_bet)
    p_win <- input$win_prob / 100
    decimal_odds <- as.numeric(input$odds)  # directly using the value associated with the choice
    
    
    combinations <- choose(total_legs, min_winning_legs)
    bet_per_group <- total_bet / combinations
    
    outcomes <- 0:total_legs
    probabilities <- dbinom(outcomes, total_legs, p_win)
    
    winnings <- sapply(outcomes, function(wins) {
      if (wins >= min_winning_legs) {
        winning_combinations <- choose(wins, min_winning_legs)
        winnings_per_combination <- bet_per_group * (decimal_odds^min_winning_legs)
        total_winnings <- winnings_per_combination * winning_combinations
        return(total_winnings)
      } else {
        return(-total_bet)  # Ensuring that losses are capped at total bet
      }
    })
    
    profits <- ifelse(outcomes < min_winning_legs, -total_bet, winnings - total_bet)
    expected_values <- profits * probabilities
    
    # Create the data frame
    df <- data.frame(
      Win_Outcomes = outcomes,
      Probabilities = probabilities,
      Amount_Risked = rep(total_bet, length(outcomes)),
      Winnings = winnings,
      Profits = profits,
      Expected_Value = expected_values
    )
    
    # Add total row
    total_row <- data.frame(
      Win_Outcomes = "Total",
      Probabilities = sum(probabilities),
      Amount_Risked = "",
      Winnings = "",
      Profits = "",
      Expected_Value = sum(expected_values)
    )
    
    rbind(df, total_row)  # Append the total row
  })
  
  output$round_robin_table <- renderDT({
    datatable(expected_values(), 
              options = list(pageLength = 12, scrollX = TRUE),
              rownames = FALSE) %>% 
      formatCurrency(c('Winnings', 'Profits', 'Expected_Value', 'Amount_Risked'), currency = "$", digits = 2) %>%
      formatPercentage('Probabilities', digits = 2)
  })
  
  
  
  # expected_values <- reactive({
  #   total_legs <- as.numeric(input$total_legs)  
#   min_winning_legs <- as.numeric(input$win_legs)
#   total_bet <- as.numeric(input$weekly_bet)
#   p_win <- input$win_prob / 100
#   decimal_odds <- as.numeric(input$odds)  # Directly using the decimal odds value provided by the UI
#   
#   combinations <- choose(total_legs, min_winning_legs)
#   bet_per_group <- total_bet / combinations
#   
#   outcomes <- 0:total_legs
#   probabilities <- dbinom(outcomes, total_legs, p_win)
#   
#   winnings <- sapply(outcomes, function(wins) {
#     if (wins >= min_winning_legs) {
#       winning_combinations <- choose(wins, min_winning_legs)
#       winnings_per_combination <- bet_per_group * (decimal_odds^min_winning_legs)
#       total_winnings <- winnings_per_combination * winning_combinations
#       return(total_winnings)
#     } else {
#       return(-total_bet)  # Ensuring that losses are capped at total bet
#     }
#   })
#   
#   profits <- ifelse(outcomes < min_winning_legs, -total_bet, winnings - total_bet)
#   expected_values <- profits * probabilities
#   
#   # Create the data frame
#   df <- data.frame(
#     Win_Outcomes = outcomes,
#     Probabilities = probabilities,
#     Amount_Risked = rep(total_bet, length(outcomes)),
#     Winnings = winnings,
#     Profits = profits,
#     Expected_Value = expected_values
#   )
# 
#   # Add total row
#   total_row <- data.frame(
#     Win_Outcomes = "Total",
#     Probabilities = sum(probabilities),
#     Amount_Risked = "",
#     Winnings = "",
#     Profits = "",
#     Expected_Value = sum(expected_values)
#   )
# 
#   rbind(df, total_row)  # Append the total row
# })
# 
# output$round_robin_table <- renderDT({
#   datatable(expected_values(),
#             options = list(pageLength = 12, scrollX = TRUE),
#             rownames = FALSE) %>%
#     formatCurrency(c('Winnings', 'Profits', 'Expected_Value', 'Amount_Risked'), currency = "$", digits = 2) %>%
#     formatPercentage('Probabilities', digits = 2)
# })
  
  output$spread_picks <- renderDT({
    datatable(
      df_both %>%
        select(away_team, home_team, away_line, home_line, Mark_vs_Model_spread) %>%
        filter(!is.na(Mark_vs_Model_spread)),
      options = list(
        autoWidth = FALSE, 
        scrollX = TRUE, 
        pageLength = 16,
        columnDefs = list(list(className = 'dt-center', targets = '_all')),  # Centering all columns
        rownames = FALSE  # This option removes row names
      )
    )
  })
  
  output$ou_picks <- renderDT({
    datatable(
      df_both %>%
        select(away_team, home_team, total_line, Mark_vs_Model_ou) %>%
        filter(!is.na(Mark_vs_Model_ou)),
      options = list(
        autoWidth = FALSE, 
        scrollX = TRUE, 
        pageLength = 16,
        columnDefs = list(list(className = 'dt-center', targets = '_all')),  # Centering all columns
        rownames = FALSE  # This option removes row names
      )
    )
  })


  
  ###Input warning messages
  
  ##################
  #####OVVVVVVVVVVVVVVVVVVVVVVVVVVVERRRRRRRRRRRRRRRRRRRRR/UNDER
  observe({
    if(length(input$Result)<1){
      output$num_Result <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                              "WARNING: No Result selected, please select Result." ,
                                              "</b></font>")
      })
    }
    else{
      output$num_Result <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                              "",
                                              "</b></font>")
      })
    }
  })
  
  
  observe({
    if(length(input$Matchup)<1){
      output$num_matchup <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                               "WARNING: No Matchup selected, please select Matchup." ,
                                               "</b></font>")
      })
    }
    else{
      output$num_matchup <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                               "",
                                               "</b></font>")
      })
    }
  })
  
  observe({
    if(length(input$Model)<1){
      output$num_model <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                             "WARNING: No Model selected, please select Model(s)." ,
                                             "</b></font>")
      })
    }
    else{
      output$num_model <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                             "",
                                             "</b></font>")
      })
    }
  })
  
  observe({
    if(length(input$home_team)<1){
      output$num_home_team <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                 "WARNING: No Home Team selected, please select Home Team." ,
                                                 "</b></font>")
      })
    }
    else{
      output$num_home_team <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                 "",
                                                 "</b></font>")
      })
    }
  })
  
  
  
  observe({
    if(length(input$away_team)<1){
      output$num_away_team <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                 "WARNING: No Away Team selected, please select Away Team." ,
                                                 "</b></font>")
      })
    }
    else{
      output$num_away_team <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                 "",
                                                 "</b></font>")
      })
    }
  })
  
  
  observe({
    if(length(input$season)<1){
      output$num_season <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                              "WARNING: No Season selected, please select Season." ,
                                              "</b></font>")
      })
    }
    else{
      output$num_season <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                              "",
                                              "</b></font>")
      })
    }
  })
  
  observe({
    if(length(input$week)<1){
      output$num_week <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                            "WARNING: No Week selected, please select Week." ,
                                            "</b></font>")
      })
    }
    else{
      output$num_week <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                            "",
                                            "</b></font>")
      })
    }
  })
  
  ####Predictive Analytics
  
  observe({
    if(length(input$Model1)<1){
      output$num_model1 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                              "WARNING: No Model selected, please select Model(s)." ,
                                              "</b></font>")
      })
    }
    else{
      output$num_model1 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                              "",
                                              "</b></font>")
      })
    }
  })
  
  
  observe({
    if(length(input$Matchup1)<1){
      output$num_matchup1 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                "WARNING: No Matchup selected, please select Matchup." ,
                                                "</b></font>")
      })
    }
    else{
      output$num_matchup1 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                "",
                                                "</b></font>")
      })
    }
  })
  
  observe({
    if(length(input$home_team1)<1){
      output$num_home_team1 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                  "WARNING: No Home Team selected, please select Home Team." ,
                                                  "</b></font>")
      })
    }
    else{
      output$num_home_team1 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                  "",
                                                  "</b></font>")
      })
    }
  })
  
  
  
  observe({
    if(length(input$away_team1)<1){
      output$num_away_team1 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                  "WARNING: No Away Team selected, please select Away Team." ,
                                                  "</b></font>")
      })
    }
    else{
      output$num_away_team1 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                  "",
                                                  "</b></font>")
      })
    }
  })
  
  
  observe({
    if(length(input$season1)<1){
      output$num_season1 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                               "WARNING: No Season selected, please select Season." ,
                                               "</b></font>")
      })
    }
    else{
      output$num_season1 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                               "",
                                               "</b></font>")
      })
    }
  })
  
  
  ####Predictive Analytics 
  
  observe({
    if(length(input$Model11)<1){
      output$num_model11 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                               "WARNING: No Model selected, please select Model(s)." ,
                                               "</b></font>")
      })
    }
    else{
      output$num_model11 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                               "",
                                               "</b></font>")
      })
    }
  })
  
  
  observe({
    if(length(input$Matchup11)<1){
      output$num_matchup11 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                 "WARNING: No Matchup selected, please select Matchup." ,
                                                 "</b></font>")
      })
    }
    else{
      output$num_matchup11 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                 "",
                                                 "</b></font>")
      })
    }
  })
  
  observe({
    if(length(input$home_team11)<1){
      output$num_home_team11 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                   "WARNING: No Home Team selected, please select Home Team." ,
                                                   "</b></font>")
      })
    }
    else{
      output$num_home_team11 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                   "",
                                                   "</b></font>")
      })
    }
  })
  
  
  
  observe({
    if(length(input$away_team11)<1){
      output$num_away_team11 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                   "WARNING: No Away Team selected, please select Away Team." ,
                                                   "</b></font>")
      })
    }
    else{
      output$num_away_team11 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                   "",
                                                   "</b></font>")
      })
    }
  })
  
  
  observe({
    if(length(input$season11)<1){
      output$num_season11 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                "WARNING: No Season selected, please select Season." ,
                                                "</b></font>")
      })
    }
    else{
      output$num_season11 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                "",
                                                "</b></font>")
      })
    }
  })
  
  
  
  
  ####Tier Analytics
  
  observe({
    if(length(input$Model2)<1){
      output$num_model2 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                              "WARNING: No Model selected, please select Model(s)." ,
                                              "</b></font>")
      })
    }
    else{
      output$num_model2 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                              "",
                                              "</b></font>")
      })
    }
  })
  
  
  
  
  observe({
    if(length(input$home_team2)<1){
      output$num_home_team2 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                  "WARNING: No Home Team selected, please select Home Team." ,
                                                  "</b></font>")
      })
    }
    else{
      output$num_home_team2 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                  "",
                                                  "</b></font>")
      })
    }
  })
  
  
  
  observe({
    if(length(input$away_team2)<1){
      output$num_away_team2 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                  "WARNING: No Away Team selected, please select Away Team." ,
                                                  "</b></font>")
      })
    }
    else{
      output$num_away_team2 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                  "",
                                                  "</b></font>")
      })
    }
  })
  
  
  observe({
    if(length(input$season2)<1){
      output$num_season2 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                               "WARNING: No Season selected, please select Season." ,
                                               "</b></font>")
      })
    }
    else{
      output$num_season2 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                               "",
                                               "</b></font>")
      })
    }
  })
  
  observe({
    if(length(input$week2)<1){
      output$num_week2 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                             "WARNING: No Week selected, please select Week." ,
                                             "</b></font>")
      })
    }
    else{
      output$num_week2 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                             "",
                                             "</b></font>")
      })
    }
  })
  
  
  ########################SPREAD
  
  
  observe({
    if(length(input$Result100)<1){
      output$num_Result100 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                 "WARNING: No Result selected, please select Result." ,
                                                 "</b></font>")
      })
    }
    else{
      output$num_Result100 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                 "",
                                                 "</b></font>")
      })
    }
  })
  
  
  observe({
    if(length(input$Matchup100)<1){
      output$num_matchup100 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                  "WARNING: No Matchup selected, please select Matchup." ,
                                                  "</b></font>")
      })
    }
    else{
      output$num_matchup100 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                  "",
                                                  "</b></font>")
      })
    }
  })
  
  observe({
    if(length(input$Model100)<1){
      output$num_model100 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                "WARNING: No Model selected, please select Model(s)." ,
                                                "</b></font>")
      })
    }
    else{
      output$num_model100 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                "",
                                                "</b></font>")
      })
    }
  })
  
  observe({
    if(length(input$home_team100)<1){
      output$num_home_team100 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                    "WARNING: No Home Team selected, please select Home Team." ,
                                                    "</b></font>")
      })
    }
    else{
      output$num_home_team100 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                    "",
                                                    "</b></font>")
      })
    }
  })
  
  
  
  observe({
    if(length(input$away_team100)<1){
      output$num_away_team100 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                    "WARNING: No Away Team selected, please select Away Team." ,
                                                    "</b></font>")
      })
    }
    else{
      output$num_away_team100 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                    "",
                                                    "</b></font>")
      })
    }
  })
  
  
  observe({
    if(length(input$season100)<1){
      output$num_season100 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                 "WARNING: No Season selected, please select Season." ,
                                                 "</b></font>")
      })
    }
    else{
      output$num_season100 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                 "",
                                                 "</b></font>")
      })
    }
  })
  
  observe({
    if(length(input$week100)<1){
      output$num_week100 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                               "WARNING: No Week selected, please select Week." ,
                                               "</b></font>")
      })
    }
    else{
      output$num_week100 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                               "",
                                               "</b></font>")
      })
    }
  })
  
  ####Predictive Analytics
  
  observe({
    if(length(input$Model101)<1){
      output$num_model101 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                "WARNING: No Model selected, please select Model(s)." ,
                                                "</b></font>")
      })
    }
    else{
      output$num_model101 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                "",
                                                "</b></font>")
      })
    }
  })
  
  
  observe({
    if(length(input$Matchup101)<1){
      output$num_matchup101 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                  "WARNING: No Matchup selected, please select Matchup." ,
                                                  "</b></font>")
      })
    }
    else{
      output$num_matchup101 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                  "",
                                                  "</b></font>")
      })
    }
  })
  
  observe({
    if(length(input$home_team101)<1){
      output$num_home_team101 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                    "WARNING: No Home Team selected, please select Home Team." ,
                                                    "</b></font>")
      })
    }
    else{
      output$num_home_team101 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                    "",
                                                    "</b></font>")
      })
    }
  })
  
  
  
  observe({
    if(length(input$away_team101)<1){
      output$num_away_team101 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                    "WARNING: No Away Team selected, please select Away Team." ,
                                                    "</b></font>")
      })
    }
    else{
      output$num_away_team101 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                    "",
                                                    "</b></font>")
      })
    }
  })
  
  
  observe({
    if(length(input$season101)<1){
      output$num_season101 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                 "WARNING: No Season selected, please select Season." ,
                                                 "</b></font>")
      })
    }
    else{
      output$num_season101 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                 "",
                                                 "</b></font>")
      })
    }
  })
  
  
  ####Predictive Analytics
  
  # observe({
  #   if(length(input$Model1011)<1){
  #     output$num_model1011 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
  #                                               "WARNING: No Model selected, please select Model(s)." ,
  #                                               "</b></font>")
  #     })
  #   }
  #   else{
  #     output$num_model1011 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
  #                                               "",
  #                                               "</b></font>")
  #     })
  #   }
  # })
  
  
  observe({
    if(length(input$Matchup1011)<1){
      output$num_matchup1011 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                   "WARNING: No Matchup selected, please select Matchup." ,
                                                   "</b></font>")
      })
    }
    else{
      output$num_matchup1011 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                   "",
                                                   "</b></font>")
      })
    }
  })
  
  observe({
    if(length(input$home_team1011)<1){
      output$num_home_team1011 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                     "WARNING: No Home Team selected, please select Home Team." ,
                                                     "</b></font>")
      })
    }
    else{
      output$num_home_team1011 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                     "",
                                                     "</b></font>")
      })
    }
  })
  
  
  
  observe({
    if(length(input$away_team1011)<1){
      output$num_away_team1011 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                     "WARNING: No Away Team selected, please select Away Team." ,
                                                     "</b></font>")
      })
    }
    else{
      output$num_away_team1011 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                     "",
                                                     "</b></font>")
      })
    }
  })
  
  
  observe({
    if(length(input$season1011)<1){
      output$num_season1011 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                  "WARNING: No Season selected, please select Season." ,
                                                  "</b></font>")
      })
    }
    else{
      output$num_season1011 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                  "",
                                                  "</b></font>")
      })
    }
  })
  

  
  ####Tier Analytics
  
  
  observe({
    if(length(input$home_team102)<1){
      output$num_home_team102 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                    "WARNING: No Home Team selected, please select Home Team." ,
                                                    "</b></font>")
      })
    }
    else{
      output$num_home_team102 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                    "",
                                                    "</b></font>")
      })
    }
  })
  
  
  
  observe({
    if(length(input$away_team102)<1){
      output$num_away_team102 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                    "WARNING: No Away Team selected, please select Away Team." ,
                                                    "</b></font>")
      })
    }
    else{
      output$num_away_team102 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                    "",
                                                    "</b></font>")
      })
    }
  })
  
  
  observe({
    if(length(input$season102)<1){
      output$num_season102 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                 "WARNING: No Season selected, please select Season." ,
                                                 "</b></font>")
      })
    }
    else{
      output$num_season102 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                                 "",
                                                 "</b></font>")
      })
    }
  })
  
  observe({
    if(length(input$week102)<1){
      output$num_week102 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                               "WARNING: No Week selected, please select Week." ,
                                               "</b></font>")
      })
    }
    else{
      output$num_week102 <-  renderText({paste("<font color=\"#CC5500\"><font size='5'><b>",
                                               "",
                                               "</b></font>")
      })
    }
  })
  ####################################  
  
  ###############################OU
  
  
  
  
  
  summ_table1 <- eventReactive(input$UPDATE1,{
    
    showModal(modalDialog("Summarizing data...",footer=NULL, easyClose = TRUE, size = "m",fade=TRUE,style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45"))
    
    validate(
      need(length(input$Model1)>0,"Model must be selected"),
      need(length(input$home_team1)>0,"Home Team must be selected"),
      need(length(input$away_team1)>0,"Away Team must be selected"),
      need(length(input$season1)>0,"Season must be selected"),
      need(length(input$Matchup1)>0,"Matchup must be selected")
    )
    
    summ_table1 <- Pred_Model_Past %>%
      filter(model %in% input$Model1,
             home_team %in% input$home_team1,
             away_team %in% input$away_team1,
             season %in% input$season1,
             matchup %in% input$Matchup1
      ) %>%
      group_by_at(c("season","week",input$GROUP_BY1)) %>%
      summarise(under_pred=mean(Under_Pred,na.rm = T),over_pred=mean(Over_Pred,na.rm=T),
                total_line=mean(total_line,na.rm=T)) %>% 
      select(season,week,input$GROUP_BY1,under_pred,over_pred,total_line)
    
    
    
    Sys.sleep(1.5)
    removeModal()
    
    summ_table1
    
  })
  
  
  # Output Table 
  output$compare_table1 <- DT::renderDT(server = FALSE,{
    
    
    input$UPDATE1
    isolate(
      DT::datatable(summ_table1(), 
                    rownames = TRUE,
                    extensions = 'Buttons',
                    options = list(
                      pageLength = 50,
                      language = list(),
                      dom = 'Bfrtip',
                      #dom = '<"dt-top-row"<"dt-left"<"dt-buttons"B>><"dt-right"<"dt-search"f>>>t',
                      autoWidth = FALSE,
                      scrollX = TRUE,
                      #scrollY = "400px",
                      #scrollCollapse = TRUE,
                      buttons = c('copy', 'csv', 'excel'),
                      filter = (position = "top")
                    )
                    
      )%>%
        formatPercentage(columns = c("over_pred","under_pred"),
                         digits = 2) %>% 
        formatRound(columns=c("total_line"),digits=2)
    )
    
    
  })
  
  
  summ_table11 <- eventReactive(input$UPDATE11,{
    
    showModal(modalDialog("Summarizing data...",footer=NULL, easyClose = TRUE, size = "m",fade=TRUE,style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45"))
    
    validate(
      # need(length(input$Model11)>0,"Model must be selected"),
      need(length(input$home_team11)>0,"Home Team must be selected"),
      need(length(input$away_team11)>0,"Away Team must be selected"),
      need(length(input$season11)>0,"Season must be selected"),
      need(length(input$Matchup11)>0,"Matchup must be selected")
    )
    
    summ_table11 <- Pred_Model_Past %>%
      filter(
        #model %in% input$Model11,
        home_team %in% input$home_team11,
        away_team %in% input$away_team11,
        season %in% input$season11,
        matchup %in% input$Matchup11
      ) %>%
      select(season, week,away_team,home_team,away_off,away_def,home_off,home_def,wind,temp,model, Under_Pred, Over_Pred, total_line
             #,run_edition
      ) %>%
      pivot_wider(names_from = "model", values_from = c(Under_Pred, Over_Pred)) %>%
      mutate(
        Over_Pred = case_when(
          input$MODEL_COMBO_OU_PRED == "All" ~ (Over_Pred_lr + Over_Pred_rf + Over_Pred_xg) / 3,
          input$MODEL_COMBO_OU_PRED == "LRXG" ~ (Over_Pred_lr + Over_Pred_xg) / 2,
          input$MODEL_COMBO_OU_PRED == "LRRF" ~ (Over_Pred_lr + Over_Pred_rf) / 2,
          input$MODEL_COMBO_OU_PRED == "RFXG" ~ (Over_Pred_rf + Over_Pred_xg) / 2,
          input$MODEL_COMBO_OU_PRED == "LR" ~ (Over_Pred_lr),
          input$MODEL_COMBO_OU_PRED == "RF" ~ (Over_Pred_rf),
          input$MODEL_COMBO_OU_PRED == "XG" ~ (Over_Pred_xg),
          TRUE ~ NA_real_
        ),
        Under_Pred = case_when(
          input$MODEL_COMBO_OU_PRED == "All" ~ (Under_Pred_lr + Under_Pred_rf + Under_Pred_xg) / 3,
          input$MODEL_COMBO_OU_PRED == "LRXG" ~ (Under_Pred_lr + Under_Pred_xg) / 2,
          input$MODEL_COMBO_OU_PRED == "LRRF" ~ (Under_Pred_lr + Under_Pred_rf) / 2,
          input$MODEL_COMBO_OU_PRED == "RFXG" ~ (Under_Pred_rf + Under_Pred_xg) / 2,
          input$MODEL_COMBO_OU_PRED == "LR" ~ (Under_Pred_lr),
          input$MODEL_COMBO_OU_PRED == "RF" ~ (Under_Pred_rf),
          input$MODEL_COMBO_OU_PRED == "XG" ~ (Under_Pred_xg),
          TRUE ~ NA_real_
        )
      ) %>%
      mutate(
        `Mod All Agree` = case_when(
          input$MODEL_COMBO_OU_PRED == "All" ~ ifelse(
            (Over_Pred_lr >= 0.5 & Over_Pred_rf >= 0.5 & Over_Pred_xg >= 0.5) |
              (Under_Pred_lr >= 0.5 & Under_Pred_rf >= 0.5 & Under_Pred_xg >= 0.5),
            "Yes", "No"
          ),
          input$MODEL_COMBO_OU_PRED == "LRXG" ~ ifelse(
            (Over_Pred_lr >= 0.5 & Over_Pred_xg >= 0.5) |
              (Under_Pred_lr >= 0.5 & Under_Pred_xg >= 0.5),
            "Yes", "No"
          ),
          input$MODEL_COMBO_OU_PRED == "LRRF" ~ ifelse(
            (Over_Pred_lr >= 0.5 & Over_Pred_rf >= 0.5) |
              (Under_Pred_lr >= 0.5 & Under_Pred_rf >= 0.5),
            "Yes", "No"
          ),
          input$MODEL_COMBO_OU_PRED == "RFXG" ~ ifelse(
            (Over_Pred_rf >= 0.5 & Over_Pred_xg >= 0.5) |
              (Under_Pred_rf >= 0.5 & Under_Pred_xg >= 0.5),
            "Yes", "No"
          ),
          input$MODEL_COMBO_OU_PRED == "LR" ~ ifelse(
            (Over_Pred_lr >= 0.5) |
              (Under_Pred_lr >= 0.5),
            "Yes", "No"
          ),
          input$MODEL_COMBO_OU_PRED == "RF" ~ ifelse(
            (Over_Pred_rf >= 0.5) |
              (Under_Pred_rf >= 0.5),
            "Yes", "No"
          ),
          input$MODEL_COMBO_OU_PRED == "XG" ~ ifelse(
            (Over_Pred_xg >= 0.5) |
              (Under_Pred_xg >= 0.5),
            "Yes", "No"
          ),
          TRUE ~ "NA"
        ),
        `High Conf` = ifelse(Over_Pred >= 0.57 | Under_Pred >= 0.595, "Yes", "No"),
        `Mod All Agree High Conf`=ifelse(`High Conf`=="Yes"&`Mod All Agree`=="Yes","Yes","No")
      ) %>%
      select(`Mod All Agree`,`High Conf`,`Mod All Agree High Conf`,season, week,away_team,home_team,Under_Pred,Over_Pred,total_line,
             away_off,away_def,home_off,home_def,wind,temp
             
             #,run_edition
      ) %>% 
      arrange(desc(`Mod All Agree High Conf`))
    
    
    
    Sys.sleep(1.5)
    removeModal()
    
    summ_table11
    
  })
  
  
  # Output Table 
  output$compare_table11 <- DT::renderDT(server = FALSE,{
    input$UPDATE11
    isolate(
      DT::datatable(summ_table11(), 
                    rownames = TRUE,
                    extensions = 'Buttons',
                    options = list(
                      pageLength = 50,
                      language = list(),
                      dom = 'Bfrtip',
                      autoWidth = FALSE,
                      scrollX = TRUE,
                      buttons = c('copy', 'csv', 'excel'),
                      filter = list(position = "top")
                    )
      ) %>%
        formatPercentage(columns = c("Over_Pred", "Under_Pred"), digits = 2) %>%
        formatRound(columns = c("total_line"), digits = 1) %>% 
        formatStyle(
          'Mod All Agree High Conf',
          target = 'cell',
          backgroundColor = styleEqual(
            "Yes", "#90EE90"  # Light green for "Yes", no color for "No"
          )
        )
    )
  })
  
  
  
  
  summ_table2 <- eventReactive(input$UPDATE2,{
    
    showModal(modalDialog("Summarizing data...",footer=NULL, easyClose = TRUE, size = "m",fade=TRUE,style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45"))
    
    validate(
      # need(length(input$Model2)>0,"Model must be selected"),
      need(length(input$home_team2)>0,"Home Team must be selected"),
      need(length(input$away_team2)>0,"Away Team must be selected"),
      need(length(input$season2)>0,"Season must be selected"),
      need(length(input$Matchup2)>0,"Matchup must be selected"),
      
    )
    
    OU_join<- Hist_Model_Past %>%
      filter(home_team %in% input$home_team2,
             away_team %in% input$away_team2,
             season %in% input$season2,
             matchup %in% input$Matchup2,
             week %in% input$week2) %>%
      filter(!Result %in% c("Push")) %>%
      mutate(Result_Count=ifelse(Result %in% c("Correct"),1,0))
    
    summ_table2 <- Hist_Model_Past %>%
      filter(home_team %in% input$home_team2,
             away_team %in% input$away_team2,
             season %in% input$season2,
             matchup %in% input$Matchup2,
             week %in% input$week2) %>%
      filter(!Result %in% c("Push")) %>%
      select(season, week, away_team, home_team, model, Under_Pred, Over_Pred, total_line, total) %>%
      pivot_wider(names_from = "model", values_from = c(Under_Pred, Over_Pred)) %>%
      left_join(OU_join) %>% 
      mutate(
        Over_Pred = case_when(
          input$MODEL_COMBO_OU == "All" ~ (Over_Pred_lr + Over_Pred_rf + Over_Pred_xg) / 3,
          input$MODEL_COMBO_OU == "LRXG" ~ (Over_Pred_lr + Over_Pred_xg) / 2,
          input$MODEL_COMBO_OU == "LRRF" ~ (Over_Pred_lr + Over_Pred_rf) / 2,
          input$MODEL_COMBO_OU == "RFXG" ~ (Over_Pred_rf + Over_Pred_xg) / 2,
          input$MODEL_COMBO_OU == "LR" ~ (Over_Pred_lr),
          input$MODEL_COMBO_OU == "RF" ~ (Over_Pred_rf),
          input$MODEL_COMBO_OU == "XG" ~ (Over_Pred_xg),
          TRUE ~ NA_real_
        ),
        Under_Pred = case_when(
          input$MODEL_COMBO_OU == "All" ~ (Under_Pred_lr + Under_Pred_rf + Under_Pred_xg) / 3,
          input$MODEL_COMBO_OU == "LRXG" ~ (Under_Pred_lr + Under_Pred_xg) / 2,
          input$MODEL_COMBO_OU == "LRRF" ~ (Under_Pred_lr + Under_Pred_rf) / 2,
          input$MODEL_COMBO_OU == "RFXG" ~ (Under_Pred_rf + Under_Pred_xg) / 2,
          input$MODEL_COMBO_OU == "LR" ~ (Under_Pred_lr),
          input$MODEL_COMBO_OU == "RF" ~ (Under_Pred_rf),
          input$MODEL_COMBO_OU == "XG" ~ (Under_Pred_xg),
          TRUE ~ NA_real_
        )
      ) %>%
      mutate(
        # Result_Count = ifelse(
        #   (Over_Pred >= 0.5 & total > total_line) | (Under_Pred >= 0.5 & total < total_line),
        #   1, 0
        # ),
        Model_Agree_Count = case_when(
          input$MODEL_COMBO_OU == "All" ~ ifelse(
            (Over_Pred_lr >= 0.5 & Over_Pred_rf >= 0.5 & Over_Pred_xg >= 0.5) |
              (Under_Pred_lr >= 0.5 & Under_Pred_rf >= 0.5 & Under_Pred_xg >= 0.5),
            1, 0
          ),
          input$MODEL_COMBO_OU == "LRXG" ~ ifelse(
            (Over_Pred_lr >= 0.5 & Over_Pred_xg >= 0.5) |
              (Under_Pred_lr >= 0.5 & Under_Pred_xg >= 0.5),
            1, 0
          ),
          input$MODEL_COMBO_OU == "LRRF" ~ ifelse(
            (Over_Pred_lr >= 0.5 & Over_Pred_rf >= 0.5) |
              (Under_Pred_lr >= 0.5 & Under_Pred_rf >= 0.5),
            1, 0
          ),
          input$MODEL_COMBO_OU == "RFXG" ~ ifelse(
            (Over_Pred_rf >= 0.5 & Over_Pred_xg >= 0.5) |
              (Under_Pred_rf >= 0.5 & Under_Pred_xg >= 0.5),
            1, 0
          ),
          input$MODEL_COMBO_OU == "LR" ~ ifelse(
            (Over_Pred_lr >= 0.5) |
              (Under_Pred_lr >= 0.5),
            1,0
          ),
          input$MODEL_COMBO_OU == "RF" ~ ifelse(
            (Over_Pred_rf >= 0.5) |
              (Under_Pred_rf >= 0.5),
            1, 0
          ),
          input$MODEL_COMBO_OU == "XG" ~ ifelse(
            (Over_Pred_xg >= 0.5) |
              (Under_Pred_xg >= 0.5),
            1, 0
          ),
          TRUE ~ 0
        ),
        Model_Agree_Corr = case_when(
          input$MODEL_COMBO_OU == "All" ~ ifelse(
            ((Over_Pred_lr >= 0.5 & Over_Pred_rf >= 0.5 & Over_Pred_xg >= 0.5) |
               (Under_Pred_lr >= 0.5 & Under_Pred_rf >= 0.5 & Under_Pred_xg >= 0.5)) &
              Result_Count == 1,
            1, 0
          ),
          input$MODEL_COMBO_OU == "LRXG" ~ ifelse(
            ((Over_Pred_lr >= 0.5 & Over_Pred_xg >= 0.5) |
               (Under_Pred_lr >= 0.5 & Under_Pred_xg >= 0.5)) &
              Result_Count == 1,
            1, 0
          ),
          input$MODEL_COMBO_OU == "LRRF" ~ ifelse(
            ((Over_Pred_lr >= 0.5 & Over_Pred_rf >= 0.5) |
               (Under_Pred_lr >= 0.5 & Under_Pred_rf >= 0.5)) &
              Result_Count == 1,
            1, 0
          ),
          input$MODEL_COMBO_OU == "RFXG" ~ ifelse(
            ((Over_Pred_rf >= 0.5 & Over_Pred_xg >= 0.5) |
               (Under_Pred_rf >= 0.5 & Under_Pred_xg >= 0.5)) &
              Result_Count == 1,
            1, 0
          ),
          input$MODEL_COMBO_OU == "LR" ~ ifelse(
            ((Over_Pred_lr >= 0.5) |
               (Under_Pred_lr >= 0.5)) &
              Result_Count == 1,
            1, 0
          ),
          input$MODEL_COMBO_OU == "RF" ~ ifelse(
            ((Over_Pred_rf >= 0.5) |
               (Under_Pred_rf >= 0.5)) &
              Result_Count == 1,
            1, 0
          ),
          input$MODEL_COMBO_OU == "XG" ~ ifelse(
            ((Over_Pred_xg >= 0.5) |
               (Under_Pred_xg >= 0.5)) &
              Result_Count == 1,
            1, 0
          ),
          TRUE ~ 0
        ),
        Sixty_Plus_Count = ifelse(Over_Pred >= 0.57 | Under_Pred >= 0.595, 1, 0),
        Sixty_Plus_Corr = ifelse(Sixty_Plus_Count == 1 & Result_Count == 1, 1, 0),
        All_60_Count=ifelse((Over_Pred >= 0.57 | Under_Pred >= 0.595)&Model_Agree_Count==1,1,0),
        All_60_Corr=ifelse(Sixty_Plus_Corr==1&Model_Agree_Corr==1,1,0)
      ) %>%
      group_by_at(c(input$GROUP_BY2)) %>%
      summarise(
        under_pred = mean(Under_Pred, na.rm = TRUE),
        over_pred = mean(Over_Pred, na.rm = TRUE),
        total_line = mean(total_line, na.rm = TRUE),
        total = mean(total, na.rm = TRUE),
        `High Conf Obs` = sum(Sixty_Plus_Count, na.rm = TRUE),
        Sixty_Plus_Corr = sum(Sixty_Plus_Corr, na.rm = TRUE),
        `Mod All Agree Obs`= sum(Model_Agree_Count, na.rm = TRUE),
        Model_Agree_Corr = sum(Model_Agree_Corr, na.rm = TRUE),
        All_60plus_Corr=sum(All_60_Corr,na.rm=T),
        `Mod All Agree High Conf Obs`=sum(All_60_Count,na.rm=T),
        `Corr Obs` = sum(Result_Count, na.rm = TRUE),
        `Total Obs` = n()
      ) %>%
      mutate(
        `win %` = `Corr Obs` / `Total Obs`,
        `Mod All Agree win %` = Model_Agree_Corr / `Mod All Agree Obs`,
        `High Conf win %` = Sixty_Plus_Corr / `High Conf Obs`,
        `Mod All Agree High Conf win %`=All_60plus_Corr/`Mod All Agree High Conf Obs`
      ) %>%
      select(input$GROUP_BY2, under_pred, over_pred, total_line, total,
             `Total Obs`, `Mod All Agree Obs`, `High Conf Obs`,`Mod All Agree High Conf Obs`,
             `win %`, `Mod All Agree win %`,`High Conf win %`,`Mod All Agree High Conf win %`)
    
    
    
    
    Sys.sleep(1.5)
    removeModal()
    
    summ_table2
    
  })
  
  
  # Output Table 
  output$compare_table2 <- DT::renderDT(server = FALSE,{
    
    
    input$UPDATE2
    isolate(
      DT::datatable(summ_table2(), 
                    rownames = TRUE,
                    extensions = 'Buttons',
                    options = list(
                      pageLength = 50,
                      language = list(),
                      dom = 'Bfrtip',
                      #dom = '<"dt-top-row"<"dt-left"<"dt-buttons"B>><"dt-right"<"dt-search"f>>>t',
                      autoWidth = FALSE,
                      scrollX = TRUE,
                      #scrollY = "400px",
                      #scrollCollapse = TRUE,
                      buttons = c('copy', 'csv', 'excel'),
                      filter = (position = "top")
                    )
                    
      )%>%
        formatPercentage(columns = c("over_pred","under_pred","win %", "Mod All Agree win %","High Conf win %","Mod All Agree High Conf win %"),
                         digits = 2) %>% 
        formatRound(columns=c("total","total_line"),digits=1) %>% 
        formatRound(columns=c("Total Obs",
                              #"Correct Obs",
                              "High Conf Obs",
                              #"Sixty_Plus_Correct",
                              "Mod All Agree Obs",
                              "Mod All Agree High Conf Obs"
                              
                              #,
                              #"Model_Agree_Correct"
        ),
        digits=0)
    )
    
    
  })
  
  
  
  ###########################SPREAD
  ####################
  ###########################SPREAD
  
  
  
  summ_table101 <- eventReactive(input$UPDATE101,{
    
    showModal(modalDialog("Summarizing data...",footer=NULL, easyClose = TRUE, size = "m",fade=TRUE,style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45"))
    
    validate(
      need(length(input$Model101)>0,"Model must be selected"),
      need(length(input$home_team101)>0,"Home Team must be selected"),
      need(length(input$away_team101)>0,"Away Team must be selected"),
      need(length(input$season101)>0,"Season must be selected"),
      need(length(input$Matchup101)>0,"Matchup must be selected")
    )
    
    summ_table101 <- Pred_Model_Spread_Past %>%
      filter(model %in% input$Model101,
             home_team %in% input$home_team101,
             away_team %in% input$away_team101,
             season %in% input$season101,
             matchup %in% input$Matchup101
      ) %>%
      group_by_at(c("season","week",input$GROUP_BY101)) %>%
      summarise(away_cover=mean(away_cov_Pred,na.rm=T),home_cover=mean(home_cov_Pred,na.rm = T),
                away_line=mean(spread_line,na.rm=T),home_line=-1*mean(spread_line,na.rm=T)) %>% 
      select(season,week,input$GROUP_BY101,away_cover,home_cover,away_line,home_line)
    
    
    
    Sys.sleep(1.5)
    removeModal()
    
    summ_table101
    
  })
  
  
  # Output Table 
  output$compare_table101 <- DT::renderDT(server = FALSE,{
    
    
    input$UPDATE101
    isolate(
      DT::datatable(summ_table101(), 
                    rownames = TRUE,
                    extensions = 'Buttons',
                    options = list(
                      pageLength = 50,
                      language = list(),
                      dom = 'Bfrtip',
                      #dom = '<"dt-top-row"<"dt-left"<"dt-buttons"B>><"dt-right"<"dt-search"f>>>t',
                      autoWidth = FALSE,
                      scrollX = TRUE,
                      #scrollY = "400px",
                      #scrollCollapse = TRUE,
                      buttons = c('copy', 'csv', 'excel'),
                      filter = (position = "top")
                    )
                    
      )%>%
        formatPercentage(columns = c("away_cover","home_cover"),
                         digits = 2) %>% 
        formatRound(columns=c("away_line","home_line"),digits=2)
    )
    
    
  })
  
  
  
  summ_table1011 <- eventReactive(input$UPDATE1011,{
    
    showModal(modalDialog("Summarizing data...",footer=NULL, easyClose = TRUE, size = "m",fade=TRUE,style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45"))
    
    validate(
      #need(length(input$Model1011)>0,"Model must be selected"),
      need(length(input$home_team1011)>0,"Home Team must be selected"),
      need(length(input$away_team1011)>0,"Away Team must be selected"),
      need(length(input$season1011)>0,"Season must be selected"),
      need(length(input$Matchup1011)>0,"Matchup must be selected")
    )
    
    summ_table1011 <- Pred_Model_Spread_Past %>%
      filter(home_team %in% input$home_team1011,
             away_team %in% input$away_team1011,
             season %in% input$season1011,
             matchup %in% input$Matchup1011
      ) %>%
      rename(away_cover=away_cov_Pred,home_cover=home_cov_Pred) %>% 
      select(season, week,away_team,home_team,away_off,away_def,home_off,home_def,wind,temp,model, home_cover, away_cover, spread_line
             # ,run_edition
      ) %>%
      mutate(away_line=spread_line,home_line=-1*spread_line) %>% 
      select(-spread_line) %>% 
      pivot_wider(names_from = "model", values_from = c(home_cover, away_cover)) %>%
      mutate(
        away_cover = case_when(
          input$MODEL_COMBO_SPREAD_PRED == "All" ~ (away_cover_lr + away_cover_rf + away_cover_xg) / 3,
          input$MODEL_COMBO_SPREAD_PRED == "LRXG" ~ (away_cover_lr + away_cover_xg) / 2,
          input$MODEL_COMBO_SPREAD_PRED == "LRRF" ~ (away_cover_lr + away_cover_rf) / 2,
          input$MODEL_COMBO_SPREAD_PRED == "RFXG" ~ (away_cover_rf + away_cover_xg) / 2,
          input$MODEL_COMBO_SPREAD_PRED == "LR" ~ (away_cover_lr),
          input$MODEL_COMBO_SPREAD_PRED == "RF" ~ (away_cover_rf),
          input$MODEL_COMBO_SPREAD_PRED == "XG" ~ (away_cover_xg),
          TRUE ~ NA_real_
        ),
        home_cover = case_when(
          input$MODEL_COMBO_SPREAD_PRED == "All" ~ (home_cover_lr + home_cover_rf + home_cover_xg) / 3,
          input$MODEL_COMBO_SPREAD_PRED == "LRXG" ~ (home_cover_lr + home_cover_xg) / 2,
          input$MODEL_COMBO_SPREAD_PRED == "LRRF" ~ (home_cover_lr + home_cover_rf) / 2,
          input$MODEL_COMBO_SPREAD_PRED == "RFXG" ~ (home_cover_rf + home_cover_xg) / 2,
          input$MODEL_COMBO_SPREAD_PRED == "LR" ~ (home_cover_lr),
          input$MODEL_COMBO_SPREAD_PRED == "RF" ~ (home_cover_rf),
          input$MODEL_COMBO_SPREAD_PRED == "XG" ~ (home_cover_xg),
          TRUE ~ NA_real_
        )
      ) %>%
      mutate(
        `Mod All Agree` = case_when(
          input$MODEL_COMBO_SPREAD_PRED == "All" ~ ifelse(
            (away_cover_lr >= 0.5 & away_cover_rf >= 0.5 & away_cover_xg >= 0.5) |
              (home_cover_lr >= 0.5 & home_cover_rf >= 0.5 & home_cover_xg >= 0.5),
            "Yes", "No"
          ),
          input$MODEL_COMBO_SPREAD_PRED == "LRXG" ~ ifelse(
            (away_cover_lr >= 0.5 & away_cover_xg >= 0.5) |
              (home_cover_lr >= 0.5 & home_cover_xg >= 0.5),
            "Yes", "No"
          ),
          input$MODEL_COMBO_SPREAD_PRED == "LRRF" ~ ifelse(
            (away_cover_lr >= 0.5 & away_cover_rf >= 0.5) |
              (home_cover_lr >= 0.5 & home_cover_rf >= 0.5),
            "Yes", "No"
          ),
          input$MODEL_COMBO_SPREAD_PRED == "RFXG" ~ ifelse(
            (away_cover_rf >= 0.5 & away_cover_xg >= 0.5) |
              (home_cover_rf >= 0.5 & home_cover_xg >= 0.5),
            "Yes", "No"
          ),
          input$MODEL_COMBO_SPREAD_PRED == "LR" ~ ifelse(
            (away_cover_lr >= 0.5) |
              (home_cover_lr >= 0.5),
            "Yes", "No"
          ),
          input$MODEL_COMBO_SPREAD_PRED == "RF" ~ ifelse(
            (away_cover_rf >= 0.5) |
              (home_cover_rf >= 0.5),
            "Yes", "No"
          ),
          input$MODEL_COMBO_SPREAD_PRED == "XG" ~ ifelse(
            (away_cover_xg >= 0.5) |
              (home_cover_xg >= 0.5),
            "Yes", "No"
          ),
          TRUE ~ "NA"
        ),
        `High Conf` = ifelse(away_cover >= 0.595 | home_cover >= 0.58, "Yes", "No"),
        `Mod All Agree High Conf`=ifelse(`High Conf`=="Yes"&`Mod All Agree`=="Yes","Yes","No")
      ) %>%
      select(`Mod All Agree`,`High Conf`,`Mod All Agree High Conf`,season,week,away_team,home_team,away_cover, home_cover,away_line,home_line,
             away_off,away_def,home_off,home_def,wind,temp
             #,run_edition
      ) %>% 
      arrange(desc(`Mod All Agree High Conf`))
    
    
    
    Sys.sleep(1.5)
    removeModal()
    
    summ_table1011
    
  })
  
  
  # Output Table 
  output$compare_table1011 <- DT::renderDT(server = FALSE,{
    
    
    input$UPDATE1011
    isolate(
      DT::datatable(summ_table1011(), 
                    rownames = TRUE,
                    extensions = 'Buttons',
                    options = list(
                      pageLength = 50,
                      language = list(),
                      dom = 'Bfrtip',
                      #dom = '<"dt-top-row"<"dt-left"<"dt-buttons"B>><"dt-right"<"dt-search"f>>>t',
                      autoWidth = FALSE,
                      scrollX = TRUE,
                      #scrollY = "400px",
                      #scrollCollapse = TRUE,
                      buttons = c('copy', 'csv', 'excel'),
                      filter = (position = "top")
                    )
                    
      )%>%
        formatPercentage(columns = c("away_cover","home_cover"),
                         digits = 2) %>% 
        formatRound(columns=c("away_line","home_line"),digits=2) %>% 
        formatStyle(
          'Mod All Agree High Conf',
          target = 'cell',
          backgroundColor = styleEqual(
            "Yes", "#90EE90"  # Light green for "Yes", no color for "No"
          )
        )
    )
    
    
  })
  
  
  
  summ_table102 <- eventReactive(input$UPDATE102,{
    
    showModal(modalDialog("Summarizing data...",footer=NULL, easyClose = TRUE, size = "m",fade=TRUE,style="color:#0B0B45; background-color: #CC5500; border-color: #0B0B45"))
    
    validate(
      need(length(input$home_team102)>0,"Home Team must be selected"),
      need(length(input$away_team102)>0,"Away Team must be selected"),
      need(length(input$season102)>0,"Season must be selected"),
      need(length(input$Matchup102)>0,"Matchup must be selected"),
    )
    
    
    
    
    
    spread_join<- Hist_Model_Spread_Past %>%
      filter(home_team %in% input$home_team102,
             away_team %in% input$away_team102,
             season %in% input$season102,
             matchup %in% input$Matchup102,
             week %in% input$week102) %>%
      filter(!Result %in% c("Push")) %>%
      mutate(Result_Count=ifelse(Result %in% c("Correct"),1,0))
    
    
    summ_table102 <- Hist_Model_Spread_Past %>%
      filter(home_team %in% input$home_team102,
             away_team %in% input$away_team102,
             season %in% input$season102,
             matchup %in% input$Matchup102,
             week %in% input$week102) %>%
      filter(!Result %in% c("Push")) %>%
      rename(away_cover=away_cov_Pred,home_cover=home_cov_Pred) %>% 
      select(season, week, away_team, home_team, model, home_cover, away_cover, spread_line, result) %>%
      mutate(away_line=spread_line,home_line=-1*spread_line,
             away_result=result,home_result=-1*result) %>% 
      select(-spread_line) %>% 
      pivot_wider(names_from = "model", values_from = c(home_cover, away_cover)) %>%
      left_join(spread_join) %>% 
      mutate(
        away_cover = case_when(
          input$MODEL_COMBO_SPREAD == "All" ~ (away_cover_lr + away_cover_rf + away_cover_xg) / 3,
          input$MODEL_COMBO_SPREAD == "LRXG" ~ (away_cover_lr + away_cover_xg) / 2,
          input$MODEL_COMBO_SPREAD == "LRRF" ~ (away_cover_lr + away_cover_rf) / 2,
          input$MODEL_COMBO_SPREAD == "RFXG" ~ (away_cover_rf + away_cover_xg) / 2,
          input$MODEL_COMBO_SPREAD == "LR" ~ (away_cover_lr) / 1,
          input$MODEL_COMBO_SPREAD == "RF" ~ (away_cover_rf) / 1,
          input$MODEL_COMBO_SPREAD == "XG" ~ (away_cover_xg) / 1,
          TRUE ~ NA_real_
        ),
        home_cover = case_when(
          input$MODEL_COMBO_SPREAD == "All" ~ (home_cover_lr + home_cover_rf + home_cover_xg) / 3,
          input$MODEL_COMBO_SPREAD == "LRXG" ~ (home_cover_lr + home_cover_xg) / 2,
          input$MODEL_COMBO_SPREAD == "LRRF" ~ (home_cover_lr + home_cover_rf) / 2,
          input$MODEL_COMBO_SPREAD == "RFXG" ~ (home_cover_rf + home_cover_xg) / 2,
          input$MODEL_COMBO_SPREAD == "LR" ~ (home_cover_lr) / 1,
          input$MODEL_COMBO_SPREAD == "RF" ~ (home_cover_rf) / 1,
          input$MODEL_COMBO_SPREAD == "XG" ~ (home_cover_xg) / 1,
          TRUE ~ NA_real_
        )
      ) %>%
      mutate(
        Sixty_Plus_Count = ifelse(away_cover >= 0.595 | home_cover >= 0.58, 1, 0),
        Sixty_Plus_Corr= ifelse((away_cover >= 0.595 | home_cover >= 0.58) & Result_Count==1,1,0),
        Model_Agree_Count = case_when(
          input$MODEL_COMBO_SPREAD == "All" ~ ifelse(
            (away_cover_lr >= 0.5 & away_cover_rf >= 0.5 & away_cover_xg >= 0.5) |
              (home_cover_lr >= 0.5 & home_cover_rf >= 0.5 & home_cover_xg >= 0.5),
            1, 0
          ),
          input$MODEL_COMBO_SPREAD == "LRXG" ~ ifelse(
            (away_cover_lr >= 0.5 & away_cover_xg >= 0.5) |
              (home_cover_lr >= 0.5 & home_cover_xg >= 0.5),
            1, 0
          ),
          input$MODEL_COMBO_SPREAD == "LRRF" ~ ifelse(
            (away_cover_lr >= 0.5 & away_cover_rf >= 0.5) |
              (home_cover_lr >= 0.5 & home_cover_rf >= 0.5),
            1, 0
          ),
          input$MODEL_COMBO_SPREAD == "RFXG" ~ ifelse(
            (away_cover_rf >= 0.5 & away_cover_xg >= 0.5) |
              (home_cover_rf >= 0.5 & home_cover_xg >= 0.5),
            1, 0
          ),
          input$MODEL_COMBO_SPREAD == "LR" ~ ifelse(
            (away_cover_lr >= 0.5) |
              (home_cover_lr >= 0.5),
            1, 0
          ),
          input$MODEL_COMBO_SPREAD == "RF" ~ ifelse(
            (away_cover_rf >= 0.5) |
              (home_cover_rf >= 0.5),
            1, 0
          ),
          input$MODEL_COMBO_SPREAD == "XG" ~ ifelse(
            (away_cover_xg >= 0.5) |
              (home_cover_xg >= 0.5),
            1, 0
          ),
          TRUE ~ 0
        ),
        Model_Agree_Corr = case_when(
          input$MODEL_COMBO_SPREAD == "All" ~ ifelse(
            ((away_cover_lr >= 0.5 & away_cover_rf >= 0.5 & away_cover_xg >= 0.5) |
               (home_cover_lr >= 0.5 & home_cover_rf >= 0.5 & home_cover_xg >= 0.5)) &
              Result_Count == 1,
            1, 0
          ),
          input$MODEL_COMBO_SPREAD == "LRXG" ~ ifelse(
            ((away_cover_lr >= 0.5 & away_cover_xg >= 0.5) |
               (home_cover_lr >= 0.5 & home_cover_xg >= 0.5)) &
              Result_Count == 1,
            1, 0
          ),
          input$MODEL_COMBO_SPREAD == "LRRF" ~ ifelse(
            ((away_cover_lr >= 0.5 & away_cover_rf >= 0.5) |
               (home_cover_lr >= 0.5 & home_cover_rf >= 0.5)) &
              Result_Count == 1,
            1, 0
          ),
          input$MODEL_COMBO_SPREAD == "RFXG" ~ ifelse(
            ((away_cover_rf >= 0.5 & away_cover_xg >= 0.5) |
               (home_cover_rf >= 0.5 & home_cover_xg >= 0.5)) &
              Result_Count == 1,
            1, 0
          ),
          input$MODEL_COMBO_SPREAD == "LR" ~ ifelse(
            ((away_cover_lr >= 0.5) |
               (home_cover_lr >= 0.5)) &
              Result_Count == 1,
            1, 0
          ),
          input$MODEL_COMBO_SPREAD == "RF" ~ ifelse(
            ((away_cover_rf >= 0.5) |
               (home_cover_rf >= 0.5)) &
              Result_Count == 1,
            1, 0
          ),
          input$MODEL_COMBO_SPREAD == "XG" ~ ifelse(
            ((away_cover_xg >= 0.5) |
               (home_cover_xg >= 0.5)) &
              Result_Count == 1,
            1, 0
          ),
          TRUE ~ 0
        ),
        All_60_Count=ifelse((away_cover >= 0.595 | home_cover >= 0.58)&Model_Agree_Count==1,1,0),
        All_60_Corr=ifelse(Sixty_Plus_Corr==1&Model_Agree_Corr==1,1,0)
      ) %>%
      group_by_at(c(input$GROUP_BY102)) %>%
      summarise(
        result = mean(result, na.rm = TRUE),
        home_cover = mean(home_cover, na.rm = TRUE),
        away_cover = mean(away_cover, na.rm = TRUE),
        away_line = mean(away_line, na.rm = TRUE),
        home_line = mean(home_line, na.rm = TRUE),
        away_result = mean(away_result, na.rm = TRUE),
        home_result = mean(home_result, na.rm = TRUE),
        `High Conf Obs` = sum(Sixty_Plus_Count, na.rm = TRUE),
        Sixty_Plus_Corr = sum(Sixty_Plus_Corr, na.rm = TRUE),
        `Mod All Agree Obs` = sum(Model_Agree_Count, na.rm = TRUE),
        Model_Agree_Corr = sum(Model_Agree_Corr, na.rm = TRUE),
        All_60plus_Corr=sum(All_60_Corr,na.rm=T),
        `Mod All Agree High Conf Obs`=sum(All_60_Count,na.rm=T),
        `Corr Obs` = sum(Result_Count, na.rm = TRUE),
        `Total Obs` = n()
      ) %>%
      mutate(
        `win %` = `Corr Obs` / `Total Obs`,
        `Mod All Agree win %` = Model_Agree_Corr / `Mod All Agree Obs`,
        `High Conf win %` = Sixty_Plus_Corr / `High Conf Obs`,
        `Mod All Agree High Conf win %`=All_60plus_Corr/`Mod All Agree High Conf Obs`,
        outcome = ifelse(away_result < 0,
                         paste("Away wins by", round(abs(away_result),1)),
                         ifelse(away_result==0,"Tie",
                                paste("Home wins by", round(abs(home_result),1))))
      ) %>% 
      select(input$GROUP_BY102, away_cover, home_cover, away_line, home_line, result, outcome,
             `Total Obs`, `Mod All Agree Obs`, `High Conf Obs`,`Mod All Agree High Conf Obs`,
             `win %`, `Mod All Agree win %`,`High Conf win %`,`Mod All Agree High Conf win %`)
    
    
    
    
    
    
    
    
    
    Sys.sleep(1.5)
    removeModal()
    
    summ_table102
    
  })
  
  
  # Output Table 
  output$compare_table102 <- DT::renderDT(server = FALSE,{
    
    
    input$UPDATE102
    isolate(
      DT::datatable(summ_table102(), 
                    rownames = TRUE,
                    extensions = 'Buttons',
                    options = list(
                      pageLength = 50,
                      language = list(),
                      dom = 'Bfrtip',
                      #dom = '<"dt-top-row"<"dt-left"<"dt-buttons"B>><"dt-right"<"dt-search"f>>>t',
                      autoWidth = FALSE,
                      scrollX = TRUE,
                      #scrollY = "400px",
                      #scrollCollapse = TRUE,
                      buttons = c('copy', 'csv', 'excel'),
                      filter = (position = "top")
                    )
                    
      )%>%
        formatPercentage(columns = c("away_cover", "home_cover", "High Conf win %", "win %", "Mod All Agree win %","Mod All Agree High Conf win %"),
                         digits = 2) %>% 
        formatRound(columns=c("result","away_line","home_line"),digits=1) #%>% 
      # formatRound(columns=c("Total Obs","Model_Agree Obs", "Sixty_Plus Obs"),digits=0)
    )
    
    
  })
  
  
  
}