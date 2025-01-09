#
# STAT 6341: Sports Analytics
# Final Project App
# Delaney Helgeson
#
#

library(shiny)
library(shinydashboard)
library(fontawesome)
library(plotly)
library(dplyr)
library(tidyverse)
library(tidymodels)


setwd("C:/Users/delan/Downloads/STAT6341 Sports Analytics/Fantasy Project/FantasyApp")

# Upload Data sets for Visualizations
## QB data that is pre-processed for modeling
qb_data_modeling <- read_csv("qb_All_ready_for_modeling_k_5_passAttgte5.csv")
qb_data_raw <- read_csv("qb_All_ready_for_dashboard_passAttgte5.csv")

# Upload model for predictions
qb_lm_fit <- readRDS("qb_lm_fit.rds")

# Set plot margins
mrg <- list(l = 50, r = 50,
            b = 50, t = 50,
            pad = 20)


# Define UI for application that draws a histogram
ui <- dashboardPage(skin='blue',
                    dashboardHeader(title = 'NFL Quarterbacks'),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Home", tabName = 'home_tab', icon = icon('home')),
                        menuItem("Explore Quarterback Data", tabName = 'explore_tab', 
                                 icon = icon("football", lib = "font-awesome")),
                        menuItem("Get a Prediction", tabName = 'prediction_tab', 
                                 icon = icon("chart-line", lib = "font-awesome"))
                      )),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'home_tab',
                                fluidRow(box(
                                  title = "Welcome!", status = 'primary',
                                  "The purpose of this dashboard is to explore NFL quarterback performance through the 
                                   lens of fantasy football. Data at the player-game level from 2000-2018 is used
                                   throughout the dashboard for the visualization and prediction components.",
                                   "In the sidebar, click the \"Explore Quarterback Data\" menu item to analyze 
                                   interactive plots. Click the \"Get a Prediction\" menu item to receive a predicted
                                    weekly fantasy score for a player in your line-up.", width = 12)),
                                fluidRow(column(imageOutput(outputId = "nfllogo"), width=6,offset = 2))
                                ),
                        
                        tabItem(tabName = 'explore_tab',
                                # Figure 1
                                box(title = "Figure 1", status = 'primary',
                                  fluidRow(
                                  column(4,
                                         selectInput(inputId = "fig1_year", label = "Select Year",
                                                     choices = c(2000:2018), selected = 2018),
                                         selectInput(inputId = "fig1_attribute", 
                                                     label = "Select a Performance Statistic",
                                                     choices = c("Fantasy Points" = "FantPtRm",
                                                                 "Passes Attempted" = "PassesAttRm",
                                                                 "Passes Completed" = "PassesCompRm",
                                                                 "Completion Percentage" = "CmpPercentageRm",
                                                                 "Yards Gained / Pass Attempt" = "YdsGainPerPassAttRm",
                                                                 "Passing Yards" = "PassingYdsRm"
                                                                 ),
                                                     selected = "Fantasy Points"),
                                         sliderInput(inputId = "fig1_playergamenum",
                                                     label = "Select a Minimum Number of Games Played",
                                                     min = 1, max = 10, step = 1, value = 1),
                                         actionButton(inputId = "fig1_Go", label = "Go", 
                                                      icon("cat", lib = "font-awesome"))),
                                  column(8, plotlyOutput(outputId = "qbfig1"))),
                                  width = 12),
                                
                                # Figure 2
                                box(title = "Figure 2", status = 'primary',
                                  fluidRow(
                                  column(4,
                                         selectInput(inputId = "fig2_attribute",
                                                     label = "Select a Performance Statistic",
                                                     choices = c("Fantasy Points" = "FantPt.Future",
                                                                 "Passes Attempted" = "PassesAtt.Future",
                                                                 "Passes Completed" = "PassesComp.Future",
                                                                 "Completion Percentage" = "CmpPercentage.Future",
                                                                 "Yards Gained / Pass Attempt" = "YdsGainPerPassAtt.Future",
                                                                 "Passing Yards" = "PassingYds.Future"
                                                     ),
                                                     selected = "Fantasy Points"),
                                         actionButton(inputId = "fig2_Go", label = "Go", 
                                                      icon("cat", lib = "font-awesome"))),
                                  column(8, plotlyOutput(outputId = "qbfig2"))
                                ), width = 12),
                                

                                
                                # Figure 3
                                box(title = 'Figure 3', status = 'primary',
                                  fluidRow(
                                  column(4,
                                         selectInput(inputId = "fig3_year", label = "Select Year",
                                                     choices = c(2000:2018), selected = 2018),
                                         selectInput(inputId = "fig3_team1", label = "Choose a team",
                                                     choices = unique(qb_data_raw$Team.Future), 
                                                     selected = 'DAL'),
                                         selectInput(inputId = "fig3_team2", label = "Choose another team",
                                                     choices = unique(qb_data_raw$Team.Future), 
                                                     selected = 'DEN'),
                                         actionButton(inputId = "fig3_Go", label = "Go", 
                                                      icon("cat", lib = "font-awesome"))),
                                  column(8, plotlyOutput(outputId = "qbfig3"))
                                  ), width = 12),
                                
                                # Figure 4
                                box(title = 'Figure 4', status = 'primary',
                                  fluidRow(
                                  column(4,
                                         selectInput(inputId = "fig4_year", label = "Select Year",
                                                     choices = c(2000:2018), selected = 2018),
                                         selectInput(inputId = "fig4_attribute",
                                                     label = "Select a Performance Statistic",
                                                     choices = c("Fantasy Points" = "FantPt.Future",
                                                                 "Passes Attempted" = "PassesAtt.Future",
                                                                 "Passes Completed" = "PassesComp.Future",
                                                                 "Completion Percentage" = "CmpPercentage.Future",
                                                                 "Yards Gained / Pass Attempt" = "YdsGainPerPassAtt.Future",
                                                                 "Passing Yards" = "PassingYds.Future"
                                                     ),
                                                     selected = "Fantasy Points"),
                                         actionButton(inputId = "fig4_Go", label = "Go", 
                                                      icon("cat", lib = "font-awesome"))),
                                  column(8, plotlyOutput(outputId = "qbfig4"))
                                  ), width = 12)
                                
                                ),
                        
                        tabItem(tabName = 'prediction_tab',
                                fluidRow(box(
                                  title = "Weekly Fantasy Point Projections", status = 'primary',
                                  "Upload a csv with information related to your player's performance history
                                  and the upcoming game to receive a predicted fantasy score for this week. 
                                  For more details on the appropriate column structure of your file, 
                                  see the example dataset in the documentation.", width = 12)),
                                
                                fluidRow(box(
                                  title = "Upload your player data", status = 'success',
                                  fileInput(inputId = "user_file", label = "Choose File", accept = c(".csv")),
                                  actionButton(inputId = "Go", label = "Go", icon("refresh")),
                                  width = 6
                                  ), 
                                  valueBoxOutput("projected_fantasy_score_box", width = 6)),
                                fluidRow(
                                  valueBoxOutput("player_name_box", width = 4),
                                  valueBoxOutput("team_name_box", width = 4),
                                  valueBoxOutput("opponent_name_box", width = 4)),
                                box(status = 'primary',
                                  fluidRow(
                                    column(6, plotlyOutput(outputId = "predictionsplot1")),
                                    column(6, plotlyOutput(outputId = "predictionsplot2"))
                                ), width = 12)
                                
                                )
                        
                      )
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # FIGURE 1
  storage_fig1 <- reactiveValues()
  observeEvent(input$fig1_Go, {
    year_fig1 <- input$fig1_year
    playergamenum_fig1 <- input$fig1_playergamenum
    att <- input$fig1_attribute
    fig1_data <- qb_data_modeling %>% 
      filter(Year == year_fig1, PlayerGameNum >= playergamenum_fig1) %>%
      select(c(att, PlayerGameNum, Player, FantPt.Future, Year))
    colnames(fig1_data) <- c("fig1_attribute", "PlayerGameNum", "Player", "FantPt.Future", "Year")
    storage_fig1$fig1_data <- fig1_data
    
    items <- setNames(c("Fantasy Points","Passes Attempted","Passes Completed",
                        "Completion Percentage","Yards Gained / Pass Attempt","Passing Yards"),
                      c("FantPtRm","PassesAttRm","PassesCompRm","CmpPercentageRm",
                        "YdsGainPerPassAttRm","PassingYdsRm"))
    attname <- items[input$fig1_attribute]
    storage_fig1$attname <- attname
    storage_fig1$year_fig1 <- year_fig1
    
    output$qbfig1 <- renderPlotly({
      df <- storage_fig1$fig1_data
      
      plot_ly(df, x = ~fig1_attribute, y = ~FantPt.Future, name = "Fantasy Points",
              hovertemplate = paste('Player: ', df$Player,
                                    '<br>Moving Avg: ', round(df$fig1_attribute,2),
                                    '<br>Upcoming Fant Pt: ', df$FantPt.Future,
                                    '<br>Player Game No.: ', df$PlayerGameNum),
              marker = list(color = 'rgb(22, 140, 23, 0.8)'))  %>%
        layout(title = paste("Upcoming Fantasy Points vs. ", storage_fig1$attname,"\nMoving Average for the ",
                             storage_fig1$year_fig1," Season", sep=''),
               yaxis = list(title = 'Fantasy Points in the Upcoming Game'),
               xaxis = list(title = paste('Moving Average of', storage_fig1$attname, '(k = 5)')),
               margin = mrg)
    })
  })

  
  # FIGURE 2
  storage_fig2 <- reactiveValues()
  observeEvent(input$fig2_Go, {
    att <- input$fig2_attribute
    fig2_data_1 <- qb_data_raw %>% filter(InjuredBodyPart != "N") %>%
      select(c(att, InjuredBodyPart))
    colnames(fig2_data_1) <- c("fig2_attribute", "InjuredBodyPart")
    fig2_data <- fig2_data_1  %>% 
      group_by(InjuredBodyPart) %>%
      summarize(MeanAtt.Future = mean(fig2_attribute),
                count = n())
    items <- setNames(c("Fantasy Points","Passes Attempted","Passes Completed",
                        "Completion Percentage","Yards Gained / Pass Attempt","Passing Yards"),
                      c("FantPt.Future","PassesAtt.Future","PassesComp.Future","CmpPercentage.Future",
                        "YdsGainPerPassAtt.Future","PassingYds.Future"))
    attname <- items[input$fig2_attribute]
    storage_fig2$attname <- attname
    storage_fig2$fig2_data <- fig2_data
    
    output$qbfig2 <- renderPlotly({
      df <- storage_fig2$fig2_data
      
      
      plot_ly(df, y = ~InjuredBodyPart, x = ~MeanAtt.Future, type = 'bar', orientation = 'h',
              name = "Injury",
              hovertemplate = paste('Mean ',attname,': ', round(df$MeanAtt.Future,2),
                                    '<br>Count of Players: ', df$count, sep ='')) %>% 
        layout(title = paste('Mean', attname,'by Body Part Injured for 2000-2018 Seasons'),
               yaxis = list(title = 'Body Part Injured',
                            categoryorder = "total ascending"),
               xaxis = list(title = paste('Mean ', attname,
                                          ' in First Game Following Recovery',sep='')),
               margin = mrg)
    })
  })
  
  
  # FIGURE 3
  storage_fig3 <- reactiveValues()
  observeEvent(input$fig3_Go, {
    fig3_team1 <- input$fig3_team1
    fig3_team2 <- input$fig3_team2
    fig3_year <- input$fig3_year
    fig3_data_1 <- qb_data_raw %>%
      filter(Team.Future %in% c(fig3_team1, fig3_team2) & Year == fig3_year) %>%
      select(c(Team.Future, FantPt.Future, Year))
    storage_fig3$fig3_data <- fig3_data_1
    storage_fig3$fig3_year <- fig3_year
    
    output$qbfig3 <- renderPlotly({
      df <- storage_fig3$fig3_data
      year <- storage_fig3$fig3_year
      
      plot_ly(df, x = ~Team.Future, y = ~FantPt.Future, color = ~Team.Future, type = "box") %>% 
        layout(title = paste('Fantasy Points Scored by Team for the',year,'Season'),
             yaxis = list(title = "Fantasy Points"),
             xaxis = list(title = "Team"), margin = mrg)
    })
  })
  
  # FIGURE 4
  storage_fig4 <- reactiveValues()
  observeEvent(input$fig4_Go, {
    fig4_year <- input$fig4_year
    att <- input$fig4_attribute
    items <- setNames(c("Fantasy Points","Passes Attempted","Passes Completed",
                        "Completion Percentage","Yards Gained / Pass Attempt","Passing Yards"),
                      c("FantPt.Future","PassesAtt.Future","PassesComp.Future","CmpPercentage.Future",
                        "YdsGainPerPassAtt.Future","PassingYds.Future"))
    attname <- items[input$fig4_attribute]
    
    fig4_data_1 <- qb_data_raw %>%
      filter(Year == fig4_year) %>%
      select(c(att, "Team.Future", "TeamScore", "WinLoss"))
    colnames(fig4_data_1) <- c("attribute", "Team.Future", "TeamScore", "WinLoss")
    
    fig4_data <- fig4_data_1 %>%
      group_by(Team.Future) %>%
      summarize(AverageScore = mean(TeamScore),
             TotalWins = sum(WinLoss == "W"),
             AvgAtt = mean(as.numeric(attribute), na.rm=TRUE)) %>%
      mutate(Team.Future = toupper(Team.Future),
             Team.Future = ifelse(Team.Future == "TEN","OTI",Team.Future),
             Team.Future = ifelse(Team.Future == "OAK","RAI",Team.Future),
             Team.Future = ifelse(Team.Future == "LAR","RAM",Team.Future),
             Team.Future = ifelse(Team.Future == "ARI","CRD",Team.Future),
             Team.Future = ifelse(Team.Future == "BAL","RAV",Team.Future),
             Team.Future = ifelse(Team.Future == "IND","CLT",Team.Future),
             Team.Future = ifelse(Team.Future == "HOU","HTX",Team.Future)) %>%
      select(c(Team.Future, AverageScore, TotalWins, AvgAtt))

    storage_fig4$fig4_data <- fig4_data
    storage_fig4$fig4_year <- fig4_year
    storage_fig4$attname <- attname
    
    output$qbfig4 <- renderPlotly({
      df <- storage_fig4$fig4_data
      year <- storage_fig4$fig4_year
      attname <- storage_fig4$attname
      
      sizeref <- 2.0 * max(df$AvgAtt) / (10**2)
      
      plot_ly(df, x = ~TotalWins, y = ~AverageScore, text = ~Team.Future, type='scatter',
                            mode='markers', color = ~AvgAtt, name = 'Team Performance',
                            #colorbar = list(text = "Average Fantasy Points per Player"), 
                            colors = 'Blues',
                            sizes = c(4, 9.6),
                            marker = list(size = ~AvgAtt,
                                          opacity=0.6,
                                          sizemode = 'radius',
                                          sizeref = sizeref
                                          #,colorbar = list(title = list(text = "Average Fantasy Points per Player"))
      ),
      hovertemplate = paste("Team: ",df$Team.Future,
                            "<br>Total Wins: ",df$TotalWins,
                            "<br>Avg Final Score: ",round(df$AverageScore,2),
                            "<br>Avg ",attname,": ",round(df$AvgAtt,2),sep ='')) %>% 
        layout(title = paste('Team Wins, Scores, and',attname,'\nfor the',year,'Season '),
                    xaxis = list(title = "Total Wins"),
                    yaxis = list(title = "Average Final Score"),
                    margin = mrg)

    })
  })
  
  
  
  # PREDICTION
  store_predicted <- reactiveValues()
  
  # Event for user file upload
  observeEvent(input$Go, {
    inFile <- input$user_file
    user_data_ <- read_csv(inFile$datapath)
    store_predicted$Team.Future <- user_data_$Team.Future
    store_predicted$Opponent.Future <- user_data_$Opponent.Future
    store_predicted$Player <- user_data_$Player
    store_predicted$Year <- user_data_$Year
    store_predicted$FuturePlayerGameNum <- user_data_$FuturePlayerGameNum
    store_predicted$GameNum <- user_data_$GameNum
    
    user_data <- user_data_ %>% select(-c("...1","Player","GameNum","Week",
                                          "PlayerGameNum","FuturePlayerGameNum",
                                          "WinLoss","Year","Team.Future","Opponent.Future"))
    
    store_predicted$user_pred <- round(predict(qb_lm_fit, user_data)$.pred, 2)
    
    # Player Name Output Box
    output$player_name_box <- renderValueBox({
      player_print <- store_predicted$Player
      valueBox(
        player_print, "Player Name", icon = icon("person", lib = "font-awesome") 
      )
    })
    # Team Name Output Box
    output$team_name_box <- renderValueBox({
      team_print <- store_predicted$Team.Future
      valueBox(
        team_print, "Team", icon = icon("people-group", lib = "font-awesome"), color = 'green'
      )
    })
    # Opponent Name Output Box
    output$opponent_name_box <- renderValueBox({
      team_print <- store_predicted$Opponent.Future
      valueBox(
        team_print, "Opponent", icon = icon("people-group", lib = "font-awesome"), color = 'red'
      )
    })
    
    # Player Passes Pie chart
    output$predictionsplot1 <- renderPlotly({
      player_name <- store_predicted$Player
      year <- store_predicted$Year
      futureplayergamenum <- store_predicted$FuturePlayerGameNum
      pie_df_1 <- qb_data_raw %>% filter(Player == player_name & Year == year &
                                       FuturePlayerGameNum < futureplayergamenum)
      pie_df_2 <- qb_data_raw %>% filter(Player == player_name & Year < year)
      pie_df_3 <- rbind(pie_df_1, pie_df_2)
      plot_ly(type='pie', labels=c("Total Passes Completed", "Total Passes Incompleted"), 
              values=c(sum(pie_df_3$PassesComp), sum(pie_df_3$PassesInc))) %>%
        layout(title = "Total Passes Attempted Over Career To Date")
    })
    
    # Team Stats Bar Chart
    output$predictionsplot2 <- renderPlotly({
      team_name <- store_predicted$Team.Future
      opponent_name <- store_predicted$Opponent.Future
      year <- store_predicted$Year
      game_num <- store_predicted$GameNum
      bar_df_1 <- qb_data_raw %>% filter(Team.Future == team_name & Year == year &
                                         GameNum <= game_num)
      bar_df_2 <- qb_data_raw %>% filter(Opponent.Future == opponent_name & Year == year &
                                           GameNum <= game_num)
      val1 <- mean(bar_df_1$FantPt)
      val2 <- mean(bar_df_2$FantPt)
      plot_ly(x = c(team_name, paste("Against",opponent_name)), 
             y = c(val1, val2),
             name = "",
             type = 'bar') %>%
        layout(title = "Mean Fantasy Points Scored by QBs YTD",
               xaxis = list(title = "Team"),
               yaxis = list(title = "Mean Fantasy Points Scored"))
    })
  })
  
  # Fantasy Score output Box
  output$projected_fantasy_score_box <- renderValueBox({ 
    if(is.null(store_predicted$user_pred)) {
      user_pred_print <- c(0.0)
    } else{
      user_pred_print <- store_predicted$user_pred
    }
    valueBox(
      user_pred_print, "Projected Fantasy Score", icon = icon("football", lib = "font-awesome"),
      color = "blue"
    )
  })
  

  
  # PICTURE ON WELCOME TAB
  output$nfllogo <- renderImage({
      list(src = "nfllogo.jpg",
           width = 450,
           height = 300)
    }, deleteFile = F)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

