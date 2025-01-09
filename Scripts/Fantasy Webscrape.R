# Delaney Helgeson
# STAT6341 - Project
# Webscraping
#######################

library(robotstxt)
library(rvest)
library(stringr)
library(plyr)
library(tidyverse)
library(dplyr)




##---------Injury history----------------------------------------------------------


getInjuries <- function(year) {
  injuries_all <- data.frame()
  for(week in 1:17){
    link <- paste("https://www.nfl.com/injuries/league/",year,"/REG",week, sep="")
    injury_temp <- read_html(file.path(link)) %>%
      html_nodes(".nfl-o-injury-report__wrap") %>%
      html_table()
    injury <- injury_temp[[1]]
    
    # Filter out column-name rows
    injury <- injury %>%
      filter(Player != "Player") %>%
      mutate(Year = year,
        Week = week, .before = Player)
    
    injuries_all <- rbind(injuries_all, injury)
    rm(injury_temp)
    rm(injury)
  }
  return(injuries_all)
}

year <- 2008
setwd(paste("C:/Users/delan/Downloads/",year,sep=''))
injuries <- getInjuries(year)
write.csv(injuries, paste("injuries_",year,".csv",sep=''))

# https://www.pro-football-reference.com/players/A/AlleJo02.htm
# https://www.pro-football-reference.com/players/J/JohnTa01.htm


#----------- CHECK OUT INJURY STUFF FOR 2008--------------------------------------------------------


getInjuries <- function(year) {
  injuries_all <- data.frame()
  for(week in 1:17){
    link <- paste("https://www.nfl.com/injuries/league/",year,"/REG",week, sep="")
    injury_temp <- read_html(file.path(link)) %>%
      html_nodes(".nfl-o-injury-report__wrap") %>%
      html_table()
    injury <- injury_temp[[1]]
    if(dim(injury)[1] == 0){}
    else{    
      # Filter out column-name rows
      injury <- injury %>%
        dplyr::filter(Player != "Player") %>%
        dplyr::mutate(Year = year,
                      Week = week, .before = Player)
      
      injuries_all <- rbind(injuries_all, injury)}

    rm(injury_temp)
    rm(injury)
  }
  return(injuries_all)
}
year <- 2000
setwd(paste("C:/Users/delan/Downloads/",year,sep=''))
injuries <- getInjuries(year)
write.csv(injuries, paste("injuries_",year,".csv",sep=''))
#----------------------------------------------------------------------------------------------------


city <- "dal"
## Gamelog data
link <- paste("https://www.pro-football-reference.com/teams/", 
              city,"/2021/gamelog/",sep="")

# Grab data from website
player_logs_temp <- read_html(file.path(link)) %>%
  html_nodes("div div") %>%
  html_table()


#----------------------------------------------------------------------------------------
# Game-By-Game Player Logs


getQuarterBackData <- function(year){
  qb_data <- data.frame()
  
  # Define column names
  QB_column_names <- c("Rank","Player","FantPt","RushingAtt2","PassTgt2","Fmb2","PC2_2","PassesAtt2",
                       "Day","GameNum","Week","Date","AgeChar","Team","HomeOrAway","Opponent","Result",
                       "PassesComp","PassesAtt","PassesInc","CmpPercentage","PassingYds","PassingTD",
                       "InterceptThrown","Pick6","TDPercentage","InterceptPercentage","Rate",
                       "TimesSacked","YdsLostSacsks","SackPercentage","YdsGainPerPassAtt","AdjYdsGainPerPassAtt",
                       "AdjNetYdsGainPerPassAtt","YdsGainPerPassCmp","PassTgt","Receptions","ReceivingYds",
                       "RecYdsPerRecepetion","ReceivingTD","CatchPercentage","RecYdsPerTgt","FantPt2","PPR",
                       "DKPt","FDPt","Fmb","FmbRecovered","YdsFmbRecovered","FmbRecovTD","ForcedFmb",
                       "RushingAtt","RushingYds","RushingsYdsPerAtt","RushingTD","AllTD","XPM","XPA",
                       "XPPercent","FieldGoalsMade","FieldGoalsAtt","FieldGoalPercentage","PC2","Sfty",
                       "AllPts","Position","Draft")
  # Bind all weeks together
  for(week in 1:17){
    print(week)
   # Read in data (must be csv... R.I.P.)
   qb_data_temp <- read.csv(paste("QB_",year,"_",week,".csv", sep=""), col.names = QB_column_names)
   
   # Bind data together
   qb_data <- rbind(qb_data, qb_data_temp)
 }
   #Remove duplicate columns
   qb_data <- qb_data %>%
     select(-c("RushingAtt2","PassTgt2","Fmb2","PC2_2","PassesAtt2","FantPt2"))
   
   
   #Calculate Age
   qb_data <- qb_data %>%
     mutate(AgeYear = as.numeric(str_split_fixed(AgeChar, "-", 2)[,1]),
            AgeDay = as.numeric(str_split_fixed(AgeChar, "-", 2)[,2]),
            Age = (AgeYear + AgeDay/365), .before = Team) %>%
     select(-c("AgeYear","AgeDay","AgeChar"))
   
   
   # Fix draft positions. Excel has converted numbers into "dates"
   qb_data <- qb_data %>%
     mutate(Draft = str_replace(Draft,"Jan","1"),
            Draft = str_replace(Draft,"Feb","2"),
            Draft = str_replace(Draft,"Mar","3"),
            Draft = str_replace(Draft,"Apr","4"),
            Draft = str_replace(Draft,"May","5"),
            Draft = str_replace(Draft,"Jun","6"),
            Draft = str_replace(Draft,"Jul","7"),
            Draft = str_replace(Draft,"Aug","8"),
            Draft = str_replace(Draft,"Sep","9"),
            Draft = str_replace(Draft,"Jan","1"),
            Draft = str_replace(Draft,"Oct","10"),
            Draft = str_replace(Draft,"Nov","11"),
            Draft = str_replace(Draft,"Dec","12"))
   
   # Get Draft Round and Draft Position
   qb_data <- qb_data %>%
     rowwise %>%
     mutate(DraftRound = min(as.numeric(str_split_fixed(Draft, "-", 2)[,1]),
                             as.numeric(str_split_fixed(Draft, "-", 2)[,2])),
            OverallDraftPick = max(as.numeric(str_split_fixed(Draft, "-", 2)[,1]),
                                   as.numeric(str_split_fixed(Draft, "-", 2)[,2])),
            .before = Draft) %>%
     select(-c("Draft"))
   
   return(qb_data)
}

year <- "2015"

setwd("~/")
setwd(paste("C:/Users/delan/Downloads/",year,sep=''))
allqbdata <- getQuarterBackData(year)
write.csv(allqbdata, paste("QB_",year,"_All.csv",sep=''))




# function for Tight Ends, Running Backs, Wide Receivers
# Enter position as second argument as: "TE", "RB", or "WR"
getTERBWRData <- function(year, position = "TE"){
  # Column Names
  terbwr_data <- data.frame()
  TE_RB_WR_column_names <- c("Rank","Player","FantPt","RushingAtt2","PassTgt2","Fmb2","PC2_2",
                             "Day","GameNum","Week","Date","AgeChar","Team","HomeOrAway","Opponent","Result",
                             "PassTgt","Receptions","ReceivingYds",
                             "RecYdsPerRecepetion","ReceivingTD","CatchPercentage","RecYdsPerTgt","FantPt2","PPR",
                             "DKPt","FDPt","Fmb","FmbRecovered","YdsFmbRecovered","FmbRecovTD","ForcedFmb",
                             "RushingAtt","RushingYds","RushingsYdsPerAtt","RushingTD","AllTD","XPM","XPA",
                             "XPPercent","FieldGoalsMade","FieldGoalsAtt","FieldGoalPercentage","PC2","Sfty",
                             "AllPts","Position","Draft")
  for(week in 1:17){
    # Read in data (must be csv... R.I.P.)
    terbwr_data_temp <- read.csv(paste(position,"_",year,"_",week,".csv", sep=""), col.names = TE_RB_WR_column_names)
    
    # Bind data together
    terbwr_data <- rbind(terbwr_data, terbwr_data_temp)
  }
  #Remove duplicate columns
  terbwr_data <- terbwr_data %>%
    select(-c("RushingAtt2","PassTgt2","Fmb2","PC2_2","FantPt2"))
  
  
  #Calculate Age
  terbwr_data <- terbwr_data %>%
    mutate(AgeYear = as.numeric(str_split_fixed(AgeChar, "-", 2)[,1]),
           AgeDay = as.numeric(str_split_fixed(AgeChar, "-", 2)[,2]),
           Age = (AgeYear + AgeDay/365), .before = Team) %>%
    select(-c("AgeYear","AgeDay","AgeChar"))
  
  
  # Fix draft positions. Excel has converted numbers into "dates"
  terbwr_data <- terbwr_data %>%
    mutate(Draft = str_replace(Draft,"Jan","1"),
           Draft = str_replace(Draft,"Feb","2"),
           Draft = str_replace(Draft,"Mar","3"),
           Draft = str_replace(Draft,"Apr","4"),
           Draft = str_replace(Draft,"May","5"),
           Draft = str_replace(Draft,"Jun","6"),
           Draft = str_replace(Draft,"Jul","7"),
           Draft = str_replace(Draft,"Aug","8"),
           Draft = str_replace(Draft,"Sep","9"),
           Draft = str_replace(Draft,"Jan","1"),
           Draft = str_replace(Draft,"Oct","10"),
           Draft = str_replace(Draft,"Nov","11"),
           Draft = str_replace(Draft,"Dec","12"))
  
  # Get Draft Round and Draft Position
  terbwr_data <- terbwr_data %>%
    rowwise %>%
    mutate(DraftRound = min(as.numeric(str_split_fixed(Draft, "-", 2)[,1]),
                            as.numeric(str_split_fixed(Draft, "-", 2)[,2])),
           OverallDraftPick = max(as.numeric(str_split_fixed(Draft, "-", 2)[,1]),
                                  as.numeric(str_split_fixed(Draft, "-", 2)[,2])),
           .before = Draft) %>%
    select(-c("Draft"))
  
  return(terbwr_data)
}

# Grab data for TE/RB/WR
year <- "2018"
setwd(paste("C:/Users/delan/Downloads/",year,sep=''))
allwrdata <- getTERBWRData(year,"TE")
write.csv(allwrdata, paste("TE_",year,"_All.csv",sep=''))





####---------------------- Get Game Logs ------------------------------------------------
# See HW 3 for more details


cities <- c("buf","nwe","nyj","mia","cin","cle","pit","rav","clt","oti","htx","jax",
            "rai","den","kan","sdg","phi","nyg","was","was","was","dal","min","gnb","det","chi",
            "nor","car","tam","atl","crd","ram","sea","sfo")
city_full_names <- c("Buffalo Bills", "New England Patriots", "New York Jets", "Miami Dolphins",
                     "Cincinnati Bengals","Cleveland Browns", "Pittsburgh Steelers", "Baltimore Ravens",
                     "Indianapolis Colts","Tennessee Titans", "Houston Texans", "Jacksonville Jaguars",
                     "Las Vegas Raiders","Denver Broncos", "Kansas City Chiefs", "Los Angeles Chargers",
                     "Philadelphia Eagles","New York Giants",
                     "Washington Football Team","Washington Commanders","Washington Redskins",
                     "Dallas Cowboys", "Minnesota Vikings", "Green Bay Packers", "Detroit Lions", 
                     "Chicago Bears", "New Orleans Saints", "Carolina Panthers", "Tampa Bay Buccaneers",
                     "Atlanta Falcons", "Arizona Cardinals", "Los Angeles Rams", "Seattle Seahawks",
                     "San Francisco 49ers")
#
cities_df <- data.frame(Full_Name = city_full_names, Alias = cities)


# List cities that made the playoffs
# We need to know which teams made the play-offs in order to scrape the data correctly.
play_off_teams_2016 <- c("nwe","mia","pit","dal","gnb","htx","sea","det","rai","kan","atl","nyg")


getTeamLogs <- function(cities, play_off_teams, year){
  # Construct Links
  player_logs_all <- data.frame()
  
  for (city in cities) {
    # Construct Link
    link <- paste("https://www.pro-football-reference.com/teams/", 
                  city,"/",year,"/gamelog/",sep="")
    
    # Grab data from website
    player_logs_temp <- read_html(file.path(link)) %>%
      html_nodes("div table") %>%
      html_table()
    
    
    # Pull team table from list
    player_logs <- player_logs_temp[[1]]
    
    # Fix column names
    colnames(player_logs)[c(9,10,12,13,14,18,22,23,24,25,31)] <- paste(colnames(player_logs)[c(9,10,12,13,14,18,22,23,24,25,31)], player_logs[1,c(9,10,12,13,14,18,22,23,24,25,31)], sep='')
    colnames(player_logs)[-c(9,10,12,13,14,18,22,23,24,25,31)] <- player_logs[1,-c(9,10,12,13,14,18,22,23,24,25,31)]
    colnames(player_logs)[17] <- "YdsLost"
    colnames(player_logs)[5] <- "Outcome"
    colnames(player_logs)[7] <- "HomeOrAway"
    
    # Remove first row with column names, empty rows, and redundant columns
    player_logs <- player_logs[-1, -4]
    
    #add column for team name
    player_logs <- player_logs %>%
      add_column(Team = city, .before = "Week")
    
    # Make columns appropriate data type
    player_logs[,c(9:35)] <- sapply(player_logs[,c(9:35)], as.numeric)
    
    #---------------------------------------------------------------------------------
    # Grab the correct table if depending on whether the team made the playoffs
    if(city %in% play_off_teams){
      opponent_index <- 3
    } else {
      opponent_index <- 2
    }
    
    # Pull Opponent table from list
    player_logs_opp <- player_logs_temp[[opponent_index]]
    
    # Fix column names
    colnames(player_logs_opp)[c(9,10,12,13,14,18,22,23,24,25,31)] <- paste(colnames(player_logs_opp)[c(9,10,12,13,14,18,22,23,24,25,31)], player_logs_opp[1,c(9,10,12,13,14,18,22,23,24,25,31)], sep='')
    colnames(player_logs_opp)[-c(9,10,12,13,14,18,22,23,24,25,31)] <- player_logs_opp[1,-c(9,10,12,13,14,18,22,23,24,25,31)]
    colnames(player_logs_opp)[17] <- "YdsLost"
    colnames(player_logs_opp)[5] <- "Outcome"
    colnames(player_logs_opp)[7] <- "HomeOrAway"
    # Rename Opponent stats
    colnames(player_logs_opp)[c(11:36)] <- paste("opp.", colnames(player_logs_opp)[11:36], sep='')
    
    # Remove first row with column names, empty rows, and redundant columns
    player_logs_opp <- player_logs_opp[-1, -4]
    
    #add column for team name
    player_logs_opp <- player_logs_opp %>%
      add_column(Team = city, .before = "Week")
    
    # Make columns appropriate data type
    player_logs_opp[,c(9:35)] <- sapply(player_logs_opp[,c(9:35)], as.numeric)
    
    # Bind team and opponent data
    player_logs_both <- full_join(player_logs, player_logs_opp)
    
    # Compile all game logs into a single data frame
    player_logs_all <- rbind(player_logs_all, player_logs_both)
    rm(player_logs)
    rm(player_logs_opp)
    rm(player_logs_both)
  }
  # Return list of tables from all teams
  return(player_logs_all)
}

year <- 2016
TeamLogs_2016 <- getTeamLogs(unique(cities), play_off_teams_2016, year)
setwd(paste("C:/Users/delan/Downloads/",year,sep=''))
write.csv(TeamLogs_2016, "TeamLogs_2016.csv")
