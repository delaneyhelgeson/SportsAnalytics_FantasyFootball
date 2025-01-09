# Second iteratio of webscrape, after website changed designs

getQuarterBackData <- function(year){
  
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

    # Read in data (must be csv... R.I.P.)
    qb_data <- read.csv(paste("QB_",year,"_all_unprocessed.csv", sep=""), col.names = QB_column_names)
    

  #Remove duplicate columns
  qb_data <- qb_data %>%
    select(-c("RushingAtt2","PassTgt2","Fmb2","PC2_2","PassesAtt2","FantPt2"))
  
  
  #Calculate Age
  qb_data <- qb_data %>%
    dplyr::mutate(AgeYear = as.numeric(str_split_fixed(AgeChar, "-", 2)[,1]),
           AgeDay = as.numeric(str_split_fixed(AgeChar, "-", 2)[,2]),
           Age = (AgeYear + AgeDay/365), .before = Team) %>%
    select(-c("AgeYear","AgeDay","AgeChar"))
  
  
  # Fix draft positions. Excel has converted numbers into "dates"
  qb_data <- qb_data %>%
    dplyr::mutate(Draft = str_replace(Draft,"Jan","1"),
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
    dplyr::mutate(DraftRound = min(as.numeric(str_split_fixed(Draft, "-", 2)[,1]),
                            as.numeric(str_split_fixed(Draft, "-", 2)[,2])),
           OverallDraftPick = max(as.numeric(str_split_fixed(Draft, "-", 2)[,1]),
                                  as.numeric(str_split_fixed(Draft, "-", 2)[,2])),
           .before = Draft) %>%
    select(-c("Draft"))
  
  return(qb_data)
}

year <- "2000"

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

  terbwr_data <- read.csv(paste(position,"_",year,"_all_unprocessed.csv", sep=""), col.names = TE_RB_WR_column_names)
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
year <- "2015"
setwd(paste("C:/Users/delan/Downloads/",year,sep=''))
allwrdata <- getTERBWRData(year,"TE")
write.csv(allwrdata, paste("TE_",year,"_All.csv",sep=''))


