library(car)
library(dplyr)
library(party)
library(rpart)       # performing regression trees
library(rpart.plot) 
library(caret)
library(neuralnet)
library(forecast)
library(gains)
library(MASS)
library(ggplot2)
library(corrplot)
library(tidyverse)
library(tidyquant)
library(timetk)
library(randomForest)

#which( colnames(raw_data)=="championID" )
raw_data <- read.csv('u60_player_game_info_combined.csv')

#need to get rid of big outliers for duration
outliers <- boxplot(raw_data$gameDuration, plot = TRUE)$out
focus_data <- raw_data[!(raw_data$gameDuration %in% outliers), ]

#No challenge columns are important @eoin
focus_data <- focus_data %>% select(-ends_with('challenge')) 

#focus only on these main games, not tutorials. subset data to only include these
focus_data = focus_data[focus_data$gameMode == 'CLASSIC' || focus_data$gameMode == 'ARAM' || focus_data$gameMode == 'URF' ,]


# convnert the startimestamp to a date time format
focus_data$gameStartTimestamp <- as.POSIXct(focus_data$gameStartTimestamp/1000, origin = "1970-01-01")
focus_data$gameStartTimestamp <- as_datetime(focus_data$gameStartTimestamp )
focus_data$gameStartTimestamp 

# convnert the startimestamp to a date time format
focus_data$gameEndTimestamp <- as.POSIXct(focus_data$gameEndTimestamp/1000, origin = "1970-01-01")
focus_data$gameEndTimestamp <- as_datetime(focus_data$gameEndTimestamp )
focus_data$gameEndTimestamp 







#-------------- Looking at how current games affect engagement over next time intervals ---------#

## adds 5 new columns
## 1. how many seconds a puuid has played after the initial game within ONE HOUR
## 2. how many seconds a puuid has played after the initial game within THREE HOURS
## 3. how many seconds a puuid has played after the initial game within ONE DAY
## 4. how many seconds a puuid has played after the initial game within THREE DAYS
## 5. how many seconds a puuid has played after the initial game within SEVEN DAYS


#the function input needs to have the columns gameStartTimeStamp and puuid
gameEngagementFutures <- function(focus_data) {
  
  days <- focus_data[,c('gameEndTimestamp')]
  puuid_1 <- focus_data[,c('puuid')]
  duration_futures <- focus_data
  
  hour_future_duration <- focus_data %>% group_by(puuid, matchID) %>%
    summarise(nextHourGameDuration = sum(focus_data$gameDuration[days %within% interval(gameEndTimestamp+1,gameEndTimestamp+3600)& puuid_1 == puuid]))
  
  three_hour_future_duration <- focus_data %>% group_by(puuid, matchID) %>%
    summarise(nextThreeHourGameDuration = sum(focus_data$gameDuration[days %within% interval(gameEndTimestamp+1,gameEndTimestamp+3600*3)& puuid_1 == puuid]))
  
  daily_future_duration <- focus_data %>% group_by(puuid, matchID) %>%
    summarise(nextDayGameDuration = sum(focus_data$gameDuration[days %within% interval(gameEndTimestamp+1,gameEndTimestamp+24*3600)& puuid_1 == puuid]))
  
  three_day_future_duration <- focus_data %>% group_by(puuid, matchID) %>%
    summarise(nextThreeDayGameDuration = sum(focus_data$gameDuration[days %within% interval(gameEndTimestamp+1,gameEndTimestamp+24*3600*3)& puuid_1 == puuid]))
  
  weekly_future_duration <- focus_data %>% group_by(puuid, matchID) %>%
    summarise(nextWeekGameDuration = sum(focus_data$gameDuration[days %within% interval(gameEndTimestamp+1,gameEndTimestamp+24*3600*7)& puuid_1 == puuid]))
  
  #combined the data to ensure we have raw data on how long players spent in next week
  merged_data <-  merge(focus_data, hour_future_duration, by = c("puuid", "matchID")) 
  merged_data <-  merge(merged_data, three_hour_future_duration, by = c("puuid", "matchID")) 
  merged_data <-  merge(merged_data, daily_future_duration, by = c("puuid", "matchID"))
  merged_data <-  merge(merged_data, three_day_future_duration, by = c("puuid", "matchID")) 
  merged_data <-  merge(merged_data, weekly_future_duration, by = c("puuid", "matchID"))
  
  return(merged_data)
}


#-----------------end of future duration function -----------#



## call functions and get official data
test_data <-gameEngagementFutures(focus_data)
test_data <-matchWinStreak(test_data)

test_data_locked <- test_data

#--------------- analysis on the effect of streaks and likelihood of people playing more-------#


## look at distributions and effects on future game duration played
endedLosingStreak <- test_data_locked[test_data_locked$ended_losing_streak == 1,]
endedWinningStreak <- test_data_locked[test_data_locked$ended_winning_streak == 1,]
#neutralStreak <- test_data_locked[test_data_locked$ended_winning_streak == 0 && 
#                                  test_data_locked$ended_losing_streak==0,]

els_density_hour <- density(endedLosingStreak$nextHourGameDuration)
ews_density_hour <- density(endedWinningStreak$nextHourGameDuration)
els_density_3hour <- density(endedLosingStreak$nextThreeHourGameDuration)
ews_density_3hour <- density(endedWinningStreak$nextThreeHourGameDuration) 
els_density_day <- density(endedLosingStreak$nextDayGameDuration)
ews_density_day <- density(endedWinningStreak$nextDayGameDuration)
els_density_3day <- density(endedLosingStreak$nextThreeDayGameDuration)
ews_density_3day <- density(endedWinningStreak$nextThreeDayGameDuration) 
els_density_week <- density(endedLosingStreak$nextWeekGameDuration)
ews_density_week <- density(endedWinningStreak$nextWeekGameDuration) 

plot(ews_density_week, col= 'blue') # plots the results
lines(els_density_week,col = "red")
plot(ews_density_3day, col= 'blue') # plots the results
lines(els_density_3day,col = "red")
plot(ews_density_day, col= 'blue') # plots the results
lines(els_density_day,col = "red")
plot(ews_density_3hour,col = "blue")
lines(els_density_3hour,col = "red")
plot(ews_density_hour,col = "blue")
lines(els_density_hour,col = "red")


#No challenge columns are important @eoin
test_data_locked <- test_data_locked %>%
  select(-ends_with("challenge")) 

#focus only on these main games, not tutorials. subset data to only include these
test_data_locked = test_data_locked[test_data_locked$gameMode == 'CLASSIC' || test_data_locked$gameMode == 'ARAM' || test_data_locked$gameMode == 'URF' ,]

test_data_locked$gameMode
urf <- test_data_locked[test_data_locked$gameMode == 'URF',]

max(urf$gameStartTimestamp)
min(urf$gameStartTimestamp)

plot(urf$gameStartTimestamp)
typeof(urf$gameStartTimestamp)

churn_rate


urf <- urf[urf$gameStartTimestamp >= '2022-04-01 16:00:56 UTC',]

min_urf <- min(urf$gameStartTimestamp)
min_urf
max_urf <- max(urf$gameStartTimestamp)
max_urf


daily_retention_rate <- count(distinct(test_data_locked$puuid))



length((unique(test_data_locked$puuid))

weekly_retention_rate <- y

df_retention <- test_data_locked %>%
  group_by(gameMode) %>%
  arrange(gameStartTimestamp) >%>
  mutate()


expand.grid(date1 = unique(test_data_locked$gameStartTimestamp),
            date2 = unique(test_data_locked$gameStartTimestamp)) %>%
  filter(date1 < date2) %>%
  group_by(date1, date2 ) %>%
  do({ids_1 = setdiff(unique(test_data_locked[test_data_locked$gameStartTimestamp == ymd(.$date1),]$puuid),
                      unique(test_data_locked[test_data_locked$gameStartTimestamp < ymd(.$date1),]$puuid))
  N_ids_1 = length(ids_1)
  ids_2 = unique(test_data_locked[test_data_locked$gameStartTimestamp == ymd(.$date2),]$puuid)
  N_ids_2 = length(inntersect(ids_2, ids_1))
  data.frame(Prc = N_ids_2/N_ids_1)
  }) %>% ungroup()











