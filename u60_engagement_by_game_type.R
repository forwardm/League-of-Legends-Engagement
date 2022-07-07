## lets goooooo, engagement on users

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
print(getwd())

#which( colnames(raw_data)=="championID" )
raw_data <- read.csv('u60_player_game_info_combined.csv')

#need to get rid of big outliers for duration
outliers <- boxplot(raw_data$gameDuration, plot = FALSE)$out
focus_data <- raw_data[!(raw_data$gameDuration %in% outliers), ]


#focus only on these main games, not tutorials. subset data to only include these
focus_data = focus_data[focus_data$gameMode == 'CLASSIC' || focus_data$gameMode == 'ARAM' || focus_data$gameMode == 'URF' ,]
 

# convnert the startimestamp to a date time format
focus_data$gameStartTimestamp <- as.POSIXct(focus_data$gameStartTimestamp/1000, origin = "1970-01-01")
focus_data$gameStartTimestamp <- as_datetime(focus_data$gameStartTimestamp )
focus_data$gameStartTimestamp 






#-------------- Looking at how current games affect engagement over next time intervals ---------#

## adds 5 new columns
## 1. how many seconds a puuid has played after the initial game within ONE HOUR
## 2. how many seconds a puuid has played after the initial game within THREE HOURS
## 3. how many seconds a puuid has played after the initial game within ONE DAY
## 4. how many seconds a puuid has played after the initial game within THREE DAYS
## 5. how many seconds a puuid has played after the initial game within SEVEN DAYS


#the function input needs to have the columns gameStartTimeStamp and puuid
gameEngagementFutures <- function(focus_data) {
  
  days <- focus_data[,c('gameStartTimestamp')]
  puuid_1 <- focus_data[,c('puuid')]
  duration_futures <- focus_data
  
  hour_future_duration <- focus_data %>% group_by(puuid, matchID) %>%
    summarise(nextHourGameDuration = sum(focus_data$gameDuration[days %within% interval(gameStartTimestamp+1,gameStartTimestamp+3600)& puuid_1 == puuid]))
  
  three_hour_future_duration <- focus_data %>% group_by(puuid, matchID) %>%
    summarise(nextThreeHourGameDuration = sum(focus_data$gameDuration[days %within% interval(gameStartTimestamp+1,gameStartTimestamp+3600*3)& puuid_1 == puuid]))
  
  daily_future_duration <- focus_data %>% group_by(puuid, matchID) %>%
    summarise(nextDayGameDuration = sum(focus_data$gameDuration[days %within% interval(gameStartTimestamp+1,gameStartTimestamp+24*3600)& puuid_1 == puuid]))
  
  three_day_future_duration <- focus_data %>% group_by(puuid, matchID) %>%
    summarise(nextThreeDayGameDuration = sum(focus_data$gameDuration[days %within% interval(gameStartTimestamp+1,gameStartTimestamp+24*3600*3)& puuid_1 == puuid]))
  
  weekly_future_duration <- focus_data %>% group_by(puuid, matchID) %>%
    summarise(nextWeekGameDuration = sum(focus_data$gameDuration[days %within% interval(gameStartTimestamp+1,gameStartTimestamp+24*3600*7)& puuid_1 == puuid]))
  
  #combined the data to ensure we have raw data on how long players spent in next week
  merged_data <-  merge(focus_data, hour_future_duration, by = c("puuid", "matchID")) 
  merged_data <-  merge(merged_data, three_hour_future_duration, by = c("puuid", "matchID")) 
  merged_data <-  merge(merged_data, daily_future_duration, by = c("puuid", "matchID"))
  merged_data <-  merge(merged_data, three_day_future_duration, by = c("puuid", "matchID")) 
  merged_data <-  merge(merged_data, weekly_future_duration, by = c("puuid", "matchID"))
  
  return(merged_data)
}


#-----------------end of future duration function -----------#




#--------------------make a function that tells win streaks---------------

## this function takes in a certain data object and creates 4 additional columns
## 1. snap win streaking
## 2. snap losing streak
## 3. length of winning streak
## 4. length of losing streak
##
## ideally these characteristics may have influence on the overall outcome of players 
## future engagement

matchWinStreak <- function(focus_data){
  
  ## sort data by puuid and gamestimestamp ascending
  sorted_data <- focus_data  %>% arrange(puuid, gameStartTimestamp)
  
  ## create new columns with all zeros
  sorted_data$winStreak = 0
  sorted_data$lossStreak = 0
  sorted_data$ended_losing_streak = 0
  sorted_data$ended_winning_streak = 0
   
  #make 4 new columns of zeroes
  #----- code here -------#
  
  #need to change length to nrow
  for (i in 1:nrow(sorted_data)){
    
    
    ## checks if its first row, then its the list starting, unknown what streaks were before
    if(i==1){
      sorted_data[i,c('winStreak')] = 0
      sorted_data[i,c('lossStreak')]  = 0
      sorted_data[i,c('ended_losing_streak')] = 0
      sorted_data[i,c('ended_winning_streak')]  = 0
      #look continue up
      i=i+1
      print('hi')
      
    }
    
    ## checks if same player is previous row.
    if(sorted_data[i,c('puuid')] !=  sorted_data[i-1,c('puuid')]){
      print('mismatch puuid')
      sorted_data[i,c('winStreak')] = 0
      sorted_data[i,c('lossStreak')]  = 0
      sorted_data[i,c('ended_losing_streak')] = 0
      sorted_data[i,c('ended_winning_streak')]  = 0
      i=i+1
    }
    
    else{
      
      #if current match won = win, then increase win streak
  
      if(sorted_data[i,'win']== 'True' &&  sorted_data[i-1,c('winStreak')]  == 0  ){
        #print('in1')
        sorted_data[i,c('winStreak')] = sorted_data[i-1,c('winStreak')] + 1
        sorted_data[i,c('lossStreak')]  = 0
        sorted_data[i,c('ended_losing_streak')] = 1
        sorted_data[i,c('ended_winning_streak')]  = 0
        i=i+1
      }
      sorted_data$win
      if(sorted_data[i,'win']== 'False' &&  sorted_data[i-1,c('winStreak')]  == 0 ){
        #print('in2')
        sorted_data[i,c('winStreak')] = 0
        sorted_data[i,c('lossStreak')]  = sorted_data[i-1,c('lossStreak')] + 1
        sorted_data[i,c('ended_losing_streak')] = 0
        sorted_data[i,c('ended_winning_streak')]  = 0
        i=i+1
        
      }
      
      if(sorted_data[i,'win']== 'True' &&  sorted_data[i-1,c('winStreak')]  > 0 ){
        #print('in3')
        sorted_data[i,c('winStreak')] = sorted_data[i-1,c('winStreak')] + 1
        sorted_data[i,c('lossStreak')]  = 0
        sorted_data[i,c('ended_losing_streak')] = 0
        sorted_data[i,c('ended_winning_streak')]  = 0
        i=i+1
      }
      
      if(sorted_data[i,'win']== 'False' &&  sorted_data[i-1,c('winStreak')]  > 0 ){
        #print('in4')
        sorted_data[i,c('winStreak')] = 0
        sorted_data[i,c('lossStreak')]  = sorted_data[i-1,c('lossStreak')] + 1
        sorted_data[i,c('ended_losing_streak')] = 0
        sorted_data[i,c('ended_winning_streak')]  = 1
        i=i+1
      }
      
        
    }
    
    
    }
  return(sorted_data)
}


## testing cases for if implemented correctly
winstreak_test_input <- focus_data  
head(winstreak_test_input)
  
winstreak_data <-matchWinStreak(winstreak_test_input)

column_puuid <- which( colnames(winstreak_data)=="puuid" )

random_puuid <- winstreak_data[4000,column_puuid]

winstreak_random_puuid_test <- winstreak_data[winstreak_data$puuid ==random_puuid,]

winstreak_random_puuid_test[, c('matchID','gameStartTimestamp', 'win',
                                'winStreak', 'lossStreak',
                                'ended_winning_streak','ended_winning_streak')]

# --------------- end of match win streak function and test cases ------------#





test_data <-gameEngagementFutures(focus_data)
test_data <-matchWinStreak(test_data)

#regresion data and subsetted columns
regression_data<- test_data[,c(5,6,8,12,136,162,165,127,148,182,209,212, 226,245:249)]
regression_data


head(regression_data)

summary(regression_data)

which( colnames(test_data)=="win" )


regression_data %>% 
  group_by( win ) %>% 
  summarise( percent = 100 * n() / nrow( regression_data ) )


#select only these game types and make own df
classic_games = regression_data[regression_data$gameMode == 'CLASSIC',]

aram_games = regression_data[regression_data$gameMode == 'ARAM',]

urf_games = regression_data[regression_data$gameMode == 'URF',]



#density plots of people who play different types of games
#one hour
urf_density <- density(urf_games$nextHourGameDuration) # returns the density data 
classic_density<-density(classic_games$nextHourGameDuration)
aram_density <- density(aram_games$nextHourGameDuration)
plot(urf_density, col= 1) # plots the results
lines(classic_density,col = "blue")
lines(aram_density, col = 'green')

mean(urf_games$nextHourGameDuration)
mean(classic_games$nextHourGameDuration)
mean(aram_games$nextHourGameDuration)

median(urf_games$nextHourGameDuration)
median(classic_games$nextHourGameDuration)
median(aram_games$nextHourGameDuration)

#three hours
urf_density <- density(urf_games$nextThreeHourGameDuration) # returns the density data 
classic_density<-density(classic_games$nextThreeHourGameDuration)
aram_density <- density(aram_games$nextThreeHourGameDuration)
plot(urf_density, col= 1) # plots the results
lines(classic_density,col = "blue")
lines(aram_density, col = 'green')

#day hours
urf_density <- density(urf_games$nextDayGameDuration) # returns the density data 
classic_density<-density(classic_games$nextDayGameDuration)
aram_density <- density(aram_games$nextDayGameDuration)
plot(urf_density, col= 1) # plots the results
lines(classic_density,col = "blue")
lines(aram_density, col = 'green')

#week duration
urf_density <- density(urf_games$nextWeekGameDuration) # returns the density data 
classic_density<-density(classic_games$nextWeekGameDuration)
aram_density <- density(aram_games$nextWeekGameDuration)
plot(urf_density, col= 1) # plots the results
lines(classic_density,col = "blue")
lines(aram_density, col = 'green')

mean(urf_games$nextWeekGameDuration)
mean(classic_games$nextWeekGameDuration)
mean(aram_games$nextWeekGameDuration)

median(urf_games$nextWeekGameDuration)
median(classic_games$nextWeekGameDuration)
median(aram_games$nextWeekGameDuration)




#running regressions on gametype and general

win_data <- regression_data[,c(3:7,9,11:length(regression_data))]


#next hour
wayo <- lm(nextHourGameDuration ~  assists + deaths +
             kills + largestKillingSpree + goldSpent + totalHealsOnTeammates +
             totalTimeSpentDead + win +gameMode, data=win_data )

summary(wayo)


#next week
wayo <- lm(nextWeekGameDuration ~  assists + deaths +
             kills + largestKillingSpree + goldSpent + totalHealsOnTeammates +
             totalTimeSpentDead + win +gameMode, data=win_data )

summary(wayo)

cor(win_data$nextHourGameDuration, win_data$totalTimeSpentDead)











