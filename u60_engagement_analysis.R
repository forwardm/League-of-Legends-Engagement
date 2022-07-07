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
raw_data <- read.csv('./folder/users/u60/u60_player_game_info_combined.csv')
raw_data


#focus_data <- subset(raw_data, select=c('puuid', 'gameStartTimestamp', 'gameDuration', 'assists', 'kills', 'deaths', 'summonerLevel', 'matchID'))
#focus_data <- subset(raw_data, select=c(2,4,5,10,11,135,161,127,148,182,209))
focus_data <- raw_data

# convnert the startimestamp to a date time format
focus_data$gameStartTimestamp <- as.POSIXct(focus_data$gameStartTimestamp/1000, origin = "1970-01-01")
focus_data$gameStartTimestamp <- as_datetime(focus_data$gameStartTimestamp )
focus_data$gameStartTimestamp 

#check min date:
earliest_game_date <- min(focus_data$gameStartTimestamp)

#find relative week compared to first game approval
focus_data$week_num  = round(difftime(focus_data$gameStartTimestamp, earliest_game_date, units = "weeks"))
focus_data$day_num  = round(difftime(focus_data$gameStartTimestamp, earliest_game_date, units = "days"))




#---------Looking at correlation between KDA and weekly duration-----------#

grouped_data_week <- focus_data %>% group_by(puuid, week_num) %>%
  summarise(weekly_kills = sum(kills), weekly_assists = sum(assists),
            weekly_deaths = sum(deaths), weekly_duration = sum(gameDuration))

grouped_data$kda <- (grouped_data$weekly_kills +grouped_data$weekly_assists) / (grouped_data$weekly_deaths)

#create a kda group and omit nas and infinite numbers
kda_duration <- grouped_data_week[, c('kda',  'weekly_duration')]
kda_duration <- na.omit(kda_duration)
kda_duration <- kda_duration[is.finite(omit_nas$kda) , ]


# find correlation between kda and  weekly durationn
cor(kda_duration)




#-------------- Looking at how current games affect engagement over next time intervals ---------#

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
    summarise(nextThreeDayGameDuration = sum(focus_data$gameDuration[days %within% interval(gameStartTimestamp+1,gameStartTimestamp+24*3600)& puuid_1 == puuid]))
  
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

regression_data %>% filter(puuid == 'HnublqKGARVGUs-E85zk8RjOQbnis_y0ht2LFP7skypFQ3Kb5nEsEGqZo_ZqVWxXUIFGVb8YJwyfsg')

eh <- focus_data

test_data <-gameEngagementFutures(eh)


#which( colnames(raw_data)=="championID" )
regression_data<- test_data[,c(1,2,4,5,6,10,12,136,162,127,148,182,209,212, 245:249)]

#regression_data$championId <- factor(regression_data$championId)

which( colnames(test_data)=="totalTimeSpentDead" )
regression_data_2 <- regression_data[,c(7:11,13,14,18)]
head(regression_data_2)

regression <- lm(nextThreeDayGameDuration ~., data= regression_data_2)



summary(regression)


correlation <- cor(regression_data_2)
corrplot(correlation)


# Create the tree.

m2 <- rpart(
  formula = nextThreeHourGameDuration ~ .,
  data= regression_data,
  method  = "anova"
)


# Plot the tree.
rpart.plot(m2)

#show correlation matrix of the relevant rows
correlation_data <- regression_data[,-c(6)]
correlations <- cor(correlation_data)
corrplot(correlations)




