## lets goooooo, engagement on users

install.packages('car')
install.packages('randomForest')
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

winstreak_random_puuid_test[, c('puuid','matchID','gameStartTimestamp', 'win',
                                'winStreak', 'lossStreak',
                                'ended_winning_streak','ended_losing_streak')]

# --------------- end of match win streak function and test cases ------------#


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



## look at distributions and effects on future game duration played and the numeric value

plot(test_data_locked$nextDayGameDuration, test_data_locked$winStreak)
cor(test_data_locked$nextDayGameDuration^2, test_data_locked$winStreak)

winStreakData <-  test_data_locked[test_data_locked$winStreak>0,]
lossStreakData <-  test_data_locked[test_data_locked$lossStreak>0,]

# hour plot and corr
plot( winStreakData$winStreak,winStreakData$nextHourGameDuration)
cor(winStreakData$nextHourGameDuration, winStreakData$winStreak)
plot(lossStreakData$nextHourGameDuration, lossStreakData$lossStreak)
cor(lossStreakData$nextHourGameDuration, lossStreakData$lossStreak)

#three hour
cor(winStreakData$nextThreeHourGameDuration, winStreakData$winStreak)
cor(lossStreakData$nextThreeHourGameDuration, lossStreakData$lossStreak)
cor(test_data_locked$nextThreeHourGameDuration, test_data_locked$lossStreak)


#day
cor(winStreakData$nextDayGameDuration, winStreakData$winStreak)
cor(lossStreakData$nextDayGameDuration, lossStreakData$lossStreak)
cor(test_data_locked$nextDayGameDuration, test_data_locked$lossStreak)
cor(test_data_locked$nextDayGameDuration, test_data_locked$winStreak)


#3 day
cor(winStreakData$nextThreeDayGameDuration, winStreakData$winStreak)
cor(lossStreakData$nextThreeDayGameDuration, lossStreakData$lossStreak)
cor(test_data_locked$nextThreeDayGameDuration, test_data_locked$lossStreak)
cor(test_data_locked$nextThreeDayGameDuration, test_data_locked$winStreak)

#week
cor(winStreakData$nextWeekGameDuration, winStreakData$winStreak)
cor(lossStreakData$nextWeekGameDuration, lossStreakData$lossStreak)
cor(test_data_locked$nextWeekGameDuration, test_data_locked$lossStreak)

test_data[, c('gameDuration', 'gameStartTimestamp', 'nextDayGameDuration',
              'nextThreeDayGameDuration')]

colnames(test_data_locked)

mean(test_data_locked$nextDayGameDuration)
median(test_data_locked$nextDayGameDuration)






#-----------------Regressions------------#

## regression model of the next week and day game duration played per player
heyo <- lm( nextDayGameDuration~ winStreak + lossStreak + ended_winning_streak +
              ended_losing_streak+ kills+ assists +deaths
            ,data = test_data_locked )
summary(heyo)

heyo <- lm( nextWeekGameDuration~ winStreak + lossStreak + ended_winning_streak +
              ended_losing_streak+ kills+ assists +deaths + win
            ,data = test_data_locked )
summary(heyo)




colnames(test_data_locked)
subsetted_correlation <- test_data_locked[, c('nextWeekGameDuration','nextDayGameDuration', 'nextHourGameDuration','winStreak' , 'lossStreak' , 'ended_winning_streak' ,
                                                'ended_losing_streak', 'kills', 'assists' ,'deaths' ,
                                              'totalDamageDealt', 'goldEarned', 'goldSpent', 'largestKillingSpree',
                                              'totalTimeSpentDead')]
corrplot(cor(subsetted_correlation))

cor(subsetted_correlation$totalDamageDealt, subsetted_correlation$nextHourGameDuration)

heyo <- lm( nextHourGameDuration~ winStreak + lossStreak + ended_winning_streak +
              ended_losing_streak+ kills+ assists +deaths + win+totalDamageDealt+
              goldEarned + goldSpent + largestKillingSpree +totalTimeSpentDead+timeCCingOthers
            ,data = test_data_locked )
summary(heyo)
heyo <- glm( nextHourGameDuration~ winStreak + lossStreak + ended_winning_streak +
              ended_losing_streak+ kills+ assists +deaths + win+totalDamageDealt+
              goldEarned + goldSpent + largestKillingSpree +totalTimeSpentDead+timeCCingOthers
            ,data = test_data_locked )
RMSE(heyo$fitted.values, test_data_locked$nextHourGameDuration)
mean(test_data_locked$nextHourGameDuration)
summary(heyo)
vif(heyo)

test_data_locked$timeCCingOthers



## subset into where people in next hour/day/week have played 0 hours and see and key distribution differences with certain variables


noHourPlayedTraits <- test_data_locked[test_data_locked$nextHourGameDuration == 0,]
HourPlayedTraits <- test_data_locked[test_data_locked$nextHourGameDuration > 0,]


noDayPlayedTraits <- test_data_locked[test_data_locked$nextDayGameDuration == 0,]
DayPlayedTraits <- test_data_locked[test_data_locked$nextDayGameDuration > 0,]


hi <-  c('winStreak' , 'lossStreak' , 'ended_winning_streak' ,
  'ended_losing_streak', 'kills', 'assists' ,'deaths' ,'totalDamageDealt',
  'goldEarned' , 'goldSpent' , 'largestKillingSpree' ,'totalTimeSpentDead','timeCCingOthers')
hi1 <- noDayPlayedTraits[, c(12:14, 126:128,130:225,245:253)]
summary(hi1)
hi2 <- DayPlayedTraits[, c(12:14, 126:128,130:225,245:253)]
summary(hi2)


hi1 <- noDayPlayedTraits[,c('winStreak' , 'lossStreak' , 'ended_winning_streak' ,
                            'ended_losing_streak', 'kills', 'assists' ,'deaths' ,'totalDamageDealt',
                            'goldEarned' , 'goldSpent' , 'largestKillingSpree' ,'totalTimeSpentDead','timeCCingOthers','championId')]
hi2 <- DayPlayedTraits[,c('winStreak' , 'lossStreak' , 'ended_winning_streak' ,
                          'ended_losing_streak', 'kills', 'assists' ,'deaths' ,'totalDamageDealt',
                          'goldEarned' , 'goldSpent' , 'largestKillingSpree' ,'totalTimeSpentDead','timeCCingOthers', 'championId')]
colnames(DayPlayedTraits)
hello<-(colMeans(hi1)-colMeans(hi2))/colMeans(hi2)

hello

median_hello <- apply(hi1,2,median)
median_hello2 <- apply(hi2,2,median)

hello<-((median_hello)-(median_hello2))/(median_hello2)

hello







#--------------random forests on next hourGameduration------#
m2 <- rpart(formula =nextHourGameDuration~ winStreak + lossStreak + ended_winning_streak +
              ended_losing_streak+ kills+ assists +deaths + win+totalDamageDealt+
              goldEarned + goldSpent + largestKillingSpree +totalTimeSpentDead+timeCCingOthers,
            data = test_data_locked)
rpart.plot(m2)


data(mtcars)
rf.fit <- randomForest(formula =nextHourGameDuration~ winStreak + lossStreak + ended_winning_streak +
                         ended_losing_streak+ kills+ assists +deaths + win+totalDamageDealt+
                         goldEarned + goldSpent + largestKillingSpree +totalTimeSpentDead+timeCCingOthers,
                       data = test_data_locked,
                       ntree=250,
                       keep.forest=FALSE, importance=TRUE)

RMSE(rf.fit$predicted, test_data_locked$nextHourGameDuration)


# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf.fit))
ImpData$Var.Names <- row.names(ImpData)

sqrt(rf.fit$mse)

varImpPlot()

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

#----------- End of Random Forests ------------#


#-----------Decision Tree--------#

?ctree
tree<-ctree(formula =nextDayGameDuration~ winStreak + lossStreak + ended_winning_streak +
              ended_losing_streak+ kills+ assists +deaths +totalDamageDealt+
              goldEarned + goldSpent + largestKillingSpree +totalTimeSpentDead+timeCCingOthers,
            data = test_data_locked)

plot(tree)

tr<-rpart(formula =nextHourGameDuration~ winStreak + lossStreak + ended_winning_streak +
             ended_losing_streak+ kills+ assists +deaths +totalDamageDealt+
             goldEarned + goldSpent + largestKillingSpree +totalTimeSpentDead+timeCCingOthers,
           data = test_data_locked)
tr$variable.importance
rpart.plot(tr)

#------------- end of decision Trees

