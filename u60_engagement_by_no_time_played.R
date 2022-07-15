
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
raw_data <- read.csv('./folder/users/u60/u60_player_game_info_combined.csv')

#need to get rid of big outliers for duration
outliers <- boxplot(raw_data$gameDuration, plot = FALSE)$out
focus_data <- raw_data[!(raw_data$gameDuration %in% outliers), ]

#No challenge columns are important @eoin
focus_data <- focus_data %>% select(-ends_with('challenge')) 


#focus only on these main games, not tutorials. subset data to only include these
focus_data = focus_data[focus_data$gameMode == 'CLASSIC' | focus_data$gameMode == 'ARAM' | focus_data$gameMode == 'URF' ,]


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




test_data_locked <- test_data %>% dplyr::select(-ends_with('challenge')) 





test_data_locked <- test_data %>% dplyr::select(-c('summonerName',       'role',           'riotIdTagline'     ,
                                                   'riotIdName', 'lane',      'individualPosition'  ,        
                                                   'teamPosition'  ,   'teamEarlySurrendered' , 'summonerId' ,       
                                                   'gameEndedInSurrender'   ,'gameEndedInEarlySurrender', 'firstTowerKill'  ,
                                                   'firstTowerAssist',  'firstBloodAssist',    'eligibleForProgression',
                                                   'firstBloodKill' , 'puuid', 'matchID', 'X', 'gameCreation', 'gameStartTimestamp', 'gameType', 'gameVersion',
                                                   'gameEndTimestamp', 'championName', 'gameMode', 'win'))
ncol(test_data_locked)




#------------------  sub group by play/no play grouping----------#

## subset into where people in next hour/day/week have played 0 hours and see and key distribution differences with certain variables

#no hours played data only want for future hour engagement
noHourPlayedTraits <- test_data_locked[test_data_locked$nextHourGameDuration == 0,] %>%
  dplyr::select(-c('nextThreeHourGameDuration','nextDayGameDuration', 'nextThreeDayGameDuration','nextWeekGameDuration')) 

# hours played data only want for future hour engagement
HourPlayedTraits <- test_data_locked[test_data_locked$nextHourGameDuration > 0,] %>%
  dplyr::select(-c('nextThreeHourGameDuration','nextDayGameDuration', 'nextThreeDayGameDuration','nextWeekGameDuration')) 

#calaculate % difference between the two groups
diff_mean <- (colMeans(noHourPlayedTraits)- colMeans(HourPlayedTraits))/colMeans(HourPlayedTraits)
#only keeps values more than 15%
diff_mean_important <- diff_mean[abs(diff_mean) > 0.15  ]
max(diff_mean_important)
#plot
barplot(diff_mean_important, main= " % Difference of Players who Play & Dont Play in Next Hour ", cex.names = 0.8)



#daily

# no  time played data grou only want for future day engagement
noDayPlayedTraits <- test_data_locked[test_data_locked$nextDayGameDuration == 0,] %>%
  dplyr::select(-c('nextThreeHourGameDuration','nextHourGameDuration', 'nextThreeDayGameDuration','nextWeekGameDuration')) 

#  time played data group only want for future day engagement
dayPlayedTraits <- test_data_locked[test_data_locked$nextDayGameDuration > 0,] %>%
  dplyr::select(-c('nextThreeHourGameDuration','nextHourGameDuration', 'nextThreeDayGameDuration','nextWeekGameDuration')) 

#calculate % diff
diff_mean_day <- (colMeans(noDayPlayedTraits)- colMeans(dayPlayedTraits))/colMeans(dayPlayedTraits)
diff_mean_important_day <- diff_mean_day[abs(diff_mean_day) > 0.15  ]
max(diff_mean_important_day)
#plot
barplot(diff_mean_important_day, main= " % Difference of Players who Play & Dont Play in Next Day ", cex.names = 0.8)
diff_mean_important_day


#weekly
# no  time played data grou only want for future week engagement
noWeekPlayedTraits <- test_data_locked[test_data_locked$nextWeekGameDuration == 0,] %>%
  dplyr::select(-c('nextThreeHourGameDuration','nextDayGameDuration', 'nextThreeDayGameDuration','nextHourGameDuration')) 

#   time played data grou only want for future week engagement
WeekPlayedTraits <- test_data_locked[test_data_locked$nextWeekGameDuration > 0,] %>%
  dplyr::select(-c('nextThreeHourGameDuration','nextHourGameDuration', 'nextThreeDayGameDuration','nextDayGameDuration')) 

diff_mean_Week <- (colMeans(noWeekPlayedTraits)- colMeans(WeekPlayedTraits))/colMeans(WeekPlayedTraits)

#diff_median_Week <- (apply(noWeekPlayedTraits,2,median) - apply(WeekPlayedTraits,2,median))/ apply(WeekPlayedTraits,2,median)
#diff_median_important_Week <- (diff_median_Week[abs(diff_median_Week) > 0.15  ])

#barplot(diff_median_important_Week, main= " % Difference of Players who Play & Dont Play in Next Week ", cex.names = 0.8)

# % diff
diff_mean_Week <- (colMeans(noWeekPlayedTraits)- colMeans(WeekPlayedTraits))/colMeans(WeekPlayedTraits)
diff_mean_important_Week <- diff_mean_Week[abs(diff_mean_Week) > 0.15  ]
max(diff_mean_important_Week)

#plot
barplot(diff_mean_important_Week, main= " % Difference of Players who Play & Dont Play in Next Week ", cex.names = 0.8)



## compare all groupings of time periods
typeof(diff_mean_important)
diff_mean_important_Week
diff_mean_important_day
diff_mean_important

diff_mean_important_all <- rbind(diff_mean_important,diff_mean_important_day, diff_mean_important_Week)
diff_mean_important_all


names(diff_mean_important)


data.frame(diff_mean_important)

#make dataframes for each one, doing data wrangling next couple of lines to make it easy to put into ggplot bar graph
df1 <-  data.frame(TimePeriod = 'diff_mean_important_hour', attribute = names(diff_mean_important), PercentGreater =100*(unname(diff_mean_important)))

df2 <-  data.frame(TimePeriod = 'diff_mean_important_day', attribute = names(diff_mean_important_day), PercentGreater =100*(unname(diff_mean_important_day)))

df3 <-  data.frame(TimePeriod = 'diff_mean_important_Week', attribute = names(diff_mean_important_Week), PercentGreater =100*(unname(diff_mean_important_Week)))



diff_mean_important_all <- data.frame(bind_rows(df1,df2, df3))
diff_mean_important_all <- diff_mean_important_all[!(diff_mean_important_all$attribute == 'nextWeekGameDuration' |
                                                     diff_mean_important_all$attribute == 'nextHourGameDuration'|
                                                     diff_mean_important_all$attribute == 'nextDayGameDuration' ),]

df4 <- cbind(data.frame(diff_mean_important), data.frame(diff_mean_important_day))

## make combined dataframe of previous three
df4 <- data.frame(bind_rows(diff_mean_important,diff_mean_important_day, diff_mean_important_Week))
library(DT)
datatable(t(df4))
datatable(diff_mean_important_all)

##plot a ggplot bar plot.
ggplot(diff_mean_important_all, aes(fill=TimePeriod, x=PercentGreater, y= attribute)) + geom_bar(position = 'dodge', stat = 'identity') +
  labs( title = '% Difference of Means of Players who Played and Did not Play Over Time Period')

#--------- end of grouping and finding means------#
















#-------- looking to find correlation for the two different groups, and run regression model on those who played------#

##two correlations of interest appear
## 1. item 6 and 
## 2. longestTimeSpentLiving

summary(test_data_locked$item6)
summary(test_data_locked$longestTimeSpentLiving)
hist(test_data_locked$item6)
hist(test_data_locked$longestTimeSpentLiving)
summary(test_data_locked$unrealKills)

noDayPlayedTraits <- test_data_locked[test_data_locked$nextDayGameDuration == 0,] %>%
  select(-c('nextThreeDayGameDuration','nextHourGameDuration', 'nextThreeDayGameDuration','nextWeekGameDuration')) 
DayPlayedTraits <- test_data_locked[test_data_locked$nextDayGameDuration > 0,]%>%
  select(-c('nextThreeDayGameDuration','nextHourGameDuration', 'nextThreeDayGameDuration','nextWeekGameDuration')) 

diff <- (colMeans(noDayPlayedTraits)- colMeans(DayPlayedTraits))/((colMeans(noDayPlayedTraits)+colMeans(DayPlayedTraits))/2)
diff
barplot(diff)




hi <-  c('winStreak' , 'lossStreak' , 'ended_winning_streak' ,
         'ended_losing_streak', 'kills', 'assists' ,'deaths' ,'totalDamageDealt',
         'goldEarned' , 'goldSpent' , 'largestKillingSpree' ,'totalTimeSpentDead','timeCCingOthers')


r1 <- lm(nextHourGameDuration ~. ,data= (noHourPlayedTraits %>% select(-c('nextDayGameDuration','nextThreeHourGameDuration',
                                                                          'nextWeekGameDuration', 'nextThreeDayGameDuration'))))
summary(r1)

r2 <- lm(nextHourGameDuration ~. ,data= (HourPlayedTraits %>% select(-c('nextDayGameDuration','nextThreeHourGameDuration',
                                                                        'nextWeekGameDuration', 'nextThreeDayGameDuration'))))
summary(r2)


corrplot(cor(noHourPlayedTraits))


##--------------stepwise regression------------#

#define intercept-only model
intercept_only <- lm(nextHourGameDuration ~ 1, data= (test_data_locked %>%
                                                        select(-c('nextDayGameDuration','nextThreeHourGameDuration',
                                                                  'nextWeekGameDuration', 'nextThreeDayGameDuration'))))

#define model with all predictors
all <- lm(nextHourGameDuration ~., data= (test_data_locked %>%
                                            select(-c('nextDayGameDuration','nextThreeHourGameDuration',
                                                      'nextWeekGameDuration', 'nextThreeDayGameDuration'))))

#perform backward stepwise regression
both <- step(intercept_only, direction='both', scope=formula(all), trace=0)
summary(both)
both$anova
both$coefficients


backward <- step(all, direction='backward', scope=formula(all), trace=0)
summary(backward)
backward$anova
backward$coefficients


forward <- step(all, direction='forward', scope=formula(all), trace=0)
summary(forward)
forward$anova
forward$coefficient


#no resutls
#--------------end of stepwise regresssion---------#




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







#--------log regressions for non zero values------#

## attempting to see if the logs of anything correlated better with future engagement.
##most were horizontal datapoints, appearing to be evenly distributed
## not much inference from doing data transformation.

HourPlayedTraits

plot( (dayPlayedTraits$totalHeal+1),dayPlayedTraits$nextHourGameDuration)
plot( log(dayPlayedTraits$totalHeal+1),dayPlayedTraits$nextHourGameDuration)

cor( dayPlayedTraits$goldSpent,dayPlayedTraits$nextHourGameDuration)
cor(log(dayPlayedTraits$goldSpent+1),dayPlayedTraits$nextHourGameDuration)

wayo_hour <- lm(nextHourGameDuration ~    assists + deaths +
                  kills + largestKillingSpree + goldSpent + totalHealsOnTeammates +
                  totalTimeSpentDead + totalDamageDealt+ totalHeal+
                  totalDamageTaken+ consumablesPurchased+ champLevel +nexusLost +
                  nexusTakedowns + nexusKills
                , data=HourPlayedTraits )

summary(wayo_hour)

