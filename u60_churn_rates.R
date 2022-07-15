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




#------urf retention-------#

## tries to see impact of the urf retention rate vs overall retention rate
urf <- test_data_locked[test_data_locked$gameMode == 'URF',]

max(urf$gameStartTimestamp)
min(urf$gameStartTimestamp)

plot(urf$gameStartTimestamp)
typeof(urf$gameStartTimestamp)

urf <- urf[urf$gameStartTimestamp >= '2022-05-01 16:00:56 UTC',]

## when did urf game run in 2022, going to use later for graphs
min_urf <- min(urf$gameStartTimestamp)
min_urf
max_urf <- max(urf$gameStartTimestamp)
max_urf



#--------- intdividual game mode retentions based off current gamemode but all future gamemodes---------#
## need to change to ymd and ensure as.date, do this for everytime
urf$gameStartTimestamp <- as.Date(urf$gameStartTimestamp)
hi <- ymd(urf$gameStartTimestamp)
urf$gameStartTimestamp <- hi



#daily retention for urf alone
Retention_urf_2_day <- urf %>% 
  group_by(gameStartTimestamp) %>% 
  summarise(retention=length(intersect(puuid,urf$puuid[urf$gameStartTimestamp==(gameStartTimestamp+1)]))/n_distinct(puuid)) %>% 
  mutate(retention=lag(retention))

mean(Retention_urf_2_day$retention, na.rm =TRUE)

ggplot(aes(x=gameStartTimestamp, y= retention), data=Retention_urf_2) + geom_point()


#weekly retention for urf alone

Retention_urf_2_week <- urf %>% 
  group_by(gameStartTimestamp) %>% 
  summarise(retention=length(intersect(puuid,urf$puuid[urf$gameStartTimestamp==(gameStartTimestamp+7)]))/n_distinct(puuid)) %>% 
  mutate(retention=lag(retention,n=7))

mean(Retention_urf_2_week$retention, na.rm =TRUE)

ggplot(aes(x=gameStartTimestamp, y= retention), data=Retention_urf_2) + geom_point()



#------all retention for all games of urf aram and classic-------#
##dissects by each game mode,


all_retention <- test_data_locked
all_retention <- all_retention[all_retention$gameStartTimestamp >= '2022-04-01 16:00:56 UTC',]
all_retention$gameStartTimestamp <- as.Date(all_retention$gameStartTimestamp)
all_retention$gameStartTimestamp <- ymd(all_retention$gameStartTimestamp)


Retention_all_3_day <- all_retention %>% 
  group_by(gameStartTimestamp) %>% 
  summarise(retention=length(intersect(puuid,all_retention$puuid[all_retention$gameStartTimestamp==(gameStartTimestamp+1)]))/n_distinct(puuid)) %>% 
  mutate(retention=lag(retention))

mean(Retention_all_2$retention, na.rm =TRUE)

ggplot(aes(x=gameStartTimestamp, y= retention), data=Retention_all_3_day) + geom_point()
library(zoo)

temp.zoo <- zoo(Retention_all_3_day$retention, Retention_all_3_day$gameStartTimestamp)
m.av <- rollmean(temp.zoo, 1, fill = list(NA, NULL, NA))

Retention_all_3_day$ret.ma <- coredata(m.av)

ggplot(Retention_all_3_day, aes(gameStartTimestamp, retention)) + geom_point()  + 
  geom_line(aes(gameStartTimestamp, retention), color = 'red')


#--- -----------classic----------#

classic <- test_data_locked[test_data_locked$gameMode == 'CLASSIC',]

classic <- classic[classic$gameStartTimestamp >= '2022-05-01 16:00:56 UTC',]


classic$gameStartTimestamp <- as.Date(classic$gameStartTimestamp)
hi <- ymd(classic$gameStartTimestamp)
classic$gameStartTimestamp <- hi

#daily retention


Retention_classic_2_day <- classic %>% 
  group_by(gameStartTimestamp) %>% 
  summarise(retention=length(intersect(puuid,classic$puuid[classic$gameStartTimestamp==(gameStartTimestamp+1)]))/n_distinct(puuid)) %>% 
  mutate(retention=lag(retention))

mean(Retention_classic_2_day$retention, na.rm =TRUE)

ggplot(aes(x=gameStartTimestamp, y= retention), data=Retention_classic_2) + geom_point()


#weekly

Retention_classic_2_week <- classic %>% 
  group_by(gameStartTimestamp) %>% 
  summarise(retention=length(intersect(puuid,classic$puuid[classic$gameStartTimestamp==(gameStartTimestamp+7)]))/n_distinct(puuid)) %>% 
  mutate(retention=lag(retention,n=7))

mean(Retention_classic_2_week$retention, na.rm =TRUE)

ggplot(aes(x=gameStartTimestamp, y= retention), data=Retention_classic_2) + geom_point()


#----------- end of classic---------#


#--- -----------aram----------#

aram <- test_data_locked[test_data_locked$gameMode == 'ARAM',]

aram <- aram[aram$gameStartTimestamp >= '2022-05-01 16:00:56 UTC',]

min(aram$gameStartTimestamp)

aram$gameStartTimestamp <- as.Date(aram$gameStartTimestamp)
hi <- ymd(aram$gameStartTimestamp)
aram$gameStartTimestamp <- hi

#daily retention


Retention_aram_2_day <- aram %>% 
  group_by(gameStartTimestamp) %>% 
  summarise(retention=length(intersect(puuid,aram$puuid[aram$gameStartTimestamp==(gameStartTimestamp+1)]))/n_distinct(puuid)) %>% 
  mutate(retention=lag(retention))

mean(Retention_aram_2_day$retention, na.rm =TRUE)

ggplot(aes(x=gameStartTimestamp, y= retention), data=Retention_aram_2_day) + geom_point()


#weekly

Retention_aram_2_week <- aram %>% 
  group_by(gameStartTimestamp) %>% 
  summarise(retention=length(intersect(puuid,aram$puuid[aram$gameStartTimestamp==(gameStartTimestamp+7)]))/n_distinct(puuid)) %>% 
  mutate(retention=lag(retention,n=7))

mean(Retention_aram_2_week$retention, na.rm =TRUE)

ggplot(aes(x=gameStartTimestamp, y= retention), data=Retention_aram_2) + geom_point() + geom_ma(ma_fun = SMA, n=3)


#----------- end of aram---------#

#----------------- end of individual gameMode retention rates----------------#
## slight flaw in this as it counted if they played in future games of any type only based on current game type they played


mean(Retention_urf_2_day$retention, na.rm =TRUE)
mean(Retention_classic_2_day$retention, na.rm =TRUE)
mean(Retention_aram_2_day$retention, na.rm =TRUE)















#---------------------------------- All after urf, and during urf-and 1 month before urf------------------#

## this is the core analysis used in the disseration, it wants to understand the affects of
## introducing a new game mode into a time period and if it increases retention rate

#finds retention of all data points during urf period may 12 2022 - june 14 2022 roughly
all_during_urf <- test_data_locked
min_urf <- ymd(as.Date(min_urf))
max_urf <- ymd(as.Date(max_urf))
all_during_urf$gameStartTimestamp <- ymd(as.Date(all_during_urf$gameStartTimestamp))
all_during_urf <- all_during_urf[all_during_urf$gameStartTimestamp >= min_urf & all_during_urf$gameStartTimestamp <= max_urf ,]

min(all_during_urf$gameStartTimestamp)

all_during_urf$gameStartTimestamp <- as.Date(all_during_urf$gameStartTimestamp)
hi <- ymd(all_during_urf$gameStartTimestamp)
all_during_urf$gameStartTimestamp <- hi

#daily retention
Retention_all_during_urf_2_day <- all_during_urf %>% 
  group_by(gameStartTimestamp) %>% 
  summarise(retention=length(intersect(puuid,all_during_urf$puuid[all_during_urf$gameStartTimestamp==(gameStartTimestamp+1)]))/n_distinct(puuid)) %>% 
  mutate(retention=lag(retention))

mean(Retention_all_during_urf_2_day$retention, na.rm =TRUE)

#plots the daily retention with 3 day moving average for tracking
ggplot(aes(x=gameStartTimestamp, y= retention), data=Retention_all_during_urf_2_day) + geom_point() + geom_ma(ma_fun = SMA, n=3) +
  labs(x= 'Date', y = 'Retetion Rate ', title = 'Retention Rate of Players and 3 Day Moving Average',
       subtitle = 'During URF Game Run Period')




#inds retention of all data points after urf period june 14 2022 roughly
all_after_urf <- test_data_locked
min_urf <- ymd(as.Date(min_urf))
max_urf <- ymd(as.Date(max_urf))
all_after_urf$gameStartTimestamp <- ymd(as.Date(all_after_urf$gameStartTimestamp))
all_after_urf <- all_after_urf[(all_after_urf$gameStartTimestamp >= max_urf)   ,]

min(all_after_urf$gameStartTimestamp)
max(all_after_urf$gameStartTimestamp)
all_after_urf$gameStartTimestamp <- as.Date(all_after_urf$gameStartTimestamp)
hi <- ymd(all_after_urf$gameStartTimestamp)
all_after_urf$gameStartTimestamp <- hi

#daily retention calc
Retention_all_after_urf_2_day <- all_after_urf %>% 
  group_by(gameStartTimestamp) %>% 
  summarise(retention=length(intersect(puuid,all_after_urf$puuid[all_after_urf$gameStartTimestamp==(gameStartTimestamp+1)]))/n_distinct(puuid)) %>% 
  mutate(retention=lag(retention))

mean(Retention_all_after_urf_2_day$retention, na.rm =TRUE)

#plot retention after urf game
ggplot(aes(x=gameStartTimestamp, y= retention), data=Retention_all_after_urf_2_day) + geom_point() + geom_ma(ma_fun = SMA, n=3) +
  labs(x= 'Date', y = 'Retetion Rate ', title = 'Retention Rate of Players and 3 Day Moving Average',
       subtitle = 'After URF Game Run Period')




#inds retention of all data points during before urf period may 12 2022 roughly
all_before_urf <- test_data_locked
min_urf <- ymd(as.Date(min_urf))
max_urf <- ymd(as.Date(max_urf))
all_before_urf$gameStartTimestamp <- ymd(as.Date(all_before_urf$gameStartTimestamp))
all_before_urf <- all_before_urf[(all_before_urf$gameStartTimestamp <= min_urf & all_before_urf$gameStartTimestamp >= min_urf-30)   ,]

min(all_before_urf$gameStartTimestamp)
max(all_before_urf$gameStartTimestamp)
all_before_urf$gameStartTimestamp <- as.Date(all_before_urf$gameStartTimestamp)
hi <- ymd(all_before_urf$gameStartTimestamp)
all_before_urf$gameStartTimestamp <- hi

#daily retention before urf games started
Retention_all_before_urf_2_day <- all_before_urf %>% 
  group_by(gameStartTimestamp) %>% 
  summarise(retention=length(intersect(puuid,all_before_urf$puuid[all_before_urf$gameStartTimestamp==(gameStartTimestamp+1)]))/n_distinct(puuid)) %>% 
  mutate(retention=lag(retention))

mean(Retention_all_before_urf_2_day$retention, na.rm =TRUE)
#plot retention before urf with moving average
ggplot(aes(x=gameStartTimestamp, y= retention), data=Retention_all_before_urf_2_day) + geom_point()  + geom_ma(ma_fun = SMA, n=3) +
  labs(x= 'Date', y = 'Retetion Rate ', title = 'Retention Rate of Players and 3 Day Moving Average',
       subtitle = '30 Days Before URF Game Run Period')



#------done-------#

mean(Retention_all_during_urf_2_day$retention, na.rm =TRUE)
mean(Retention_all_after_urf_2_day$retention, na.rm =TRUE)

ggplot(aes(x=gameStartTimestamp, y= retention), data=Retention_all_after_urf_2_day) + geom_point()
ggplot(aes(x=gameStartTimestamp, y= retention), data=Retention_all_during_urf_2_day) + geom_point()




