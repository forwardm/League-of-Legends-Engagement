# League-of-Legends-Engagement

The following files will look into finding engagement through the open LOL Riot API. 

THe datafetching code finds many players and their ids/rankings through apis and creates a dataset.

The FetchingGameDataPerChampion file gets and creates a dataframe with all of the the desired players match details. From this it tells a players details for each individual match they participated in.

Riot Games API Data Collection:
1. First Run DataFetching.ipynb
2. Run FetchingGameDataPerChampion

Data Analysis:
1. u60 engagement analysis - general engagement metrics
2. u60 engagement by game type - looks at differences in game data by game type and its effect on engagement, also at bottom of file it has general regression models with lots of variables, also runs stepwise regression.
3. u60 engagement by no time played - takes two groups of data, one who did not play in next time period following a game and those who did, then runs analysis
4. u60 engagement by winning streak - analysis of impacts of winning streaks and ending winning streaks/losingstreaks
5. u60 churn rates - analysis of retention rates before during and after introduction of urf game mode.
