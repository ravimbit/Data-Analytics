## get the dataset of baseball

baseball = read.csv("baseball.csv")

str(baseball)

head(baseball)

library(ggplot2)

ggplot(baseball,aes(x=Year,y=W, color = Playoffs))+geom_point()

## This plot illustrates that the chances of getting into Playoffs are high
## When  the team has won more then 95 games

## next Observation is between the difference of Runs Scored and Runs Allowed to number of wins

baseball$RD<-baseball$RS-baseball$RA

## correlation between Run Difference and Wins

plot(baseball$RD,baseball$W)

## Visually a high correlation observed

cor(baseball$RD,baseball$W)

## correlation coefficient found to be very high at 0.938

baseballlm<-lm(W~RD, data=baseball)
summary(baseballlm)

## W = 80.904+0.104*RD is the resulting Linear Regression Equation

## to qualify to playoffs the teams need to win atleast 95 games so 
## we now need to find the difference between runs scored and runs allowed to win 95 games.

## W = 80.904+0.104*RD
95 >= 80.904+0.104*RD
RD = (95 -80.1)/0.104

## RD : The difference between runs scored and runs allowed should be 143 to make it 
## to the playoffs

## Identify the variables which determine the runs scored

baseballrslm<-lm(RS~OBP+SLG+BA+OOBP+OSLG,data = baseball)

summary(baseballrslm)

vif(baseballrslm)

baseballrslm1<-lm(RS~OBP+SLG,data = baseball)

summary(baseballrslm1)

## Runs scored is determined by OBP and SLG

RS = -811.66 + 2830.70*OBP + 1517.58*SLG

## from this we can easliy infer that the On base Percentage is the most
## important variable , SLG is important and Batting Average is overvalued


team_rank<-c(1,2,3,3,4,4,4,4,5,5)

team_wins<-c(94,88,95,88,93,94,98,97,93,94)

r<-cor(team_rank,team_wins)

r 

## r value is 0.347

team_wins2013<-c(97,97,92,93,92,96,94,96,92,90)

r2013<-cor(team_rank,team_wins2013)

r2013

## r value is -0.655

## The correlation values are different in signs and are not near for comparison
## so one cannot determine a clear correlation between winning the world series and wins


## Before Moneyball /Sabermetrics was introduced in 2002 Oakland a's won abot 90 games
## for roughly 30 million dollar spend, teams which spent the same amount were winning about 60 games
## post Moneyball techniques, this has changed and now Teams win more for less amount


## The human behavior always has an element of unpredicatability and risk the goal
## of the front office is to ensure this risk is mimised and decisions are taken based on 
## data and not by gut feeling

## Numbers dont lie impact index book on cricket


## Sabermetrics for Basketball

nba<-read.csv("NBA_test.csv")

nba<-read.csv("NBA_train.csv")

str(nba)

head(nba)

ggplot(nba,aes(x=SeasonEnd,y=W,colour = Playoffs))+geom_point()

## The chances of making it to the playoffs are high if you have won more then 40 games

nba$PD<-nba$PTS-nba$oppPTS

plot(nba$PD,nba$W)

table(nba$W,nba$Playoffs)

## works like a pivot table 

## There is a strong correlation between the difference in points scored and wins

nbamodel<-lm(PTS~FG+FGA+X2P+X2PA+X3P+X3PA+FT+FTA+ORB+DRB+AST+STL+BLK+TOV,data=nba)

library(car)

library(MASS)

step<-stepAIC(nbamodel,direction = "both")

nbamodel<-lm(PTS~FG+FGA+X2P+X2PA+FT+FTA+ORB+DRB+AST+STL+BLK+TOV,data=nba)

### Need to check for SSE and RMSE 

summary(nbamodel)

vif(nbamodel)