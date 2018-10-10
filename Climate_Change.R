################ Climate Change Understanding through Linear Regression ############

##There have been many studies documenting that the average global temperature has been increasing over the last century. The consequences of a continued rise in global temperature will be dire. Rising sea levels and an increased frequency of extreme weather events will affect billions of people

## Aim of the case study is to identify variables which lead to increase in Global temperatures

library(dplyr)
library(MASS)
library(Car)
library(sqldf)

### set the working directory
setwd("C:/Users/rmanj/Downloads")

### Import the data set
climate_change<-read.csv("climate_change.csv",stringsAsFactors = FALSE)

str(climate_change)

summary(climate_change)

## Data Cleanising as part of Crisp DM framework initiated

sqldf("Select Year, Month, count(*) from climate_change group by Year,Month having count(*) >1")

## No duplicate values found in the dataset

## Check for NA values 

which(is.na(climate_change))

## No NA values found 

## Column Names changed for CFC-11 and CFC-12
names(climate_change)[7]<-"CFC11"
names(climate_change)[8]<-"CFC12"


###### Data Cleansing part is complete before proceeding to model building data is split in 70:30 ratio

set.seed(100)

trainidices<-sample(1:nrow(climate_change),0.7*nrow(climate_change))
train<-climate_change[trainidices,]
test<-climate_change[-trainidices,]

### Train data and Test Data are separated

### Model is built with all Indepedent Variables

climate_change_model <- lm(Temp ~ MEI+ CO2+CH4+N2O+CFC11+CFC12+TSI+Aerosols,data=train)


### Step AIC is used to  arrive at a list of variables whicha re significant

step<-stepAIC(climate_change_model,direction = "both")

climate_change_model <- lm(Temp ~ MEI + CO2+CH4+N2O+CFC11+CFC12+TSI+Aerosols,data=train)

summary(climate_change_model)

## CH4 is insignifcant and removed

vif(climate_change_model)

climate_change_model1 <- lm(Temp ~ MEI + CO2+N2O+CFC11+CFC12+TSI+Aerosols,data=train)

summary(climate_change_model1)

climate_change_model2 <- lm(Temp ~ MEI + CO2+CFC11+CFC12+TSI+Aerosols,data=train)

summary(climate_change_model2)

vif(climate_change_model2)

### CFC11 and CFC12 are highly correlated and so are removed


climate_change_model3 <- lm(Temp ~ MEI +N2O+CFC11+CFC12+TSI+Aerosols,data=train)

summary(climate_change_model3)

climate_change_model3 <- lm(Temp ~ MEI +N2O+CFC11+TSI+Aerosols,data=train)

summary(climate_change_model3)

climate_change_model4 <- lm(Temp ~ MEI +CO2+TSI+Aerosols,data=train)

summary(climate_change_model4)

## Final Model is arrived at with Independent variables identified as 
## MEI , CO2, TSI, Aerosols

predict_temp<-predict(climate_change_model4,test[-11])

test$Temp_Test<-predict_temp

r<-cor(test$Temp,test$Temp_Test)

rsquared<-r^2

rsquared

## r squared is found to be 0.69 and is a good fit


