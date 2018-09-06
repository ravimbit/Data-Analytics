#########################

## Predictive Analytics Assignment
## Submitted by : Ravi Manjunatha
library(tidyr)
library(dplyr)
library(MASS)
library(car)
library(sqldf)

## set working directory
setwd("C:/Users/rmanj/Downloads")

## Import Data set
carpriceassignment<-read.csv("CarPrice_Assignment.csv")

str(carpriceassignment)

summary(carpriceassignment)

## Data Cleansing and  preparartion of Crisp DM framework initiated

## check for duplicate rows 
sqldf("Select count(*),car_ID from carpriceassignment group by car_ID having count(*)>1 ")

##No duplicate rows found in the dataset.

## Check for NA values 

which(is.na(carpriceassignment))
  
## None found 
  
## Carname separated to CarName and Model

carpriceassignment<-separate(carpriceassignment,CarName,into = c("CarName","Model"),sep = " ")

View(carpriceassignment)

## carnames are checked for inconsistencies

unique(carpriceassignment$CarName)


## Inconsistencies in CarNames found such as Nissan -nissan, toyouta --toyota ,vw,vokswagen

## CarNames are made consistent

carpriceassignment$CarName<-gsub("maxda","mazda",carpriceassignment$CarName)
carpriceassignment$CarName<-gsub("vw","volkswagen",carpriceassignment$CarName)
carpriceassignment$CarName<-gsub("vokswagen","volkswagen",carpriceassignment$CarName)
carpriceassignment$CarName<-gsub("toyouta","toyota",carpriceassignment$CarName)
carpriceassignment$CarName<-gsub("porcshce","porsche",carpriceassignment$CarName)
carpriceassignment$CarName<-gsub("nissan","Nissan",carpriceassignment$CarName)


unique(carpriceassignment$CarName)

## Variables with 2 levels of factors are identified and assigned levels accordingly

levels(carpriceassignment$fueltype)<-c(0,1)

levels(carpriceassignment$aspiration)<-c(0,1)

levels(carpriceassignment$doornumber)<-c(0,1)

levels(carpriceassignment$doornumber) <- c(4, 2)

carpriceassignment$doornumber <- as.numeric(levels(carpriceassignment$doornumber))[carpriceassignment$doornumber]

levels(carpriceassignment$enginelocation)<-c(0,1)

levels(carpriceassignment$cylindernumber) <- c(8, 5, 4, 6, 3, 12, 2)

carpriceassignment$cylindernumber <- as.numeric(levels(carpriceassignment$cylindernumber))[carpriceassignment$cylindernumber]

## factors with mutiple levels found and are converted to dummy variables

dummycarbody<-data.frame(model.matrix(~carbody,data = carpriceassignment))
dummycarbody<-dummycarbody[-1]
dummycarname<-data.frame(model.matrix(~CarName,data=carpriceassignment))
dummycarname<-dummycarname[-1]
dummydrivewheel<-data.frame(model.matrix(~drivewheel,data = carpriceassignment))
dummydrivewheel<-dummydrivewheel[-1]
dummyenginetype<-data.frame(model.matrix(~enginetype,data = carpriceassignment))
dummyenginetype<-dummyenginetype[-1]
dummyfuelsystem<-data.frame(model.matrix(~fuelsystem,data=carpriceassignment))
dummyfuelsystem<-dummyfuelsystem[-1]

carpriceassignment$symboling<-as.factor(carpriceassignment$symboling)

dummysymboling<-data.frame(model.matrix(~symboling,data=carpriceassignment))
dummysymboling<-dummysymboling[-1]

## car_id is just a unique identified and Model is to be removed as per the assignment guidelines

carpriceassignment_temp<-carpriceassignment[,-c(1,2,4,8,9,16,17,19)]

carpriceassignment_intmd<-cbind(carpriceassignment_temp,dummycarbody,dummycarname,dummydrivewheel,dummyenginetype,dummyfuelsystem,dummysymboling,dummycylindernumber)

carpriceassignment_final<-carpriceassignment_intmd

##The Data cleansing part is complete, before proceeding to model building
## data is split into train and test data in the ratio of 70 : 30

set.seed(100)
trainindices<-sample(1:nrow(carpriceassignment_final),0.7*nrow(carpriceassignment_final))
carpriceassignmenttrain<-carpriceassignment_final[trainindices,]
carpriceassignmenttest<-carpriceassignment_final[-trainindices,]

## from the train dataset a default model is build

model<-lm(price~.,data=carpriceassignmenttrain)
summary(model)

##adjusted R -Square found to be 0.9691

## step AIC amethod is used first to idenity the variables which cannot be predictor variables 
step<-stepAIC(model1,direction="both")

##based  on the results of stepAIC function '+' marked variables are removed and general business understadning of
##auto industry is considered to arrive at the first model with predictor variables, no derived variables found necessary

model2 <- lm(formula = price ~aspiration + enginelocation + 
               curbweight + drivewheelrwd  + enginetyperotor +  carbodyhatchback+
               + CarNameaudi + CarNamebmw + carwidth+horsepower+
               CarNamebuick + CarNamehonda + CarNamejaguar + CarNamemercury + 
               CarNameporsche + CarNamevolvo, data = carpriceassignmenttrain)


summary(model2)

##adjusted R -Square found to be 0.9423
## multicolinearlity checked

vif(model2) 

##carwidth having a high vif of 6.507170 and significnace is 2 **  so not removed for now.
## aspiration removed as no significance


model3 <- lm(formula = price ~enginelocation + 
               curbweight + drivewheelrwd  + enginetyperotor +  carbodyhatchback+
               + CarNameaudi + CarNamebmw + carwidth+horsepower+
               CarNamebuick + CarNamehonda + CarNamejaguar + CarNamemercury + 
               CarNameporsche + CarNamevolvo, data = carpriceassignmenttrain)
summary(model3)

##adjusted R -Square found to be 0.9416

vif(model3)

## High vif for curbwidth --10.4933 but high significane so retained, enginetyperotor removed as no significance

model4 <- lm(formula = price ~enginelocation + curbweight+
               drivewheelrwd   +  carbodyhatchback+horsepower+
               + CarNameaudi + CarNamebmw + carwidth+
               CarNamebuick + CarNamehonda + CarNamejaguar + CarNamemercury + 
               CarNameporsche + CarNamevolvo, data = carpriceassignmenttrain)

summary(model4)
##adjusted R -Square found to be 0.9407
##driveweheelrwd removed as no significance

model5 <- lm(formula = price ~enginelocation + curbweight+
               carbodyhatchback+horsepower+
               + CarNameaudi + CarNamebmw + carwidth+
               CarNamebuick + CarNamehonda + CarNamejaguar + CarNamemercury + 
               CarNameporsche + CarNamevolvo, data = carpriceassignmenttrain)
summary(model5)
##adjusted R -Square found to be 0.941
##carbodyhatchback removed as no significance found

model6 <- lm(formula = price ~enginelocation + curbweight+
               + CarNameaudi + CarNamebmw + carwidth+horsepower+
               CarNamebuick + CarNamehonda + CarNamejaguar + CarNamemercury + 
               CarNameporsche + CarNamevolvo, data = carpriceassignmenttrain)

summary(model6)
##adjusted R -Square found to be 0.9411
##CarNamemercury removed as no significance found

model7 <- lm(formula = price ~enginelocation + curbweight+
               + CarNameaudi + CarNamebmw + carwidth+horsepower+
               CarNamebuick + CarNamehonda + CarNamejaguar + 
               CarNameporsche + CarNamevolvo, data = carpriceassignmenttrain)


summary(model7)
##adjusted R -Square found to be 0.9412
vif(model7) 
##high vif foudn  for curbweight and carwidth but retained as high signicicance
## CarNamehonda removed as no significane found

model8 <- lm(formula = price ~enginelocation + curbweight+
               + CarNameaudi + CarNamebmw + carwidth+horsepower+
               CarNamebuick + CarNamejaguar + 
               CarNameporsche + CarNamevolvo, data = carpriceassignmenttrain)
summary(model8)
## adjusted R -Square found to be 0.9414
##CarNameaudi removed as no significane found

model9 <- lm(formula = price ~enginelocation + curbweight+
               CarNamebmw + carwidth+horsepower+
               CarNamebuick + CarNamejaguar + 
               CarNameporsche + CarNamevolvo, data = carpriceassignmenttrain)

summary(model9)
## adjusted R -Square found to be 0.9389

##CarNamevolvo,CarNameporsche  removed

model9 <- lm(formula = price ~enginelocation + curbweight+
               CarNamebmw + carwidth+horsepower+CarNamebuick + CarNamejaguar, 
             data = carpriceassignmenttrain)
summary(model9)
##0.9376
vif(model9)
##high vif for curbweight and removed

model10 <- lm(formula = price ~enginelocation + carwidth+horsepower+
                CarNamebmw +CarNamebuick + CarNamejaguar, 
              data = carpriceassignmenttrain)

summary(model10) 

##adjusted R square found to be a high  0.934 and all variables are of high significance

vif(model10)

## vif for all variables ofund within permissible limits so this model is retained !

## Model is now validated for test data

predict1<-predict(model11,carpriceassignmenttest[-19])
carpriceassignmenttest$test_price<-predict1

##correlation between actual price and predicted price found
r<-cor( carpriceassignmenttest$test_price, carpriceassignmenttest$price)
rsqaured<-r^2
rsqaured
## rsqaured found to be 0.858
## rsquared found to be high 0.858 so the predictor variables are able to explain 85% of the variation in the test data
## as per the industry standards this correlation is considered above the acceptance threshold 

## Goal of the Assignment
##variables which are significant in predicting the Price of the car  are found to be 
##Carwidth,Enginelocation,Horsepower,Carnames : buick,bmw,jagaur

##So the chinese car company can optimise the Carwidth,Enginelocation and Horsepower in its design
## Car brands Jaguar,Buick and BMW influence the price, so a tie-up or JV with these brands can be cosnidered as well.
