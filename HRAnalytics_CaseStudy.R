#############################Telecom Solution###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

# A large company named XYZ, employs, at any given point of time, around 4000 employees. 
# However, every year, around 15% of its employees leave the company and need to be replaced with the talent pool available in the job market. 
# The management believes that this level of attrition is bad for the company

## AIM:

# The aim is to model the probability of attrition using a logistic regression

################################################################

### Data Understanding

# Install and Load the required packages
install.packages("MASS")
install.packages("car")
install.packages("e1071")
install.packages("caret")
install.packages("cowplot")
install.packages("GGally")
install.packages("caTools")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(lubridate)
library(dplyr)
library(tidyr)

# Loading 5 files
employee_survey_data<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
general_data<- read.csv("general_data.csv", stringsAsFactors = F)
in_time<- read.csv("in_time.csv", stringsAsFactors = F)
manager_survey_data<- read.csv("manager_survey_data.csv", stringsAsFactors = F)
out_time<- read.csv("out_time.csv", stringsAsFactors = F)

#employee_survey_data<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
#general_data<- read.csv("general_data.csv", stringsAsFactors = F)
#in_time<- read.csv("in_time.csv", stringsAsFactors = F)
#manager_survey_data<- read.csv("manager_survey_data.csv", stringsAsFactors = F)
#out_time<- read.csv("out_time.csv", stringsAsFactors = F)

str(employee_survey_data)  	# 4410 obs. of  4 variables
str(general_data) 			# 4410 obs. of  24 variables including target variable
str(in_time) 				# 4410 obs. of  262 variables
str(manager_survey_data) 	# 4410 obs. of  3 variables
str(out_time) 				# 4410 obs. of  262 variables

colnames(in_time)[1] <- "EmployeeID" 					# Rename the first column as EmployeeID
colnames(out_time)[1] <- "EmployeeID" 					# Rename the first column as EmployeeID

# Collate the data together in one single file
length(unique(tolower(employee_survey_data$EmployeeID)))    # 4410, confirming EmployeeID is key 
length(unique(tolower(general_data$EmployeeID))) 			# 4410, confirming EmployeeID is key 
length(unique(tolower(manager_survey_data$EmployeeID))) 	# 4410, confirming EmployeeID is key 
length(unique(tolower(in_time$EmployeeID)))					# 4410, confirming EmployeeID is key 
length(unique(tolower(out_time$EmployeeID)))				# 4410, confirming EmployeeID is key 

setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID)	# Identical customerID across these datasets
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID) 	# Identical customerID across these datasets
setdiff(general_data$EmployeeID,in_time$EmployeeID)					# Identical customerID across these datasets
setdiff(general_data$EmployeeID,out_time$EmployeeID) 				# Identical customerID across these datasets

new_general_data<- merge(general_data,employee_survey_data, by="EmployeeID", all = F)
new_general_data<- merge(new_general_data,manager_survey_data, by="EmployeeID", all = F)
View(new_general_data) #master file

in_time$leave_count <- rowSums(is.na(in_time))			# Counting the number of leaves an employee has taken (including public holidays)
out_time$leave_count <- rowSums(is.na(out_time))		# Counting the number of leaves an employee has taken (including public holidays)
in_time$working_days <- rowSums(!is.na(in_time))		# Counting the number of days worked by an employee
out_time$working_days <- rowSums(!is.na(out_time))		# Counting the number of days worked by an employee
sum(in_time$leave_count) == sum(out_time$leave_count)	# True; Checking if the sum is same to rule out any data entry/capture error
sum(in_time$working_days) == sum(out_time$working_days)	# True; Checking if the sum is same to rule out any data entry/capture error

long_in_time <- gather(in_time, Date, in_time, X2015.01.01:X2015.12.31)		# converting the in_time to long form
long_out_time <- gather(out_time, Date, out_time, X2015.01.01:X2015.12.31)	# converting the out_time to long form

# lookup holidays in new_general_data
subset_in_time <- long_in_time[1:4410,c("EmployeeID","leave_count")]
master_data <- merge(x = new_general_data, y = subset_in_time, by.x = "EmployeeID", by.y = "EmployeeID", all.x=TRUE)
View(master_data) 

################################################################

### Data Preparation & Exploratory Data Analysis

# Understanding the structure of the collated file
str(master_data) #4410 obs. of  30 variables

## Categorical variables BusinessTravel, Department, Gender, JobRole, JobLevel,MartialStatus,Over18

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

HR<-master_data

## stacked bar charts for categorical variables 

plot_grid(
  ggplot(HR, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+ bar_theme1 , 
  ggplot(HR, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
  ggplot(HR, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
  ggplot(HR, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
  ggplot(HR, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
  ggplot(HR, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
  ggplot(HR, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
  ggplot(HR, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
  ggplot(HR, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1,
  ggplot(HR, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
  ggplot(HR, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
  ggplot(HR, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
  ggplot(HR, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
  ggplot(HR, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
  align = "h")   

## We can include some observations here


box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")



plot_grid(ggplot(HR, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(HR, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) ## No Outliers Found

plot_grid(ggplot(HR, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(HR, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)  ## No Outliers Found

plot_grid(ggplot(HR, aes(MonthlyIncome))+ geom_histogram(binwidth = 10),
          ggplot(HR, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

## Outliers found and removed through the boxplot.stats function


box <- boxplot.stats(HR$MonthlyIncome)
out <- box$out

HR1 <- HR[ !HR$MonthlyIncome %in% out, ]

plot_grid(ggplot(HR1, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),
          ggplot(HR1, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) ## Outliers found for Total Working Years

box <- boxplot.stats(HR1$TotalWorkingYears)
out <- box$out


HR2 <- HR1[ !HR1$TotalWorkingYears %in% out, ] ##  Outliers removed for Total Working Years

plot_grid(ggplot(HR2, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(HR, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

box <- boxplot.stats(HR2$YearsAtCompany)
out <- box$out

HR3 <- HR2[ !HR2$YearsAtCompany %in% out, ]


##plot_grid(ggplot(HR3, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),
##          ggplot(HR3, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
## align = "v",ncol = 1)

##box <- boxplot.stats(HR3$YearsSinceLastPromotion)
##out <- box$out

##HR4 <- HR3[ !HR3$YearsAtCompany %in% out, ]

plot_grid(ggplot(HR3, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 10),
          ggplot(HR3, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

box <- boxplot.stats(HR3$YearsWithCurrManager)
out <- box$out

HR4 <- HR3[ !HR3$YearsWithCurrManager %in% out, ]


plot_grid(ggplot(HR4, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
          ggplot(HR4, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) ## No Outliers Found

plot_grid(ggplot(HR4, aes(leave_count))+ geom_histogram(binwidth = 10),
          ggplot(HR4, aes(x="",y=leave_count))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) ## No Outliers Found

# Boxplots of numeric variables relative to Attrition

plot_grid(ggplot(HR4, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(HR4, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(HR4, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(HR4, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(HR4, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(HR4, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(HR4, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(HR4, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(HR4, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(HR4, aes(x=Attrition,y=leave_count, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          align = "v",nrow = 1)

## those with lesser age have higher attrition
## those with less number of years with current manager have a higher attrition
## those with less number of years at company have a higher attrition

## Missing Values

sapply(HR4, function(x) sum(is.na(x)))

## Missing values are found for Total Working Years -6, EnvironmentSatisfaction -19, JobSatisfaction -16,
##WorkLifeBalance - 31 , Total missing values --72 rows, Total number of records : nrow(HR4):3672
## NA values form 72/3672 less than 2% and so are removed.

HR5<-na.omit(HR4)

master_data<-HR5

nrow(master_data)

# Excluding constant value columns - EmployeeCount, Over18, StandardHours
master_data <- subset(master_data, select = -c(EmployeeCount, Over18, StandardHours))

str(master_data) ## 3582 of 27 variables

################################################################
# Feature standardization

# Normalizing continuous features 
master_data$Age <- scale(master_data$Age) 
master_data$DistanceFromHome <- scale(master_data$DistanceFromHome) 
master_data$MonthlyIncome <- scale(master_data$MonthlyIncome)  
master_data$NumCompaniesWorked<- scale(master_data$NumCompaniesWorked) 
master_data$PercentSalaryHike<- scale(master_data$PercentSalaryHike) 
master_data$TotalWorkingYears<- scale(master_data$TotalWorkingYears) 
master_data$TrainingTimesLastYear<- scale(master_data$TrainingTimesLastYear) 
master_data$YearsAtCompany<- scale(master_data$YearsAtCompany) 
master_data$YearsSinceLastPromotion<- scale(master_data$YearsSinceLastPromotion) 
master_data$YearsWithCurrManager<- scale(master_data$YearsWithCurrManager) 
master_data$leave_count<- scale(master_data$leave_count)

# converting target variable(Attrition) from No/Yes character to factor with levels 0/1 
master_data$Attrition <- ifelse(master_data$Attrition == "Yes", 1, 0)

# Checking attrition rate
Attrition_rate <- sum(master_data$Attrition)/nrow(master_data)
Attrition_rate # 17.28% 

# creating a data frame of categorical features
master_data_chr <- master_data[,c(4,5,7:12,16,22:26)]

# converting categorical attributes to factor
master_data_fact <- data.frame(sapply(master_data_chr, function(x) factor(x)))
str(master_data_fact) ## 3582 of 14 variables
 
# creating dummy variables for factor attributes
dummies<- data.frame(sapply(master_data_fact, function(x) data.frame(model.matrix(~x-1,data =master_data_fact))[,-1]))

# For variables having only two levels, gender "male" is 1 

# Final dataset
data_final <- cbind(master_data[,-c(4,5,7:12,16,22:26)],dummies)  
str(data_final) #3582 obs. of  57 variables

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(data_final$Attrition, SplitRatio = 0.7)

train = data_final[indices,]

test = data_final[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) # AIC: 2211.1; Null deviance: 2652.7; Residual deviance: 2113.1

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
vif(model_2)

#Excluding EducationField.xLife.Sciences because of high VIF
model_3<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  TrainingTimesLastYear+  
                    +YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
              + BusinessTravel.xTravel_Rarely+ Department.xSales
              +Education.x3 + Education.x4 + Education.x5  + EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree +JobLevel.x4+
                +JobRole.xHuman.Resources + JobRole.xManager+JobRole.xManufacturing.Director+JobRole.xResearch.Director
              + +JobRole.xSales.Representative+MaritalStatus.xSingle
                +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
              +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
              +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
              +JobInvolvement.x2+JobInvolvement.x3
               , family = "binomial", data = train)

summary(model_3) 

vif(model_3) 

## Removing JobInvolvement.x2,JobRole.xSales.Representative ,Education.x3 due to  Low Significane

model_4<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  TrainingTimesLastYear+  
                +YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
              + BusinessTravel.xTravel_Rarely+ Department.xSales
              + Education.x4 + Education.x5  + EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree +JobLevel.x4+
                +JobRole.xHuman.Resources + JobRole.xManager+JobRole.xManufacturing.Director+JobRole.xResearch.Director
              +MaritalStatus.xSingle
              +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
              +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
              +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
              +JobInvolvement.x3
              , family = "binomial", data = train)

summary(model_4)

vif(model_4)
 ##Removing Education.x4 due to low significance
model_5<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  TrainingTimesLastYear+  
                +YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
              + BusinessTravel.xTravel_Rarely+ Department.xSales
              + Education.x5  + EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree +JobLevel.x4+
                +JobRole.xHuman.Resources + JobRole.xManager+JobRole.xManufacturing.Director+JobRole.xResearch.Director
              +MaritalStatus.xSingle
              +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
              +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
              +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
              +JobInvolvement.x3
              , family = "binomial", data = train)

summary(model_5)

vif(model_5)

## Removing YearsAtCompany due to low significance and high vif
model_6<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  TrainingTimesLastYear+  
                + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
              + BusinessTravel.xTravel_Rarely+ Department.xSales
              + Education.x5  + EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree +JobLevel.x4+
                +JobRole.xHuman.Resources + JobRole.xManager+JobRole.xManufacturing.Director+JobRole.xResearch.Director
              +MaritalStatus.xSingle
              +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
              +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
              +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
              +JobInvolvement.x3
              , family = "binomial", data = train)

summary(model_6)

vif(model_6)

## Removing BusinessTravel.xTravel_Rarely due to low signifance

model_7<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  TrainingTimesLastYear+  
                + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
              + Department.xSales
              + Education.x5  + EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree +JobLevel.x4+
                +JobRole.xHuman.Resources + JobRole.xManager+JobRole.xManufacturing.Director+JobRole.xResearch.Director
              +MaritalStatus.xSingle
              +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
              +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
              +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
              +JobInvolvement.x3
              , family = "binomial", data = train)

summary(model_7)

vif(model_7)

## Removing EducationField.xMedical due to low significane
model_8<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  TrainingTimesLastYear+  
                + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
              + Department.xSales
              + Education.x5   + EducationField.xOther + 
                EducationField.xTechnical.Degree +JobLevel.x4+
                +JobRole.xHuman.Resources + JobRole.xManager+JobRole.xManufacturing.Director+JobRole.xResearch.Director
              +MaritalStatus.xSingle
              +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
              +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
              +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
              +JobInvolvement.x3
              , family = "binomial", data = train)

summary(model_8)

vif(model_8)

## Removing EducationField.xTechnical.Degree due to low significane
model_9<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  TrainingTimesLastYear+  
                + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
              + Department.xSales
              + Education.x5   + EducationField.xOther + 
                +JobLevel.x4+
                +JobRole.xHuman.Resources + JobRole.xManager+JobRole.xManufacturing.Director+JobRole.xResearch.Director
              +MaritalStatus.xSingle
              +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
              +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
              +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
              +JobInvolvement.x3
              , family = "binomial", data = train)

summary(model_9)

vif(model_9)

## Removing JobRole.xHuman.Resources due to low significane
model_10<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  TrainingTimesLastYear+  
                + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
              + Department.xSales
              + Education.x5   + EducationField.xOther + 
                +JobLevel.x4+
                 JobRole.xManager+JobRole.xManufacturing.Director+JobRole.xResearch.Director
              +MaritalStatus.xSingle
              +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
              +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
              +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
              +JobInvolvement.x3
              , family = "binomial", data = train)

summary(model_10)

vif(model_10)

## Removing JobLevel.x4 due to low significance
model_11<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  TrainingTimesLastYear+  
                 + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
               + Department.xSales
               + Education.x5   + EducationField.xOther + 
                 JobRole.xManager+JobRole.xManufacturing.Director+JobRole.xResearch.Director
               +MaritalStatus.xSingle
               +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
               +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
               +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
               +JobInvolvement.x3
               , family = "binomial", data = train)

summary(model_11)

vif(model_11)

## Removing EducationField.xOther due to low significance
model_12<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  TrainingTimesLastYear+  
                 + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
               + Department.xSales
               + Education.x5   + 
                 JobRole.xManager+JobRole.xManufacturing.Director+JobRole.xResearch.Director
               +MaritalStatus.xSingle
               +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
               +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
               +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
               +JobInvolvement.x3
               , family = "binomial", data = train)

summary(model_12)

vif(model_12)

## Removing Education.x5 due to low significance
model_13<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  TrainingTimesLastYear+  
                 + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
               + Department.xSales
               + 
                 JobRole.xManager+JobRole.xManufacturing.Director+JobRole.xResearch.Director
               +MaritalStatus.xSingle
               +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
               +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
               +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
               +JobInvolvement.x3
               , family = "binomial", data = train)

summary(model_13)

vif(model_13)

## Removing Job Involvement.X3 due to low significance
model_14<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  TrainingTimesLastYear+  
                 + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
               + Department.xSales
               + 
                 JobRole.xManager+JobRole.xManufacturing.Director+JobRole.xResearch.Director
               +MaritalStatus.xSingle
               +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
               +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
               +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
                              , family = "binomial", data = train)

summary(model_14)

vif(model_14)

## Removing Department.xSales due to low significance
model_15<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  TrainingTimesLastYear+  
                 + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
                             + 
                 JobRole.xManager+JobRole.xManufacturing.Director+JobRole.xResearch.Director
               +MaritalStatus.xSingle
               +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
               +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
               +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
               , family = "binomial", data = train)

summary(model_15)

vif(model_15)

## Removing JobRole.xResearch.Director due to low significance
model_16<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  TrainingTimesLastYear+  
                 + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
               + 
                 JobRole.xManager+JobRole.xManufacturing.Director+
               +MaritalStatus.xSingle
               +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
               +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
               +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
               , family = "binomial", data = train)

summary(model_16)

vif(model_16)

## Removing TrainingTimesLastYear due to low significance
model_17<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  
                 + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
               + 
                 JobRole.xManager+JobRole.xManufacturing.Director+
                 +MaritalStatus.xSingle
               +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
               +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
               +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
               , family = "binomial", data = train)

summary(model_17)

vif(model_17)

## Removing JobRole.xManager due to low significance
model_18<- glm(formula = Attrition ~ Age +NumCompaniesWorked+TotalWorkingYears+  
                 + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
                   +JobRole.xManufacturing.Director+
                 +MaritalStatus.xSingle
               +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
               +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
               +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
               , family = "binomial", data = train)

summary(model_18)

vif(model_18)

## Removing Age due to low significance
model_19<- glm(formula = Attrition ~ NumCompaniesWorked+TotalWorkingYears+  
                 + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
               +JobRole.xManufacturing.Director+
                 +MaritalStatus.xSingle
               +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
               +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
               +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
               , family = "binomial", data = train)

summary(model_19)

vif(model_19)

## Removing JobRole.xManufacturing.Director due to low significance
model_20<- glm(formula = Attrition ~ NumCompaniesWorked+TotalWorkingYears+  
                 + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
               +MaritalStatus.xSingle
               +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
               +JobSatisfaction.x2+JobSatisfaction.x3+JobSatisfaction.x4
               +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
               , family = "binomial", data = train)

summary(model_20)

vif(model_20)

## Removing JobSatisfaction.x2 due to low significance
model_21<- glm(formula = Attrition ~ NumCompaniesWorked+TotalWorkingYears+  
                 + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
               +MaritalStatus.xSingle
               +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
               +JobSatisfaction.x3+JobSatisfaction.x4
               +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
               , family = "binomial", data = train)

summary(model_21)

vif(model_21)

## Removing JobSatisfaction.x3 due to low significance
model_22<- glm(formula = Attrition ~ NumCompaniesWorked+TotalWorkingYears+  
                 + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
               +MaritalStatus.xSingle
               +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
               +JobSatisfaction.x4
               +WorkLifeBalance.x2+WorkLifeBalance.x3+WorkLifeBalance.x4
               , family = "binomial", data = train)

summary(model_22)

vif(model_22)

## Removing WorkLifeBalance.x4 due to low significance
model_23<- glm(formula = Attrition ~ NumCompaniesWorked+TotalWorkingYears+  
                 + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
               +MaritalStatus.xSingle
               +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
               +JobSatisfaction.x4
               +WorkLifeBalance.x2+WorkLifeBalance.x3
               , family = "binomial", data = train)

summary(model_23)

vif(model_23)

## Removing WorkLifeBalance.x4 due to low significance
model_24<- glm(formula = Attrition ~ NumCompaniesWorked+TotalWorkingYears+  
                 + YearsSinceLastPromotion + YearsWithCurrManager +leave_count +BusinessTravel.xTravel_Frequently
               +MaritalStatus.xSingle
               +EnvironmentSatisfaction.x2 +EnvironmentSatisfaction.x3+EnvironmentSatisfaction.x4
               +JobSatisfaction.x4
               +WorkLifeBalance.x3
               , family = "binomial", data = train)

summary(model_24)

vif(model_24)

##Final Model with 12 significant variables

final_model<-model_24



#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Churn 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)


# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.15, "Yes", "No"))

test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))



table(test_actual_attrition,test_pred_attrition)




#######################################################################
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf



#######################################################################

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, test_actual_churn, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.3132 for final model

test_cutoff_churn <- factor(ifelse(test_pred >=0.3132, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_churn, test_actual_churn, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_churn <- ifelse(test_cutoff_churn=="Yes",1,0)
test_actual_churn <- ifelse(test_actual_churn=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_churn, test_actual_churn)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_churn, test_pred, groups = 10)