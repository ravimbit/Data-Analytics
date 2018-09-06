library(tidyr)
library(ggplot2)
library(readxl)
library(dplyr)
library(lubridate)
library(xlsx)
library(plyr)
library(ggthemes)
library(scales)
library(stringr)

# Load the loans data in R
preclean_loan <- read.csv("G:/PGDDS/Course 2 - Statistics and Exploratory Data Analytics/8. EDA Case Study/loan.csv", stringsAsFactors = FALSE)
dim(preclean_loan)
str(preclean_loan)

# Identify columns with NA (in all the rows) and remove them from our analysis
new_preclean_loan <- Filter(function(x)!all(is.na(x)), preclean_loan)

# Removing 'pymnt_plan' as it has only one value i.e. 'n' and is not useful in our analysis
# Removing 'url' as it is not useful in our analysis
# Removing 'initial_list_status' as it has only one value i.e. 'f' and is not useful in our analysis
# Removing 'policy_code' as it has only one value i.e. '1' and is not useful in our analysis
# Removing 'application_type' as it has only one value i.e. 'INDIVIDUAL' and is not useful in our analysis
# Removing 'acc_now_delinq' as it has only one value i.e. '0' and is not useful in our analysis
# Removing 'delinq_amnt' as it has only one value i.e. '0' and is not useful in our analysis
# Removing 'collections_12_mths_ex_med', 'chargeoff_within_12_mths' & 'tax_liens' asthey have onnly two values i.e. '0' and 'NA'

newsub_preclean_loan <- subset(new_preclean_loan, select = -c(tax_liens, chargeoff_within_12_mths, collections_12_mths_ex_med, pymnt_plan, url, initial_list_status, policy_code, application_type, acc_now_delinq, delinq_amnt))
dim(newsub_preclean_loan)

# Check for duplicates in the 'id' and 'member_id' columns
sum(duplicated(newsub_preclean_loan$id))
sum(duplicated(newsub_preclean_loan$member_id))
# No duplicates found in id and member_id

# Check for NA in the 'id' and 'member_id' columns
sum(is.na(newsub_preclean_loan$id))
sum(is.na(newsub_preclean_loan$member_id))
# No NAs found in id and member_id

# Remove % from 'int_rate' & 'revol_util'
newsub_preclean_loan$int_rate <- gsub("%", "", newsub_preclean_loan$int_rate)
newsub_preclean_loan$int_rate <- as.numeric(newsub_preclean_loan$int_rate)
newsub_preclean_loan$revol_util <- gsub("%", "", newsub_preclean_loan$revol_util)
newsub_preclean_loan$revol_util <- as.numeric(newsub_preclean_loan$revol_util)

# Checking for other issues
summary(newsub_preclean_loan)

# As we can see, many of a few columns are integers but these should be factors and there must not be any mathematical operation done on these columns
# Thus converting them to factors
newsub_preclean_loan$id <- factor(newsub_preclean_loan$id)
newsub_preclean_loan$member_id <- factor(newsub_preclean_loan$member_id)
newsub_preclean_loan$grade <- factor(newsub_preclean_loan$grade)
newsub_preclean_loan$sub_grade <- factor(newsub_preclean_loan$sub_grade)
newsub_preclean_loan$term <- factor(newsub_preclean_loan$term)
newsub_preclean_loan$home_ownership <- factor(newsub_preclean_loan$home_ownership)
newsub_preclean_loan$verification_status <- factor(newsub_preclean_loan$verification_status)
newsub_preclean_loan$loan_status <- factor(newsub_preclean_loan$loan_status)
newsub_preclean_loan$issue_d <- factor(newsub_preclean_loan$issue_d)
newsub_preclean_loan$pub_rec_bankruptcies <- factor(newsub_preclean_loan$pub_rec_bankruptcies)

# Check for blank values
sapply(newsub_preclean_loan, function(x) length(which(x == "")))

# Discarding 'next_pymnt_d' column as more than 90% of its values are blank
newsub_preclean_loan <- subset(newsub_preclean_loan, select = -c(next_pymnt_d))

## Renaming column names to make them more intutive/self explanatory
newsub_preclean_loan<-dplyr::rename(newsub_preclean_loan,loan_amount=loan_amnt,funded_amount=funded_amnt,funded_amount_investor=funded_amnt_inv,interest_rate=int_rate,employment_tenure=emp_length,job_title=emp_title,annual_income=annual_inc,issued_month_year=issue_d,loan_description=desc,loan_title=title,month_since_last_delinq=mths_since_last_delinq,months_since_last_record=mths_since_last_record,revolving_balance=revol_bal,revolving_utilisation_rate=revol_util)

## Extracting Month from the issued_month_year column
newsub_preclean_loan$issued_month<-substr(newsub_preclean_loan$issued_month_year,1,3)
unique(newsub_preclean_loan$issued_month)

## Extracting Year from the issued_month_year column
newsub_preclean_loan$issued_year<-2000+as.numeric(substr(newsub_preclean_loan$issued_month_year,5,7))
unique(newsub_preclean_loan$issued_year)

summary(newsub_preclean_loan$interest_rate)
# Categorizing Interest Rate into three buckets
newsub_preclean_loan$interest_bucket = ifelse(newsub_preclean_loan$interest_rate <= 8, "Low", 
                                            ifelse(newsub_preclean_loan$interest_rate <= 16,"Medium", "High"))

#summary(newsub_preclean_loan$dti)
# Categorizing dti into five buckets
newsub_preclean_loan$dti_bucket = ifelse(newsub_preclean_loan$dti <= 8, "Low", 
                           ifelse(newsub_preclean_loan$dti <= 13,"Low to Medium",
                                  ifelse(newsub_preclean_loan$dti <= 19,"Medium", "High")))

# Checking for the distribution of annual income to bucketize
summary(newsub_preclean_loan$annual_income)
newsub_preclean_loan$income_bucket = ifelse(newsub_preclean_loan$annual_income <= 40000, "<=40000", 
                                              ifelse(newsub_preclean_loan$annual_income <= 60000,"40000 to 60000", 
                                                     ifelse(newsub_preclean_loan$annual_income <= 80000,"60000 to 80000", ">80000")))

sub_clean_loan<-newsub_preclean_loan
dim(sub_clean_loan)
View(sub_clean_loan)

summary(sub_clean_loan$loan_status)
summary(sub_clean_loan$income_bucket)
summary(sub_clean_loan$home_ownership)
summary(sub_clean_loan$grade)
summary(sub_clean_loan$sub_grade)
summary(sub_clean_loan$purpose)
summary(sub_clean_loan$dti_bucket)

# Export dataset for univariate & bi-variate analysis in tableau
write.csv(sub_clean_loan, "G:/PGDDS/Course 2 - Statistics and Exploratory Data Analytics/8. EDA Case Study/sub_clean_loan.csv")

#Unitvariate analysis for Income Bucket of all loan statuses
ggplot(sub_clean_loan, aes(x = as.factor(income_bucket), fill = loan_status)) + geom_bar(position = "fill") + xlab("Income Bucket") + ylab("% of loans") + scale_y_continuous(labels = percent)
# Insight: 18% of loan defaulters have annual income <= 40,000

#Analysis for Income bucket and home ownership for Charged off loan status
ggplot(sub_clean_loan[sub_clean_loan$loan_status=='Charged Off',], aes(x = as.factor(income_bucket), fill = home_ownership)) + geom_bar(position = "fill") + xlab("Income Bucket") + ylab("% of loans") + scale_y_continuous(labels = percent)
# Insight: 67% of loan defaulters in the above category have rented homes

#Analysis for Grade of all loan statuses
ggplot(sub_clean_loan, aes(x = as.factor(grade), fill = loan_status)) + geom_bar(position = "fill") + xlab("Grade") + ylab("% of loans") + scale_y_continuous(labels = percent)
# Insight: As the grade moves from A to G, the percentage of defaulting increases from 6% (in A) to 32% (in G)

#Analysis for Sub-Grade of all loan statuses
ggplot(sub_clean_loan, aes(x = as.factor(sub_grade), fill = loan_status)) + geom_bar(position = "fill") + xlab("Sub-Grade") + ylab("% of loans") + scale_y_continuous(labels = percent)
# Insight: F5 has highest default rate - 46%
# Insight: Sub-grades in G have higher default rate in sub-grades in A
# Insight: Observed one outlier - default rate of  F5(46%) > G1(30%)

#Analysis for Loan Purpose of all loan statuses
ggplot(sub_clean_loan, aes(x = as.factor(purpose), fill = loan_status)) + geom_bar(position = "fill") + xlab("Loan Purpose") + ylab("% of loans") + scale_y_continuous(labels = percent)
# Insight: Small business has the highest default rate of 26%
# Insight: Other category has 16% default rate

#Analysis for DTI of all loan statuses
ggplot(sub_clean_loan, aes(x = as.factor(dti_bucket), fill = loan_status)) + geom_bar(position = "fill") + xlab("DTI Bucket") + ylab("% of loans") + scale_y_continuous(labels = percent)
# Insight: The chances of loan default increases marginally as the DTI increases.
# Insight: Charged off percentage for Low DTI - 12% and High DTI - 16%
