#install.packages(c("ggplot2", "readr", "vcd", "lattice", "dplyr", "lubridate", "DAAG", "earth", "caret"))
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(vcd)
library(lattice)
library(dplyr)
library(lubridate)
library(DAAG)
library(earth)
library(caret)
library(stringr)
#########################################################################################
#setwd("C:/Drives/Box Sync/EDA Projects/Final Project/code")
data<-read_csv("../data/loan_orig.csv")
#Assign the data to a dataframe
na_count_orig <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count_orig <- data.frame(na_count_orig)
View(na_count_orig)

#Convert input to dataframe
dataFrame = data.frame(data)
#########################################################################################

#replacing with zero
dataFrame$emp_length[is.na(dataFrame$emp_length)] <- 0
dataFrame$mths_since_last_delinq[is.na(dataFrame$mths_since_last_delinq)] <- 0
dataFrame$mths_since_last_record[is.na(dataFrame$mths_since_last_record)] <- 0
dataFrame$mths_since_last_major_derog[is.na(dataFrame$mths_since_last_major_derog)] <- 0

#more than 50% NA columns are removed
dataFrame = dataFrame[, -which(colMeans(is.na(dataFrame)) > 0.5)]

#dropping rows with NAs for important columns
dataFrame = dataFrame[!is.na(dataFrame$delinq_2yrs),]
dataFrame = dataFrame[!is.na(dataFrame$revol_util),]
# dataFrame = dataFrame[!is.na(dataFrame$tot_coll_amt),]

#fixing home ownership and zipcode
dataFrame$home_ownership[dataFrame$home_ownership == 'NONE' | dataFrame$home_ownership == 'ANY'] = 'OTHER'
dataFrame$zip_code = as.numeric(gsub("\\D+", "", dataFrame$zip_code))

#issued_date
dataFrame$issue_d <- mdy(dataFrame$issue_d)

#defaulted
defaulted = c("Default", "Does not meet the credit policy. Status:Charged Off", "In Grace Period", "Late (16-30 days)", "Late (31-120 days)")
dataFrame = dataFrame %>% mutate(default = ifelse(!(loan_status %in% defaulted), 0, 1))

#For Plot
dataFrame %>% group_by(loan_status) %>% summarise(count = n())
dataFrame = dataFrame %>% mutate(default = ifelse(!(loan_status %in% defaulted), "No Default", "Default"))
tmp = dataFrame %>% group_by(default) %>% summarise(ncount = n())
tmp$ncount = 100 * tmp$ncount/nrow(dataFrame)
tmp$ncount_p = str_c(round(tmp$ncount,2),"%")
ggplot(tmp,aes(x=default,y=ncount,fill=default)) + geom_bar(stat="identity") + geom_text(aes(label=ncount_p),vjust = 2)

#issued_year
dataFrame <- dataFrame %>% mutate(issue_d_year = as.numeric(year(issue_d))) 

#credit history length
dataFrame$earliest_cr_line <- mdy(dataFrame$earliest_cr_line)
dataFrame$credit_age = as.numeric(difftime(Sys.Date(),dataFrame$earliest_cr_line, units = 'days'))
dataFrame$credit_age[dataFrame$credit_age < 0 | is.na(dataFrame$credit_age)] = 0
dataFrame$credit_age<-as.numeric(round(dataFrame$credit_age/365))

#grade_inq
dataFrame$grade_inq = cut(dataFrame$inq_last_6mths,breaks = c(-Inf,0,2,6,10,Inf), labels=c("A","B","C","D","E"))

dataFrame = mutate(dataFrame, purpose = ifelse(purpose == "credit_card" | purpose == "debt_consolidation"| purpose == "educational", "debt",
                                               ifelse(purpose == "car" | purpose == "major_purchase" | purpose == "vacation" | purpose == "wedding" | purpose == "medical" | purpose == "other", "purchase",
                                                      ifelse(purpose == "house" | purpose == "home_improvement" | purpose == "moving" | purpose == "renewable_energy", "purchase",
                                                             purpose))))
#character variables that need to be numeric
to_numeric = c("tot_cur_bal", "tot_coll_amt", "total_rev_hi_lim", "mths_since_last_major_derog")
dataFrame = dataFrame %>% mutate_at(.funs = funs(as.numeric), .vars = to_numeric)

#variables that need to be made factors
to_factors = c("grade","application_type","issue_d","issue_d_year","initial_list_status", "term", "emp_length", "addr_state", "purpose", "home_ownership","loan_status","pymnt_plan","verification_status","default")
dataFrame = dataFrame %>% mutate_at(.funs = funs(as.factor), .vars = to_factors)
#########################################################################################
#removing columns that don't matter
#purpose has 13 classes, ulike title which has 60000+. A lot of these would be variations of the same thing, and need grouping
#so adding title to the rempove list, and keeping only purpose
#only 52 cases, so adding funded_amnt_inv to remove list
#similar scene with out_prncp_inv and total_pymnt_inv, funded_amt (is the same as loan amt)
#length(dataFrame[dataFrame$funded_amnt != dataFrame$funded_amnt_inv,])
#policy code has only one value, so removing it
#sub-grade has too many classes and by its nature influences predictions too much - so removing it
# loan_status
# inq_last_6mths

#removing variables based on intuitive analysisi of data dictionary and data
dataFrame = within(dataFrame,rm("url","emp_title","sub_grade","id","member_id","funded_amnt","last_credit_pull_d","earliest_cr_line","next_pymnt_d","funded_amnt_inv","last_pymnt_d","title","out_prncp_inv","total_pymnt_inv","policy_code","inq_last_6mths","loan_status"))

#checking correlations of numeric features
#c("installment","total_acc","total_pymnt","out_prncp","collection_recovery_fee","total_rev_hi_lim")
nums = sapply(dataFrame, is.numeric)
numeric_features = dataFrame[,nums]
correlation = data.frame(round(cor(numeric_features, use = "complete.obs"),2))

#removing more variables based on high correlation
dataFrame = within(dataFrame,rm("installment","total_acc","out_prncp","collection_recovery_fee","total_rev_hi_lim"))

#using classification trees with term as a target variable to look at variable importance
#this is done to further reduce the feature set size

#sampling only 50% of the data
# set.seed(1)
# sample_size = floor(0.65 * nrow(dataFrame))
# index = sample(seq_len(nrow(dataFrame)), size = sample_size)
# subset = dataFrame[index, ]
# 
# tree_df = subset[,-c(3,1,8,25,26)]
# tree_df = dataFrame[,-c(5,3,27,2,1,9,24)]
# classification_tree = rpart(term ~ ., data = tree_df, method = 'class', minbucket = 200, cp = 0.01)
# summary(classification_tree)

#removing variables based on tree results
important_variables_c = c("mths_since_last_record","dti","purpose","pub_rec","open_acc","total_rec_late_fee","initial_list_status","grade","credit_age")

#Feature Importance in Desc order 
# last_pymnt_amnt verification_status         loan_status         total_pymnt         tot_cur_bal initial_list_status 
# 45                  20                  10                  10                   6                   3 
# annual_inc             issue_d        issue_d_year 
# 2                   1                   1 

# verification_status             revol_util                purpose         inq_last_6mths              revol_bal               open_acc 
# 28                     25                     22                     13                      5                      1 
# dti mths_since_last_record                issue_d            tot_cur_bal                pub_rec        last_pymnt_amnt 
# 1                      1                      1                      1                      1                      1 

# grade     last_pymnt_amnt         loan_status         tot_cur_bal             purpose          revol_util 
# 43                  28                   6                   4                   4                   3 
# annual_inc initial_list_status             issue_d      inq_last_6mths      home_ownership        issue_d_year 
# 2                   2                   1                   1                   1                   1 
# dti 
# 1 

#using regression trees with interest rate as a target variable to look at variable importance
#this is done to further reduce the feature set size
#tree_df = dataFrame[,-c(4,5, 37, 36, 26, 23)]
#regression_tree = rpart(int_rate ~ ., data = tree_df, method = 'anova', minsplit = 750, cp = 0.005)
#summary(regression_tree)
important_variables_r = c("term","total_rec_int","total_rec_prncp","loan_amnt","issue_d","last_pymnt_amnt","issue_d_year","annual_inc","revol_bal","verification_status","recoveries","home_ownership","tot_cur_bal","revol_util")

# term       total_rec_int     total_rec_prncp           loan_amnt             issue_d     last_pymnt_amnt 
# 25                  23                  13                  11                   7                   6 
# issue_d_year          annual_inc      inq_last_6mths           revol_bal         loan_status verification_status 
# 5                   3                   2                   1                   1                   1 
# revol_util         tot_cur_bal 
# 1                   1 

# term       total_rec_int           loan_amnt     last_pymnt_amnt             issue_d          annual_inc 
# 33                  24                  15                   9                   6                   5 
# inq_last_6mths         loan_status verification_status          revol_util      home_ownership                 dti 
# 3                   2                   1                   1                   1                   1 
# recoveries 
# 1 
#########################################################################################

#Creating Final Feature set
important_variables = append(important_variables_c,important_variables_r)
important_variables = append(important_variables,c("int_rate","default","grade_inq"))
dataFrame.final = dataFrame[,important_variables]

na_count_rd <-sapply(dataFrame.final, function(y) sum(length(which(is.na(y)))))
na_count_rd <- data.frame(na_count_rd)
View(na_count_rd)
dim(dataFrame.final)
#########################################################################################
##Initial Regression on 25 imp variables
#reg_data<-dataFrame.final[,c("pub_rec","total_rec_late_fee","issue_d_year","home_ownership","default","grade","total_rec_int","total_rec_prncp","purpose","last_pymnt_amnt","loan_amnt","annual_inc","revol_util","term","grade_inq","initial_list_status","revol_bal","credit_age","verification_status","tot_cur_bal","open_acc","recoveries","dti","mths_since_last_record","int_rate")]
#reg_data[,-c(25)]<-predict(preProcess(reg_data[,-c(25)],method=c("center","scale")), reg_data[,-c(25)])
#str(reg_data)

#purpose_dummy<-data.frame(predict(dummyVars("~ purpose",reg_data),reg_data))
#grade_inq_dummy<-data.frame(predict(dummyVars("~ grade_inq",reg_data),reg_data))
#reg_data<-cbind(reg_data,purpose_dummy,grade_inq_dummy)
#reg_data<-within(reg_data,rm("grade_inq.E","purpose.educational"))

#########################################################################################
#We ran series of models removing the features at every step based on metrics for feature elimination 
#Below is one of the intermdiate model
#########################################################################################

#One of the Intermediate Regression on 19 imp variables
reg_data<-dataFrame.final[,c("grade","total_rec_int","total_rec_prncp","purpose","last_pymnt_amnt","loan_amnt","annual_inc","revol_util","term","grade_inq","initial_list_status","revol_bal","credit_age","verification_status","tot_cur_bal","open_acc","recoveries","dti","mths_since_last_record","int_rate")]
reg_data[,-c(20)]<-predict(preProcess(reg_data[,-c(20)],method=c("center","scale")), reg_data[,-c(20)])
str(reg_data)
reg_data<-na.omit(reg_data)
names(reg_data)
#Data Split
set.seed(100)
smp_size <- floor(0.75 * nrow(reg_data))
train_ind <- sample(seq_len(nrow(reg_data)), size = smp_size)

train <- reg_data[train_ind, ]
test <- reg_data[-train_ind, ]
dim(train)
dim(test)
# lm()
reg_model1 <- lm(int_rate ~ .,train)
summary(reg_model1)
#Coefficients Confidence Intervals
confint(reg_model1)
#Checking Variance inflation factor for the variables
vifs<-data.frame(vif(reg_model1))
vifs

#Predictions on test set
pred <- predict(reg_model1, test)
actuals_preds <- data.frame(cbind(actuals=test$int_rate, predicteds=pred))  

head(actuals_preds,10)

#Correlation Accuracy
correlation_accuracy <- cor(actuals_preds)
#RMSE
rmse<-sqrt(mean(actuals_preds$predicteds - actuals_preds$actuals)^2)
#Min-Max Accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
#Mape
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)

#All results for INTERMEDIATE model 
correlation_accuracy
rmse
min_max_accuracy
mape

#par(mfrow = c(1, 2))
#plot(reg_model1$fitted, reg_model1$residuals, pch = 15, cex=0.5)
#plot(as.numeric(train$loan_amnt), reg_model1$residuals, pch = 15, cex=0.5)

png(filename="reg_model1_19_variables.png")
par(mfrow = c(2, 2))
plot(reg_model1)
dev.off()

#########################################################################################
#After few iteration of intermediate regression model we reached below final model 
#########################################################################################

# Linear Regression Model for 7 features based on data dictionary
regression_features<-c('grade','grade_inq','revol_util','dti','loan_amnt','mths_since_last_record','annual_inc','int_rate')
#'revol_util','default', 'revol_bal', 
reg_data<- dataFrame.final[,regression_features]
str(reg_data)
reg_data<-na.omit(reg_data)
reg_data[,-c(8)]<-predict(preProcess(reg_data[,-c(8)],method=c("center","scale")), reg_data[,-c(8)])
str(reg_data)
#train test split
set.seed(1)
smp_size <- floor(0.75 * nrow(reg_data))
train_ind <- sample(seq_len(nrow(reg_data)), size = smp_size)

train <- reg_data[train_ind, ]
test <- reg_data[-train_ind, ]
dim(train)
dim(test)
# lm()
reg_model2 <- lm(int_rate ~ .,train)

summary(reg_model2)
confint(reg_model2)

#Checking Variance inflation factor for the variables
vifs<-data.frame(vif(reg_model2))
vifs

#Correlation Accuracy
pred <- predict(reg_model2, test)
actuals_preds <- data.frame(cbind(actuals=test$int_rate, predicteds=pred))  
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

#RMSE
rmse<-sqrt(mean(actuals_preds$predicteds - actuals_preds$actuals)^2)
#Min-Max Accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)

#All results for Model 2
correlation_accuracy
rmse
min_max_accuracy
mape

#Plots
png(filename="reg_model_7_variables.png")
par(mfrow = c(2, 2))
plot(reg_model2)
dev.off()
####################################End##############################################
