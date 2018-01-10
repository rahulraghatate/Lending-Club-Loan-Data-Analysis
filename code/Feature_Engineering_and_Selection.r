library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(vcd)
library(lattice)
library(dplyr)
library(lubridate)
library(rpart)
library(randomForest)

data = read_csv("C:/Users/siddt/Documents/EDA/Final/lending-club-loan-data/loan.csv")
dim(data)
dataFrame = data.frame(data)

#replacing with zero
dataFrame$emp_length[is.na(dataFrame$emp_length)] = 0
dataFrame$mths_since_last_delinq[is.na(dataFrame$mths_since_last_delinq)] = 0
dataFrame$mths_since_last_record[is.na(dataFrame$mths_since_last_record)] = 0
dataFrame$mths_since_last_major_derog[is.na(dataFrame$mths_since_last_major_derog)] = 0

#more than 50% NA columns are removed
dataFrame = dataFrame[, -which(colMeans(is.na(dataFrame)) > 0.5)]

#issue date and issue date year
dataFrame$issue_d = mdy(dataFrame$issue_d)
dataFrame = dataFrame %>% mutate(issue_d_year = year(issue_d)) 

#defaulted
defaulted = c("Default", "Does not meet the credit policy. Status:Charged Off", "In Grace Period", "Late (16-30 days)", "Late (31-120 days)")
dataFrame = dataFrame %>% mutate(default = ifelse(!(loan_status %in% defaulted), 0, 1))

#credit history length
dataFrame$earliest_cr_line = mdy(dataFrame$earliest_cr_line)
dataFrame$credit_age = as.numeric(difftime(Sys.Date(),dataFrame$earliest_cr_line, units = 'days'))
dataFrame$credit_age[dataFrame$credit_age < 0 | is.na(dataFrame$credit_age)] = 0

#grade_inq
dataFrame$grade_inq = cut(dataFrame$inq_last_6mths,breaks = c(-Inf,0,2,6,10,Inf), labels=c("A","B","C","D","E"))

#removing columns that don't matter
#purpose has 13 classes, ulike title which has 60000+. A lot of these would be variations of the same thing, and need grouping
#so adding title to the rempove list, and keeping only purpose
#only 52 cases, so adding funded_amnt_inv to remove list
#similar scene with out_prncp_inv and total_pymnt_inv, funded_amt (is the same as loan amt)
#length(dataFrame[dataFrame$funded_amnt != dataFrame$funded_amnt_inv,])
#policy code has only one value, so removing it
#sub-grade has too many classes and by its nature influences predictions too much - so removing it
dataFrame = within(dataFrame,rm("loan_status","inq_last_6mths","url","emp_title","sub_grade","id","member_id","funded_amnt","last_credit_pull_d","earliest_cr_line","next_pymnt_d","funded_amnt_inv","last_pymnt_d","title","out_prncp_inv","total_pymnt_inv","policy_code"))

#dropping rows with NAs for important columns
dataFrame = dataFrame[!is.na(dataFrame$delinq_2yrs),]
dataFrame = dataFrame[!is.na(dataFrame$revol_util),]
dataFrame = dataFrame[!is.na(dataFrame$tot_coll_amt),]

#fixing home ownership and zipcode
dataFrame$home_ownership[dataFrame$home_ownership == 'NONE' | dataFrame$home_ownership == 'ANY'] = 'OTHER'
dataFrame$zip_code = as.numeric(gsub("\\D+", "", dataFrame$zip_code))

#character variables that need to be numeric
to_numeric = c("tot_cur_bal", "tot_coll_amt", "total_rev_hi_lim", "mths_since_last_major_derog")
dataFrame = dataFrame %>% mutate_at(.funs = funs(as.numeric), .vars = to_numeric)

#variables that need to be made factors
to_factors = c("default","grade","application_type", "issue_d_year", "initial_list_status", "term", "emp_length", "addr_state", "purpose", "home_ownership","pymnt_plan","verification_status")
dataFrame = dataFrame %>% mutate_at(.funs = funs(as.factor), .vars = to_factors)

#checking correlations of numeric features
#c("installment","total_acc","total_pymnt","out_prncp","collection_recovery_fee","total_rev_hi_lim")
nums = sapply(dataFrame, is.numeric)
numeric_features = dataFrame[,nums]
correlation = data.frame(round(cor(numeric_features, use = "complete.obs"),2))

# #checking correlations of non numeric features
# nonnumeric_features = dataFrame[,!nums]
# correlation2 = data.frame(round(cor(nonnumeric_features, use = "complete.obs"),2))
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
important_variables_c = c("mths_since_last_record","dti","purpose","pub_rec","open_acc","total_rec_late_fee","initial_list_status","grade","credit_age")

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
important_variables_r = c("int_rate","term","total_rec_int","total_rec_prncp","loan_amnt","issue_d","last_pymnt_amnt","issue_d_year","annual_inc","revol_bal","verification_status","recoveries","home_ownership","tot_cur_bal","revol_util")

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


#feature selection
#we now retain only 28 of the original variables (including the ones that were newly engineered)
important_variables = append(important_variables_c,important_variables_r)
important_variables = append(important_variables,c("default","grade_inq"))
dataFrame.final = dataFrame[,important_variables]



# ### RF model for predicting grade
# 
# #train test split
# set.seed(1)
# sample_size = floor(0.65 * nrow(dataFrame))
# index = sample(seq_len(nrow(dataFrame)), size = sample_size)
# train = dataFrame.final[index, ]
# test = dataFrame.final[-index, ]
# x_test = test[,-c(8)]
# y_test = test$grade
# 
# #model
# #predict_grade = randomForest(grade ~ ., data = train, ntree = 100, nodesize = 50, importance = TRUE)
# predict_grade = randomForest(grade ~ loan_amnt + term + verification_status + annual_inc + open_acc + grade_inq + credit_age + dti + initial_list_status, data = train, ntree = 100, nodesize = 50, importance = TRUE)
# 
# #accuracy
# predictions = predict(predict_grade, x_test, type = 'response')
# results = data.frame(cbind(y_test, predictions))
# accuracy = nrow(results[results$y_test == results$predictions,])/length(y_test)