#Project: ALY6080: TBI Data Modelling
#Author : Divya Pai, Mehak Gagneja, Naina Gupta, Shivi Jain


#
#Clean canvas ----
#clears the variables
rm(list=ls())
#clears the plots
dev.off()
#clears the console
cat("\014") 

install.packages("ROSE")
install.packages("eeptools")
install.packages("tree")
library(magrittr)
library(tidyr)
library(dplyr)
library(eeptools)
library(car)
library(rpart)
library(rpart.plot)
library(gbm)
library(caret)
library(ISLR)
library(randomForest)
library(sqldf)
library(ROSE)
library(reprtree)

# Set the file directory 
setwd("C:/Users/Divsy/Documents/MPS Analytics Courses/Qtr3/Experiential - ALY6080/To Submit/")

# Read the file
tbi_dataset <- read.csv("tbi_dataset_2.csv")

## Data Cleaning
#remove duplicates
tbi_dataset <- tbi_dataset %>% distinct() 


#replace missing values with NA
tbi_dataset[tbi_dataset == ''] <- NA
#show percentage of NA's per column 
round((colMeans(is.na(tbi_dataset)))*100,2)

#replace NA values in column 
tbi_dataset$user_type <- replace_na(tbi_dataset$user_type, 'other')
tbi_dataset$referral_group <- replace_na(tbi_dataset$referral_group, 'other') 
tbi_dataset$veteran <- replace_na(tbi_dataset$veteran, 'other') 
tbi_dataset$race <- replace_na(tbi_dataset$race, 'other') 
tbi_dataset$category <- replace_na(tbi_dataset$category, 'other') 
tbi_dataset$subcategory <- replace_na(tbi_dataset$subcategory, 'Other') 
tbi_dataset$therapies <- replace_na(tbi_dataset$therapies, 'other') 
tbi_dataset$injury_from <- replace_na(tbi_dataset$injury_from, 'Other') 
tbi_dataset$head_hit_location <- replace_na(tbi_dataset$head_hit_location, 'Other') 



# function to calculate mode
calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}


# replace missing total_tbi with mode of the column
calc_mode(tbi_dataset$total_tbi)
tbi_dataset$total_tbi <- replace_na(1) 



#change date types
tbi_dataset$date_of_birth <- as.Date(tbi_dataset$date_of_birth, format = '%m/%d/%Y')
tbi_dataset$tbi_incident_date <- as.Date(tbi_dataset$tbi_incident_date,format = '%m/%d/%Y')

#add column for patient age
tbi_dataset$age <- as.integer(as.Date(Sys.Date()) - tbi_dataset$date_of_birth) %/% 365.25
#add column for age_during_incident
tbi_dataset$age_during_incident <- as.integer(tbi_dataset$tbi_incident_date - tbi_dataset$date_of_birth) %/% 365.25


round((colMeans(is.na(tbi_dataset)))*100,2)

str(tbi_dataset)


#### Data Modelling ####
#select only tbiPatients
tbi_data <- sqldf('select referral_group,veteran,race,category,subcategory,injury_from,head_hit_location,age,gender 
                  from tbi_dataset 
                  where patient_type="tbiPatient" 
                  and user_type="tbiPatient"
                  and gender in ("male","female")')
tbi_data$gender <- as.factor(tbi_data$gender)
# tbi_data$patient_type <- as.factor(tbi_data$patient_type)
# tbi_data$user_type <- as.factor(tbi_data$user_type)
tbi_data$referral_group <- as.factor(tbi_data$referral_group)
tbi_data$veteran <- as.factor(tbi_data$veteran)
tbi_data$race <- as.factor(tbi_data$race)
tbi_data$category <- as.factor(tbi_data$category)
tbi_data$subcategory <- as.factor(tbi_data$subcategory)
#tbi_data$therapies <- as.factor(tbi_data$therapies)
tbi_data$injury_from <- as.factor(tbi_data$injury_from)
tbi_data$head_hit_location <- as.factor(tbi_data$head_hit_location)


## Split the data
set.seed(100) 
#create indexing for 70% of the data 
trainIndex <- createDataPartition(tbi_data$gender, p = 0.7, list = FALSE, times = 1)
train <- tbi_data[ trainIndex,] #70% of data
test <- tbi_data[-trainIndex,] #30% of data

#### Random Forest Model1 ####
RFM <- randomForest(gender ~ ., 
                    data = train)
#view summary of model
RFM


#Confusion matrix- test 
#make predictions in test data
probabilities.test.f <- predict(RFM, test)
#confusion matrix to measure model accuracy
confusionMatrix(probabilities.test.f, test$gender)


#### Random Forest Model2 ####
fit.cv <- train(gender ~., data=train, method="rf")
pred <- predict(fit.cv, test)
confusionMatrix(pred, test$gender)


plot(varImp(fit.cv))


#### Decision Tree Model ####
DT <- rpart(gender~ ., data = train, method = "class")
model3.pred <- predict(DT, test, type='class')
# Generating Confusion Matrix
confusionMatrix(model3.pred, test$gender)
rpart.plot(DT, extra = 106)


#### Decision Tree Model2 ####
fit.cv2 <- train(gender ~., data=train, method="rpart")
pred2 <- predict(fit.cv2, test)
confusionMatrix(pred2, test$gender)
plot(varImp(fit.cv2))

