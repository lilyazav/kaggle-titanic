library(ggplot2)
library(dplyr)

load_clean_data <- function (){
  train <- read.csv("./data/train.csv")
  test <- read.csv("./data/test.csv")
  
  # Make blank values NAs
  train$Cabin = replace(train$Cabin, train$Cabin == "", NA)
  train$Embarked = replace(train$Embarked, train$Embarked == "", NA)
  
  # make the target variable a factor, as well as other vars that are factors
  train$Survived <- as.factor(train$Survived)
  train$Pclass <- as.factor(train$Pclass)
  train$Sex <- as.factor(train$Sex)
  train$Embarked <- as.factor(train$Embarked)
  
  test$Pclass <- as.factor(test$Pclass)
  test$Sex <- as.factor(test$Sex)
  test$Embarked <- as.factor(test$Embarked)
  
  # Dealing with NAs.
  # can simply replace Age NAs with mean
  train$Age <- train$Age %>% replace_na(mean(c(train$Age, test$Age), na.rm =T))
  test$Age <- test$Age %>% replace_na(mean(c(train$Age, test$Age), na.rm =T))
  
  # For embarked, 2 NAs in train only. Replace with mode
  train$Embarked <- train$Embarked %>% replace_na("S")
  
  # For fare, 1 na in test only. Replace with mean
  test$Fare <- test$Fare %>% replace_na(mean(c(train$Fare, test$Fare), na.rm = T))
  
  test <<- test
  train <<- train
}



