### Import Libraries ###
rm(list=ls())
library(tidyverse)
library(caret)
library(randomForest)


### Read and summarize data ###
train <- read.csv("train.csv")
train <- train[, -c(1,4,13)]  # solved logical subscript is too long error

str(train)
summary(train)


### Convert "True" and "False" to factors on train and test data ### 
train$CryoSleep = as.factor(train$CryoSleep == "True")
train$VIP = as.factor(train$VIP == "True")
train$Transported = as.factor(train$Transported == "True")

str(train)


### Adjust NAs to be the mean of each respective column for both train and test data
str(train)
summary(train)

train$Age[is.na(train$Age)] <- mean(train$Age, na.rm = TRUE)
train$RoomService[is.na(train$RoomService)] <- mean(train$RoomService, na.rm = TRUE)
train$FoodCourt[is.na(train$FoodCourt)] <- mean(train$FoodCourt, na.rm = TRUE)
train$ShoppingMall[is.na(train$ShoppingMall)] <- mean(train$ShoppingMall, na.rm = TRUE)
train$Spa[is.na(train$Spa)] <- mean(train$Spa, na.rm = TRUE)
train$VRDeck[is.na(train$VRDeck)] <- mean(train$VRDeck, na.rm = TRUE)
summary(train)


## Remove NAs
train <- na.omit(train)
summary(train)

## Tune SVM Model
library(e1071)
set.seed(5082)

n <- nrow(train)
trainIndex <- sample(1:n, size = 0.7*n)   ## split train data set since we don't have labels for the test set

str(train)
summary(train)

train.train <- train[trainIndex,]
train.test <- train[-trainIndex,]

tune.out <- tune(svm,
                 Transported ~ .,
                 data=train.train,
                 kernel="radial",
                 ranges = list(cost=c(0.05, 0.5, 1, 2), gamma = c(0.01, 0.1, 0.25, 0.5)))

tune.out
summary(tune.out)

bestSVM <- tune.out$best.model
ypred <- predict(bestSVM, train.test)
confusion <- table(ypred, train.test$Transported)
confusion

testAcc <- (confusion[1]+confusion[4])/(confusion[1]+confusion[2]+confusion[3]+confusion[4])
testAcc
# 0.7921779
testErr <- (confusion[2]+confusion[3])/(confusion[1]+confusion[2]+confusion[3]+confusion[4])
testErr
# 0.2078221



#######################
###### Results ########
#######################

# Radial kernel with costs of 0.05, 0.5, 1, 2 and gammas of 0.01, 0.1, 0.25, 0.5
  ## 0.7921779 test acc
  ## 0.2078221 test err

      ## Parameter tuning of 'svm':
      ## - sampling method: 10-fold cross validation
      ## 
      ## - best parameters:
      ##   cost gamma
      ## 2   0.1
      ## 
      ## - best performance: 0.21068


# Linear kernel with costs of 0.05, 0.5, 1, 2 and gammas of 0.01, 0.1, 0.25, 0.5
  ## 0.771089 test acc
  ## 0.228911 test err

      ## Parameter tuning of 'svm':
      ##   
      ##   - sampling method: 10-fold cross validation 
      ## 
      ## - best parameters:
      ##   cost gamma
      ## 2  0.01
      ## 
      ## - best performance: 0.2200507 


# Polynomial kernel with costs of 0.05, 0.5, 1, 2 and gammas of 0.01, 0.1, 0.25, 0.5
  ## 0.7860429 test acc
  ## 0.2139571 test err

      ## Parameter tuning of 'svm':
      ##   
      ##   - sampling method: 10-fold cross validation 
      ## 
      ## - best parameters:
      ##   cost gamma
      ## 2  0.25
      ## 
      ## - best performance: 0.2131463 


# Sigmoid kernel with costs of 0.05, 0.5, 1, 2 and gammas of 0.01, 0.1, 0.25, 0.5
  ## 0.7691718 test acc
  ## 0.2308282 test err

      ## Parameter tuning of 'svm':
      ##   
      ##   - sampling method: 10-fold cross validation 
      ## 
      ## - best parameters:
      ##   cost gamma
      ## 2  0.01
      ## 
      ## - best performance: 0.2279384 



