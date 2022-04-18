## Remove Variables 
rm(list = ls())

## Load Libraries
#install.packages("xgboost")
#install.packages("data.table")
#install.packages("mlr")

library(xgboost)
library(caret)
library(gbm)
library(data.table)
library(mlr)

## Load Data
data <- read.csv("C:/Users/katya/Desktop/ML2/Group Project/spaceship-titanic/train.csv", stringsAsFactors = TRUE)
attach(data)

## Set Seed
set.seed(5082)

## Remove Passenger ID, Cabin, Name columns
#class(data)
data <- subset(data, select = -c(PassengerId, Cabin, Name))
#str(data)

## Replace NAs with Avg. Value in Age, RoomService, FoodCourt, ShoppingMall, Spa, VRDeck
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = TRUE)
data$RoomService[is.na(data$RoomService)] <- mean(data$RoomService, na.rm = TRUE)
data$FoodCourt[is.na(data$FoodCourt)] <- mean(data$FoodCourt, na.rm = TRUE)
data$ShoppingMall[is.na(data$ShoppingMall)] <- mean(data$ShoppingMall, na.rm = TRUE)
data$Spa[is.na(data$Spa)] <- mean(data$Spa, na.rm = TRUE)
data$VRDeck[is.na(data$VRDeck)] <- mean(data$VRDeck, na.rm = TRUE)

## Split data 70% train, 30% test
divideData <- createDataPartition(data$Transported, p = 0.7, list = FALSE)
train <- data[divideData,]
test<- data[-divideData,]

attach(train)
attach(test)

## Convert DF to DT
setDT(train)
setDT(test)

## One-Hot Encoding
train_label <- train$Transported
test_label <- test$Transported
new_train <- model.matrix(~. +0, data = train[, -c("Transported"), with = F])
new_test <- model.matrix( ~. +0, data = test[, -c("Transported"), with = F])

## Convert Factor to Numeric
train_label <- as.numeric(train_label) - 1
test_label <- as.numeric(test_label) - 1

## Prepare Matrix
dtrain <- xgb.DMatrix(data = new_train, label = train_label)
dtest <- xgb.DMatrix(data = new_test, label = test_label)

## Set Parameters
params <- list(booster = "gbtree", objective = "binary:logistic", eta = 0.3,
               gamma = 0, max_depth = 6, min_child_weight = 1,
               subsample = 1, colsample_bytree = 1)

## Make Model
xgbcv <- xgb.cv(params = params, data = dtrain, nrounds = 100, nfold = 5,
                showsd = T, stratified = T, print_every_n = 10,
                early.stop.round = 20, maximize = F)

xgb1 <- xgb.train(params = params, data = dtrain, nrounds = 16,
                  watchlist = list(val = dtest, train = dtrain), print_every_n = 10,
                  early_stopping_rounds = 10, maximize = F, eval_metric = "error")

## Make Preds
xgbpred <- predict(xgb1, dtest)
xgbpreds <- ifelse(xgbpred > 0.5, 1, 0)

## Confusion Matrix
CM <- table(pred = xgbpreds, label = test_label)

accuracy <- (CM[1] + CM[4]) / sum(CM)
print(accuracy)


error <- (CM[2] + CM[3]) / sum(CM)
print(error)
