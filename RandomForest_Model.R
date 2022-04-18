#clean environment, set seed to 5082
rm(list=ls())
set.seed(5082)

#load libraries
library(randomForest)
library(tidyr)
#Read in data and get data frame ready
train<- read.csv("space_train.csv", sep=',', stringsAsFactors = TRUE)
#remove columns don't need: PassengerId, Cabin, Name
train<-subset(train, select=-c(PassengerId,Cabin,Name)) 
#Replaced NAs with the average value in Age, RoomService, FoodCourt, ShoppingMall, Spa, and VRDeck
train$Age[is.na(train$Age)] <- mean(train$Age, na.rm = TRUE)
train$RoomService[is.na(train$RoomService)] <- mean(train$RoomService, na.rm = TRUE)
train$FoodCourt[is.na(train$FoodCourt)] <- mean(train$FoodCourt, na.rm = TRUE)
train$ShoppingMall[is.na(train$ShoppingMall)] <- mean(train$ShoppingMall, na.rm = TRUE)
train$Spa[is.na(train$Spa)] <- mean(train$Spa, na.rm = TRUE)
train$VRDeck[is.na(train$VRDeck)] <- mean(train$VRDeck, na.rm = TRUE)

summary(train)
#omit NAs
train<-na.omit(train)

#Train-test split using the train data
n <- nrow(train)
trainIndex <- sample(1:n, size = 0.7*n)   ## split train data set since we don't have labels for the test set

#inspect structure of df
str(train)
summary(train)

#split train/test sets
train.x <- train[trainIndex,]
test.x <- train[-trainIndex,]
train.y<-train$Transported[trainIndex]
test.y<-train$Transported[-trainIndex]

#build RF model using train data set
rf <- randomForest(Transported~.,
                   data=train.x,
                   mtry=2,
                   ntree=500,
                   importance=TRUE)
print(rf)
#tune mtry
best.mtry <- tuneRF(train.x,train.y, ntree=500,
                    mtryStart = 1, 
                    stepFactor=1.5,improve=0.01)

print(best.mtry)
#use best.mtry to build final RF model
best.rf<-randomForest(Transported~.,
                      data=train.x,
                      mtry=best.mtry,
                      ntree=500,
                      importance=TRUE)
#predict test.y
yhat<-predict(best.rf, 
              newdata=test.x)

str(yhat)
head(yhat)
#set prediction as factors
pred<-as.factor(ifelse(yhat=="False", "False", "True"))
head(pred)
#inspect importance of each feature
varImpPlot(rf) 
#construct confusion matrix
confusion<-table(pred,test.y)
#output test accuracy and test error rate
acc<-(confusion[1]+confusion[4])/(confusion[1]+confusion[2]+confusion[3]+confusion[4])
err<-1-acc
print(acc)
print(err)

