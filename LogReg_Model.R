### Import Libraries ###
library(tidyverse)
library(caret)
library(randomForest)

rm(list = ls())
set.seed(5082)

### Read and summarize data ###
train <- read.csv("train.csv")
test <- read.csv("test.csv")

str(train)
summary(train)

### Convert "True" and "False" to 1 and 0 respectively on train and test data ### 
train$CryoSleep = as.integer(train$CryoSleep == "True")
train$VIP = as.integer(train$VIP == "True")
train$Transported = as.integer(train$Transported == "True")

test$CryoSleep = as.integer(test$CryoSleep == "True")
test$VIP = as.integer(test$VIP == "True")

### Replace NAs with Avg Value for Age, RoomSerivce, FoodCourt, ShoppingMall, Spa, VRDeck
train$Age[is.na(train$Age)] <- mean(train$Age, na.rm = TRUE)
train$RoomService[is.na(train$RoomService)] <- mean(train$RoomService, na.rm = TRUE)
train$FoodCourt[is.na(train$FoodCourt)] <- mean(train$FoodCourt, na.rm = TRUE)
train$ShoppingMall[is.na(train$ShoppingMall)] <- mean(train$ShoppingMall, na.rm = TRUE)
train$Spa[is.na(train$Spa)] <- mean(train$Spa, na.rm = TRUE)
train$VRDeck[is.na(train$VRDeck)] <- mean(train$VRDeck, na.rm = TRUE)
summary(train)

### Create Dummy Variables for HomePlanet
HPEarth <- ifelse(train$HomePlanet=="Earth",1,0)
HPEuropa <- ifelse(train$HomePlanet=="Europa",1,0)
HPMars <- ifelse(train$HomePlanet=="Mars",1,0)
train <- cbind(train, HPEarth,HPEuropa, HPMars)

### Create Dummy Variables for Destination
DCancri <- ifelse(train$Destination=="55 Cancri e", 1,0)
DPSO <- ifelse(train$Destination=="PSO J318.5-22",1,0)
DTrap <- ifelse(train$Destination=="TRAPPIST-1e",1,0)
train <- cbind(train, DCancri, DPSO, DTrap)

### Create Dummy Variables for Cabin - Deck/Num/Side - Port 1 Starboard 0
temp = train$Cabin %>% str_split(pattern="/", simplify=TRUE)
CabinDeck = temp[, 1] %>% as.factor()
CabinNum = temp[, 2] %>% as.integer()
CabinSide = temp[, 3] %>% str_replace(pattern="P", replace="1") %>% str_replace(pattern="S", replace="0") %>% as.integer()
train <- cbind(train, CabinDeck, CabinNum, CabinSide)

### Train/Test Split of Training Data
n <- nrow(train)
trainIndex <- sample(1:n, size = 0.7*n)   ## split train data set since we don't have labels for the test set

str(train)
summary(train)

### Remove Non-Numeric Columns and NAs
train <- train[-c(1,2,4,5,13)]
train <- na.omit(train)

train.train <- train[trainIndex,]
train.test <- train[-trainIndex,]

glm.fit <- glm(Transported~., data = train.train, family = binomial)
glm.pred <- predict(glm.fit, newdata=train.test, type="response")
for (i in 1:length(glm.pred)) {
  if(glm.pred[i] >= 0.5){
    glm.pred[i] = 1
  }
  else{
    glm.pred[i] = 0
  }
}

table(glm.pred, train.test$Transported)
mean(glm.pred == train.test$Transported)

### 80.588 Would be 175th Place!

