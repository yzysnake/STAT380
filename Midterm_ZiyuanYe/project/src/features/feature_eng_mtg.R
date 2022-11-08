library(data.table)
library(caret)
library(Metrics)
library(glmnet)
library(plotmo)
library(lubridate)
library(zoo)


#read in data, notice the path will always look like this because the assumed working directory is the repo level folder
train<-fread("./project/volume/data/raw/Stat_380_train.csv")
test<-fread("./project/volume/data/raw/Stat_380_test.csv")
test$ic50_Omicron <- 0

##########################
# Prep Data for Modeling #
##########################

#combine together

master<-rbind(train,test,fill = TRUE)

#convert string data into numerical data

dummies <- dummyVars(ic50_Omicron ~ ., data = master)
train<-predict(dummies, newdata = train)
test<-predict(dummies, newdata = test)

#reformat after dummyVars and add back response Var

train<-data.table(train)
train$ic50_Omicron<-train_y
test<-data.table(test)


# fill NAs with mean values using zoo package
train <- na.aggregate(train)      
test <- na.aggregate(test)

# subset out only the columns to model

train <- train[, c('Id') := NULL]
test <- test[, c('Id') := NULL]


########################
# write out to interim #
########################

fwrite(train,"./project/volume/data/interim/train.csv")
fwrite(test,"./project/volume/data/interim/test.csv")
