library(caret) #http://topepo.github.io/caret/index.html
library(data.table)
library(Metrics)


train<-fread('./project/volume/data/interim/Stat_380_train2022.csv')
test<-fread('./project/volume/data/interim/Stat_380_test2022.csv')

train_y<-train$SalePrice
test$SalePrice <- 0

#combine together

master<-rbind(train,test)

#convert string data into numerical data

dummies <- dummyVars(SalePrice ~ ., data = master)
train<-predict(dummies, newdata = train)
test<-predict(dummies, newdata = test)

#reformat after dummyVars and add back response Var

train<-data.table(train)
train$SalePrice<-train_y
test<-data.table(test)


#fit a linear model with selected features
#Using LotArea, YearBuilt, GrLiveArea, OverallCond and OverallQual as selected features because there are no null values among them. Besides, t values of them are low to make prediction.
lm_model<-lm(SalePrice ~ LotArea + OverallQual + YearBuilt + GrLivArea + OverallCond ,data=train)


#assess model
summary(lm_model)


#save model
saveRDS(dummies,"./project/volume/models/SalePrice_lm.dummies")
saveRDS(lm_model,"./project/volume/models/SalePrice_lm.model")

test$SalePrice<-predict(lm_model,newdata = test)

#our file needs to follow the example submission file format. So we need to only have the Id and saleprice column and
#we also need the rows to be in the correct order

submit<-test[,.(Id, SalePrice)]

#now we can write out a submission
fwrite(submit,"./project/volume/data/processed/submit_lm.csv")


