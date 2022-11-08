library(caret) #http://topepo.github.io/caret/index.html
library(data.table)
library(Metrics)


train<-fread('./project/volume/data/interim/train_file.csv')
test<-fread('./project/volume/data/interim/test_file.csv')

train <- train[,.(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,result)]
test_withoutid <- test[,.(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10)]
train_y<-train$result


#fit a linear model
coinflip_model<-glm(result~ .,family=gaussian,data=train)


#assess model
summary(coinflip_model)

coef(coinflip_model)

#save model
saveRDS(coinfip_model,"./project/volume/models/coin_result_lm.model")

test$result<-predict(coinflip_model,newdata = test_withoutid,type="response")

#our file needs to follow the example submission file format. So we need to only have the Id and saleprice column and
#we also need the rows to be in the correct order

submit$result <- NULL
submit$id <- NULL
submit<-test[,.(id,result)]

#now we can write out a submission
fwrite(submit,"./project/volume/data/processed/submit6_coinflip.csv")


