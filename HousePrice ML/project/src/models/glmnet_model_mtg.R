library(data.table)
library(caret)
library(Metrics)
library(glmnet)
library(plotmo)
library(lubridate)
library(zoo)


#read in data, notice the path will always look like this because the assumed working directory is the repo level folder
train<-fread("./project/volume/data/interim/train.csv")
train_y<-train$SalePrice
train <- train[, c('SalePrice') := NULL]
test<-fread("./project/volume/data/interim/test.csv")
example_sub<-fread("./project/volume/data/interim/Stat_380_sample_submission.csv")


########################
# Use cross validation #
########################

train<-as.matrix(train)


test<-as.matrix(test)


gl_model<-cv.glmnet(train, train_y, alpha = 0.001,family="gaussian", nfolds = 20)


bestlam<-gl_model$lambda.min

####################################
# fit the model to all of the data #
####################################


#now fit the full model

#fit a logistic model
gl_model<-glmnet(train, train_y, alpha = 0.001,family="gaussian")

plot_glmnet(gl_model)

#save model
saveRDS(gl_model,"./project/volume/models/gl_model.model")

test<-as.matrix(test)

#use the full model
pred<-predict(gl_model,s=bestlam, newx = test)

bestlam
predict(gl_model,s=bestlam, newx = test,type="coefficients")
gl_model



#########################
# make a submision file #
#########################


#our file needs to follow the example submission file format.
#we need the rows to be in the correct order

example_sub$SalePrice<-pred


#now we can write out a submission
fwrite(example_sub,"./project/volume/data/processed/submit1.csv")

