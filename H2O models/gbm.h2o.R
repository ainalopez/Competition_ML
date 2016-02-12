rm(list = ls())

library(h2o)

#Load data: text mining variables included
setwd("~/BGSE/Kaggle Competition/data/Best Data")
train <- read.csv("train.csv")
test <- read.csv("testing.csv")

train[,61]<-as.factor(train[,61])

# Start H2O
h2o.init(nthreads = -1)
train.h <- as.h2o(train,destination_frame = "train.h")
test.h <- as.h2o(test,destination_frame = "test.h")


#Variables Selection
set.seed(2016)
inTrain<-sample(30000,20000)
model.gbm <- h2o.gbm(y = 61, x = c(1:60, 62:134), training_frame = train.h,
          ntrees = 500, distribution= "AUTO")

var.imp<-h2o.varimp(model.gbm)
big.scale<-which(var.imp$scaled_importance>0.15)
best.var<-var.imp$variable[big.scale]

#best variables:
best.var<-c(2, 28, 39, 26,  1, 44, 25, 42, 63, 29, 31, 40, 27,  4,  3,  7, 45, 51, 47, 12, 54, 46, 41, 43, 5,
17, 13, 48, 8, 18, 15, 64, 58, 59, 57, 22, 52, 49, 10)

#Model & Cross-Validation

k<-4
acc<-rep(NA,k)
set.seed(2016)
group<-sample(1:k, 30000, replace=TRUE)

num.trees<-c(10,20, 30, 40, 50, 60, 70, 80, 100)
mean.acc<-rep(NA,length(num.trees) )

for(j in 1:length(num.trees)){

 acc<-rep(NA,k)
for( i in 1:k){

  #Model
    model.gbm <- h2o.gbm(y = 61, x=best.var, training_frame = train.h[which(group!=i),],
          ntrees = num.trees[j], distribution= "AUTO")

  #Prediction
  pred <- h2o.predict(model.gbm, newdata = train.h[which(group==i),])
  pred<-as.data.frame(pred)
  pred<-as.integer(pred[,1])
  real<-train$popularity[group==i]
  
  acc[i]<-mean(pred==real)
  print(i)
 
}
 mean.acc[j]<-mean(acc)
 print(mean.acc)
}

> mean.acc
[1] 0.5121350 0.5160436 0.5188079 0.5199115 0.5205442 0.5210782 0.5196758
[8] 0.5183718 0.5176665

plot(num.trees,mean.acc, t="b")
#best choice: 60 trees


var.impo<-h2o.varimp(model.gbm)


#Model GBM for kaggle
model.gbm <- h2o.gbm(y = 61, x = best.var, training_frame = train.h,
          ntrees = 60, distribution= "AUTO")
#Prediction
pred <- h2o.predict(model.gbm, newdata = test.h)
pred<-as.data.frame(pred)
pred<-pred[,1]

#Save predictions 
solution <- data.frame(Id = test$id, popularity = as.numeric(pred))
write.csv(solution, file = "predictions_GBM_H2O_60trees.csv", row.names = FALSE)

