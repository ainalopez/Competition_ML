# 52.79 % in Kaggle
# 52.5% in my tests

require(xgboost)
library(rpart)
set.seed(1112)


#prediction <- predict(bstSparse, as.matrix(test.p))
#solution <- data.frame(Id = test$id, Popularity = as.numeric(prediction))
#write.csv(solution, file = "predictions_xgboost.csv", row.names = FALSE)
training.p11$Year <- as.numeric(training.p11$Year)
training.p11$Month <- as.numeric(training.p11$Month)
training.p11$comic <- as.numeric(training.p11$comic)
training.p11$day_of_month <- as.numeric(training.p11$day_of_month)


n <- nrow(training.p11)
fold <- sample(1:5, n, replace=TRUE)

acc <- rep (NA, 5)
cvpred <- matrix(NA,nrow=n ,ncol=ncol(training.p11))

ac <- rep(NA,5)
for (i in 1:5){
  
  bstSparse <- xgboost(data = as.matrix(training.p11[fold!=i, -c(1,54)]), label = training.p11[fold!=i, 54], max.depth = 2, 
                       eta = 0.25, num_class = 6, nthread = 6, nround = 60, objective = "multi:softmax")
  
  prediction2 <- predict(bstSparse, as.matrix(training.p11[fold==i, -c(1,54)]))
  
  ac[i] <- (1/length(prediction2)) * sum(training.p11$popularity[fold==i] == prediction2)*100
}

print( mean(ac))


bstSparse <- xgboost(data = as.matrix(training.p11[, -c(1,54)]), label = training.p11[, 54], max.depth = 2, 
                     eta = 0.25, num_class = 6, nthread = 6, nround = 60, objective = "multi:softmax")


testing.2<- testing[c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,17,18,21,22,23,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,43,44,45,46,47,48,50,51,52,53,54,55,56,57,58,59,60,61,62,63,75)]
testing.2$Year <- as.numeric(testing.2$Year)
testing.2$Month <- as.numeric(testing.2$Month)
testing.2$comic <- as.numeric(testing.2$comic)
testing.2$day_of_month <- as.numeric(testing.2$day_of_month)




prediction2 <- predict(bstSparse, as.matrix(testing.2[, -c(1)]))


solution <- data.frame(Id = test$id, popularity = as.numeric(prediction2))
write.csv(solution, file = "predictions_xgb_3.csv", row.names = FALSE)

