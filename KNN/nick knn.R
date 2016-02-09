setwd("~/Desktop")

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}
#create distance matrix
get_dist_matrix <- function(features, test,p){
  if (p==1){
    dist <- as.matrix(dist2(features,test, method = "manhattan",p=1))
  }
  if (p==2){
    dist <- as.matrix(dist2(features,test, method = "euclidean",p=2))
  }
  if (p==Inf){
    dist <- as.matrix(dist2(features,test,  method = "maximum", p=Inf))
  }
  return(dist)
}
#get k nearest neigbors
get_neighbors <- function(order_dist,k){
  neighbors <- as.matrix(order_dist[,1:k])
  return(neighbors)
}
#convert neighbors matrix to matrix of labels
get_label_matrix <- function(neighbors, labels){
  row <- nrow(neighbors)
  col <- ncol(neighbors)
  label_matrix <- matrix(0,nrow=row, ncol=col)
  label_matrix[,] <- labels[neighbors]
  return(label_matrix)
}
#find majority vote 
get_majority_vote <- function(label_matrix){
  vote <- apply(label_matrix,1,mode)
  return(vote)
}
#save csv file
save_csv <- function(solution){
  write.csv(solution, file = "predictions1.csv", row.names = FALSE)
}

kNN <- function(features, labels, test, k, p){
  if (!require("flexclust")) install.packages("flexclust"); library(flexclust)
  features <- scale(features)
  test <- scale(test)
  dist <- get_dist_matrix(features, test, p)
  order_dist <- t(as.matrix(apply(dist,2,order)))
  neighbors <- get_neighbors(order_dist, k=k)
  label_matrix <- get_label_matrix(neighbors, labels)
  predictLabels <- get_majority_vote(label_matrix)
  return(predLabels=predictLabels)
}

train <- read.csv("news_popularity_training.csv")

test <- read.csv("news_popularity_test.csv")[-2]
features <- train[c(-2,-62)]

labels <- train$popularity

# Choose k using 4-Fold Cross Validation
k <- rep(seq(1,61,2))
lk <- length(k)

# 4 fold Cross-Validation
n <- nrow(train)
fold <- sample(1:4, n, replace=TRUE)

acc <- rep (NA, lk)
cvpred <- matrix(NA,nrow=n ,ncol=ncol(train))

for (h in 1:lk){
  ac <- rep(NA,4)
  for (i in 1:4){
    l <- kNN(features = features[fold!=i, ],labels = labels[fold!=i], 
             test = features[fold==i, ],  k = k[h] , p=2)
    ac[i] <- (1/length(l)) * sum(labels[fold==i] == l)*100
  }
  acc[h] <- mean(ac)
}

k_max <- k[which.max(acc)]

prediction <- kNN(features, labels, test, k_max, p)

solution <- data.frame(Id = test$id, Popularity = prediction)
save_csv(solution)

