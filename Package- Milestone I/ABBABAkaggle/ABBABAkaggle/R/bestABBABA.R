

bestABBABA <- function(data, test, file = FALSE){

  # Load libraries
  library(tm)
  library(randomForest)
  set.seed(111)


  # TRAIN DATA: extract features from the URL characters in the training data

  # year, month, day
  data$Year         <- substr(data$url,21,24)
  data$Month        <- substr(data$url,26,27)
  data$day_of_month <- substr(data$url,29,30)

  # title
  get.title<-function(data){
    title <- sapply(1:length(data$url), function(i) {
      substr(data$url[i],32, nchar(as.character(data$url[i]))-1)})
    
    text.input <- as.factor(as.character(title))
    text.input <- gsub("-", " ", text.input)
    
    return(text.input)
  }

  text.input <- get.title(data)

  # text variables

  text.variables<-function(text.input, minimum){
    docs <- Corpus(VectorSource(text.input))
    dtm  <- DocumentTermMatrix(docs, control=list(bounds = list(global = c(minimum,Inf))))
    
    return(as.matrix(dtm))
  }

  textvars <- text.variables(text.input,150)
  
  #remove problematic words (error in random forest)
  textvars <- textvars[,-which(colnames(textvars)=='2013')]
  textvars <- textvars[,-which(colnames(textvars)=='2014')]
  textvars <- textvars[,-which(colnames(textvars)=='for')]

  words <- colnames(textvars)
  
  #remove 2's from the matrix to have binary variables
  
  for( i in 1:dim(textvars)[2]){
    textvars[,i] <- ifelse(textvars[,i]>1, 1, textvars[,i])
    }
  
  textvars <- apply(textvars, 2, as.factor)
  train    <- cbind(data[, -2], textvars)

  # TEST DATA: extract features from the URL characters in the test data
  
  # year, month, day
  test$Year         <- substr(test$url,21,24)
  test$Month        <- substr(test$url,26,27)
  test$day_of_month <- substr(test$url,29,30)

  # Text Variables
  test.input    <- get.title(test)
  docs          <- Corpus(VectorSource(test.input))
  dtm           <- DocumentTermMatrix(docs, control=list(bounds = list(global = c(0,Inf))))
  test.textvars <- inspect(dtm[,words])
  
  #remove 2's amd 3's
  
  for( i in 1:dim(test.textvars)[2]){
    test.textvars[,i] <- ifelse(test.textvars[,i]>1, 1, test.textvars[,i])
    }
  
  test.textvars <- apply(test.textvars, 2, as.factor)
  testing <- cbind(test[,-2],test.textvars)


  # Keep only the most important features found using the function RRF (package "RRF")
  training.rv<- train[c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,17,18,21,22,23,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,43,44,45,46,47,48,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,76)]
  
  forest <- randomForest(factor(popularity) ~ . , 
                       data=training.rv, 
                       replace = FALSE,
                       importance = TRUE, ntree = 800)

  # Predict test data
  prediction <- predict(forest, testing)
  solution   <- data.frame(Id = test$id, popularity = as.numeric(prediction2))
  
  if(file){
    write.csv(solution, file = "predictions.csv", row.names = FALSE)
    }
  
  return(solution)
}



