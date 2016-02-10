library(tm)
set.seed(111)

# Load data
training <- read.csv(file = "Desktop/ML_competition/Data/news_popularity_training.csv")
test     <- read.csv(file = "Desktop/ML_competition/Data/news_popularity_test.csv")

data <- training

# TRAIN DATA

# year, month, day
data$Year<-substr(data$url,21,24)
data$Month<-substr(data$url,26,27)
data$day_of_month<-substr(data$url,29,30)

# title
get.title<-function(data){
  
  title<-sapply(1:length(data$url), function(i) {
    substr(data$url[i],32, nchar(as.character(data$url[i]))-1)})
  
  text.input<-as.factor(as.character(title))
  text.input<-gsub("-", " ", text.input)
  return(text.input)
  
}

text.input<-get.title(data)

# text variables

text.variables<-function(text.input, minimum){
  
  docs <- Corpus(VectorSource(text.input))
  dtm <- DocumentTermMatrix(docs, control=list(bounds = list(global = c(minimum,Inf))))
  return(as.matrix(dtm))
  
}

textvars<-text.variables(text.input,150)

#remove problematic words (error in random forest)
# textvars<-textvars[,-which(colnames(textvars)=='100')]
textvars<-textvars[,-which(colnames(textvars)=='2013')]
textvars<-textvars[,-which(colnames(textvars)=='2014')]
# textvars<-textvars[,-which(colnames(textvars)=='2015')]
textvars<-textvars[,-which(colnames(textvars)=='for')]
# textvars<-textvars[,-which(colnames(textvars)=='next')]
# textvars<-textvars[,-which(colnames(textvars)=='200')]
# textvars<-textvars[,-which(colnames(textvars)=='360')]
# textvars<-textvars[,-which(colnames(textvars)=='90s')]

words<-colnames(textvars)

#remove 2's from the matrix to have binary variables
textvars<-ifelse(textvars>0,1,0)

#data$TM<-apply(textvars,1,sum)
textvars<-apply(textvars, 2, as.factor)

train<-cbind(data[, -2], textvars)

# TEST DATA

# year, month, day
test$Year<-substr(test$url,21,24)
test$Month<-substr(test$url,26,27)
test$day_of_month<-substr(test$url,29,30)

# Text Variables
test.input<-get.title(test)
docs <- Corpus(VectorSource(test.input))
dtm <- DocumentTermMatrix(docs, control=list(bounds = list(global = c(0,Inf))))
test.textvars<-inspect(dtm[,words])
#test$TM<-apply(textvars,1,sum)

#remove 2's amd 3's
textvars<-ifelse(textvars>0,1,0)

test.textvars<-apply(test.textvars, 2, as.factor)

testing<-cbind(test[,-2],test.textvars)





library(randomForest)


training.p11<- train[c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,17,18,21,22,23,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,43,44,45,46,47,48,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,76)]

# CHOOSE VARIABLES
#forest <- RRF(factor(popularity) ~ . , 
#                       data=training.p11, 
#                       #mtry = 10,
#                       #sampsize=c(2000,2000,2000,516,k),
#                       #cutoff=c(0.125, 0.125, 0.20, 0.25, 0.3),
#                       replace = FALSE,
#                       importance = TRUE, ntree = 300)


forest <- randomForest(factor(popularity) ~ . , 
                       data=training.p11, 
                       #mtry = 10,
                       #sampsize=c(2000,2000,2000,516,k),
                       #cutoff=c(0.125, 0.125, 0.20, 0.25, 0.3),
                       replace = FALSE,
                       importance = TRUE, ntree = 800)

forest

####predictions####
prediction2 <- predict(forest, testing)
# table(prediction, training.p2.label)

solution <- data.frame(Id = test$id, popularity = as.numeric(prediction2))
write.csv(solution, file = "predictions_rf_3.csv", row.names = FALSE)





