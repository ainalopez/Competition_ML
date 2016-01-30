# Load data
training <- read.csv(file = "Desktop/ML_competition/Data/news_popularity_training.csv")
test     <- read.csv(file = "Desktop/ML_competition/Data/news_popularity_test.csv")

# Keep only predictive data
training.p <- training[c(-1, -2, -3)]
test.p     <- test[c(-1, -2, -3)]

n <- dim(training.p)[1]

set.seed(123)
train_ind <- sample(n, size = (n/2+500))

training.p1  <- training.p[train_ind,]
training.p2 <- training.p[-train_ind,-59]
training.p2.label  <- training.p[-train_ind,59]

