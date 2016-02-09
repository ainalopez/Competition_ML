
library("kernlab")

svm.model3 <- ksvm(factor(popularity) ~ . , 
                   data=training.p1,
                   type = "C-svc",
                   kernel = "polydot", 
                   #kpar = list(sigma = 0.25),
                   C = 1, 
                   nu = 0.2, 
                   epsilon = 0.1, 
                   prob.model = FALSE,
                   # class.weights = c("1" = 40, "2"= 30,"3" =20, "4"=5, "5"=5),
                   cross = 0, 
                   fit = TRUE, 
                   cache = 40,
                   tol = 0.001, 
                   shrinking = TRUE, 
                   na.action = na.omit)

prediction <- predict(svm.model, training.p2)
svm.model


dif <- as.numeric(prediction)-training.p2$popularity
eq  <- 0
for(i in 1:length(dif)){
  if(dif[i] == 0){
    eq <- eq +1
  }
}

eq/length(dif) *100


