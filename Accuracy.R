
  
  dif <- as.numeric(pred)-training.p2$popularity 
  eq  <- 0
  
  for(i in 1:length(dif)){
    if(dif[i] == 0){
      eq <- eq +1
    }
  }
  
  print(cat("Accuracy of the validation set= ", eq/length(dif) *100 )
  
