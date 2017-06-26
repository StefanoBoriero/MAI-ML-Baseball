hitter <- read.csv("Val.CSV", header=TRUE, sep =",")
library(randomForest)
library(caret)




N <- nrow(hitter)
learn <- sample(1:N, round(2*N/3))
nlearn <- length(learn)
ntest <- N - nlearn
target <- hitter$Annual_salary
models.recap <- matrix(ncol = 4)
colnames(models.recap) <- c("k", "t", "TR_NRMSE", "VA_NRMSE")

k <- 213

CV.folds <- createFolds(target, k=k)
cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("k","fold","TR error","VA error")

cv.results[,"TR error"] <- 0
cv.results[,"VA error"] <- 0
cv.results[,"k"] <- k

for (t in c(50,100,150,200)){

for (j in 1:k){
  
  # get VA data
  va <- unlist(CV.folds[j])
  trainset <- hitter[-va,]
  valset <- hitter[va,]
  target <- trainset$Annual_salary
  targetval <- valset$Annual_salary
  
  
  #
  model.rf <- randomForest(target ~., data = trainset[,-21], ntree=t, proximity=TRUE)
  
  
 
 
  # predict TR data
  pred.va <- (predict (model.rf, trainset))
  
  # we can now have a closer look at our model:
  
  #summary(model.rf)
  #print(model.rf)
  
  #model.rf$oob.times
  #model.rf$confusion
  
  # The importance of variables
  importance(model.rf)
  varImpPlot(model.rf)
  
  N <- dim(trainset)[1]
  
  e <- 0
  temp <- 0
  norm.root.mse.train = 0
  for(i in 1:(N)){
    e = (trainset[i,21] - pred.va[i])^2#
    temp = e + temp
  }
  norm.root.mse.train <- sqrt((temp)/((N-1)*var(trainset$Annual_salary)))
  norm.root.mse.train
  
  cv.results[j,"TR error"] <- norm.root.mse.train

  
  # predict VA data
  pred.va <- predict (model.rf, newdata=valset)
  
  N <- dim(valset)[1]
  e <- 0
  temp <- 0
  
  norm.root.mse.test = NULL
  for(i in 1:(N)){
    e = (valset[i,21] - pred.va[i])^2#
    temp = e + temp
  }
  norm.root.mse.test <- sqrt((temp))
  norm.root.mse.test
  
  cv.results[j,"VA error"] <- norm.root.mse.test
  cv.results[j,"fold"] <- j
}


## have a look at the results
cv.results



## Compute the average 
(VA.error <- mean(cv.results[,"VA error"]))
(TR.error <- mean(cv.results[,"TR error"]))

nome <- paste('RF_cv=t=',t,'.txt')
nome2 <- paste('RF_VA_error_t=',t,'.txt')
write.table(cv.results, file=nome, row.names=F)
write.table(VA.error, file=nome2, row.names=F)
models.recap <- rbind(models.recap, c(TR.error, VA.error)) 
}
models.recap <- models.recap[-1,]

write.table(models.recap, file = "RF.CSV", sep =" & ", col.names = TRUE)

#plot(model.rf)

