hitter <- read.csv("/Users/riccardosimionato/Music/iTunes/iTunes Media/Podcasts/hitter_refactored_scaled.CSV", header=TRUE, sep =",")
library(MASS)
library(nnet)
library(TunePareto) # for generateCVRuns()

target <- hitter[,17]
models.recap <- matrix(ncol = 4)
colnames(models.recap) <- c("size", "decay", "TR_NRMSE", "VA_NRMSE")

k <- 100
CV.folds <- generateCVRuns(target, ntimes=1, nfold=k, stratified=TRUE)

cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("k","fold","TR error","VA error")

cv.results[,"TR error"] <- 0
cv.results[,"VA error"] <- 0
cv.results[,"k"] <- k


for (s in 2*seq(1,10,by=1)){
  
  for (d in 10^seq(-3,3)){

for (j in 1:k){
  
  # get VA data
  va <- unlist(CV.folds[[1]][[j]])
  trainset <- hitter[-va,]
  valset <- hitter[va,]
  target <- trainset[,17]
  targetval <- valset$Annual_salary
  



model.nnet <- nnet(target ~., data = trainset[,-17], size=s, maxit=200, decay=d)
#model.nnet 

# This is the fitting criterion (aka error function)
#model.nnet$value

#  fitted values for the training data
#model.nnet$fitted.values

# and the residuals
#model.nnet$residuals

## Now look at the weights

#model.nnet$wts

## I think this way is clearer:

#summary(model.nnet)

# predict TR data
pred.va <- (predict (model.nnet, trainset))



N <- dim(trainset)[1]

e <- 0
temp <- 0
norm.root.mse.train = 0
for(i in 1:(N)){
  e = (trainset[i,17] - pred.va[i])^2
  temp = e + temp
}
norm.root.mse.train <- sqrt((temp)/((N-1)*var(trainset[,17])))
norm.root.mse.train

cv.results[j,"TR error"] <- norm.root.mse.train
#summary(model.nnet)


# predict VA data
pred.va <- predict (model.nnet, newdata=valset)

N <- dim(valset)[1]
e <- 0
temp <- 0

norm.root.mse.test = NULL
for(i in 1:(N-1)){
  e = (valset[i,17] - pred.va[i])^2
  temp = e + temp
}
norm.root.mse.test <- sqrt((temp)/((N-1)*var(valset[,17])))
norm.root.mse.test

cv.results[j,"VA error"] <- norm.root.mse.test
cv.results[j,"fold"] <- j
}


## have a look at the results ...
cv.results



## What one really uses is the average of the last column
(VA.error <- mean(cv.results[,"VA error"]))
(TR.error <- mean(cv.results[,"TR error"]))

nome <- paste('NN_cv_s=',s,'d=',d,'.txt')
nome2 <- paste('NN_VA_error_s=',s,'d=',d,'.txt')
write.table(cv.results, file=nome, row.names=F)
write.table(VA.error, file=nome2, row.names=F)
models.recap <- rbind(models.recap, c(s, d, TR.error, VA.error)) 
  }
}
models.recap <- models.recap[-1,]


