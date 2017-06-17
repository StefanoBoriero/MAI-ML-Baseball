library(e1071)
library(TunePareto) # for generateCVRuns()
library(caret)
hitter <- read.csv("/Users/riccardosimionato/Music/iTunes/iTunes Media/Podcasts/Hitter_final.CSV", header=TRUE, sep =";")
hitter2 <- read.csv("/Users/riccardosimionato/Music/iTunes/iTunes Media/Podcasts/Hitter_final.CSV", header=TRUE, sep =";")

#inTraining <- createFolds(hitter$Annual_salary, k = 10, list = TRUE, returnTrain = FALSE)


#CV part
k <- 10
target <- hitter$Annual_salary
CV.folds <- generateCVRuns(target, ntimes=1, nfold=k, stratified=TRUE)

cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("k","fold","TR error","VA error")

cv.results[,"TR error"] <- 0
cv.results[,"VA error"] <- 0
cv.results[,"k"] <- k

# train on TR data
for (cost in 10^seq(-2,3))
{
  for (d in seq(2,5))
  {
    for (c in 1^seq(-2,3))
    {
      
for (j in 1:k)
{
  # get VA data
  va <- unlist(CV.folds[[1]][[j]])
  #va <- unlist(inTraining[j])
  trainset <- hitter[-va,]
  valset <- hitter[va,]
  #valset2 <- valset[,1:2]
  target <- trainset$Annual_salary
  targetval <- valset$Annual_salary
  
 
  
    model.pol <- svm(trainset[,1:20],target, C=cost, kernel="polynomial", degree=d, coef0=c, scale = FALSE)
    #plot.prediction (model.pol, paste ("quadratic kernel (C=", C, ") ", model.pol$tot.nSV, " Support Vectors", sep=""))
    
    # predict TR data
    pred.va <- predict (model.pol)
    
    tab <- table(hitter[-va,]$Annual_salary, pred.va)
    cv.results[j,"TR error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    # predict VA data
    pred.va <- predict (model.linear, newdata=valset[,1:20])
    targetval <- valset$Annual_salary
    tab <- table(targetval, pred.va)
    cv.results[j,"VA error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
}
      
      ## have a look at the results ...
      cv.results
      
      
      
      ## What one really uses is the average of the last column
      (VA.error <- mean(cv.results[,"VA error"]))
      
      nome <- paste('POL_cv_c=',cost,'degree',d,'coef',c,'.txt')
      nome2 <- paste('POL_VA_error_c=',cost,'degree',d,'coef',c,'.txt')
      write.table(cv.results, file=nome, row.names=F)
      write.table(VA.error, file=nome2, row.names=F)
    }
  }
}
