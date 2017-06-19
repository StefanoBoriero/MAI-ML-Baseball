library(e1071)
library(TunePareto) # for generateCVRuns()
hitter <- read.csv("/Users/riccardosimionato/Music/iTunes/iTunes Media/Podcasts/hitter_refactored_scaled.CSV", header=TRUE, sep =",")
library(kernlab)


#CV part
k <- 10
target <- hitter[,17]

models.recap <- matrix(ncol = 4)
colnames(models.recap) <- c("Cost", "g", "TR_NRMSE", "VA_NRMSE")


#train on TR data
for (cost in 10^seq(-2,3))
{
  for (g in 2^seq(-2,3))
    
  {
    
    CV.folds <- generateCVRuns(target, ntimes=1, nfold=k, stratified=TRUE)
    
    cv.results <- matrix (rep(0,4*k),nrow=k)
    colnames (cv.results) <- c("k","fold","TR error","VA error")
    
    cv.results[,"TR error"] <- 0
    cv.results[,"VA error"] <- 0
    cv.results[,"k"] <- k
    
    for (j in 1:k){
      
      # get VA data
      va <- unlist(CV.folds[[1]][[j]])
      trainset <- hitter[-va,]
      valset <- hitter[va,]
      target <- trainset[,17]
      targetval <- valset$Annual_salary
      
      
      model.rad <- svm(target~ ., C=cost, gamma=g, kernel = "radial", data = trainset[,-17])

      
      # predict TR data
      pred.va <- predict (model.rad, trainset)
      
      
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
      summary(model.rad)
      
      
      # predict VA data
      pred.va <- predict (model.rad, newdata=valset)
      
      N <- dim(valset)[1]
      e <- 0
      temp <- 0
      
      norm.root.mse.test = NULL
      for(i in 1:(N)){
        e = (valset[i,17] - pred.va[i])^2
        temp = e + temp
      }
      norm.root.mse.test <- sqrt((temp)/((N-1)*var(valset[,17])))
      norm.root.mse.test
      # cv.results[j,"VA error"] <- sum(error) / dim(error)[1]
      
      cv.results[j,"VA error"] <- norm.root.mse.test
      cv.results[j,"fold"] <- j
    }
    
    
    ## have a look at the results ...
    cv.results
    
    
    
    ## What one really uses is the average of the last column
    (VA.error <- mean(cv.results[,"VA error"]))
    (TR.error <- mean(cv.results[,"TR error"]))
    
    nome <- paste('RAD_cv_c=',cost,'g=',g,'.txt')
    nome2 <- paste('RAD_VA_error_c=',cost,'g=',g,'.txt')
    write.table(cv.results, file=nome, row.names=F)
    write.table(VA.error, file=nome2, row.names=F)
    
    models.recap <- rbind(models.recap, c(cost, g, TR.error, VA.error)) 
  } 
}
models.recap <- models.recap[-1,]