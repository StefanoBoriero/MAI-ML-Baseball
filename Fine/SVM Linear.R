library(e1071)
library(TunePareto) # for generateCVRuns()
hitter <- read.csv("/Users/riccardosimionato/Music/iTunes/iTunes Media/Podcasts/hitter_refactored.CSV", header=TRUE, sep =",")
library(kernlab)


#CV part
k <- 10
target <- hitter[,22]

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
     
      for (j in 1:k)
    
      {
        
       
        
        # get VA data
        va <- unlist(CV.folds[[1]][[j]])
        #va <- unlist(inTraining[j])
        trainset <- hitter[-va,]
        valset <- hitter[va,]
        #valset2 <- valset[,1:2]
        target <- trainset[,22]
        #target <- trainset$Annual_salary
        targetval <- valset$Annual_salary
        
       # trainset$Number.of.times.at.bat.in.1986 
model.linear <- svm(target~ trainset$Hits_percentage_carrer + trainset$number.of.years.in.the.major.leagues  + trainset$number.of.put.outs.in.1986 + trainset$Run_percentage_in_1986 , C=cost, gamma=g, kernel = "linear")
#model.linear <- svm(target~ .,  C=cost, gamma=g, kernel = "linear", data = trainset)

#trainset$Runs_batted_in_percentage_career
#trainset$Run_percentage_career 
#trainset$Times_at_bat_per_year
#trainset$Walks_percentage_in_1986
#target~trainset[,7] + trainset[, 9] + trainset[,10] + trainset[,19]
#number.of.walks.in.1986 + Number.of.times.at.bat.in.1986 + Hits_percentage_carrer + number.of.put.outs.in.1986

    # predict TR data
    pred.va <- predict (model.linear, trainset)
    

    N <- dim(trainset)[1]
    #error = NULL
    #for(i in 1:N){
     # e_ass <- abs(pred.va[i] - trainset[i,22])
    #  e_per <- e_ass / trainset[i,22]
    #  error <- rbind(error, e_per)
    #}
    
    e <- 0
    temp <- 0
    norm.root.mse.tes = 0
    for(i in 1:N){
      e = (trainset[i,22] - pred.va[i])^2
      temp = e + temp
      
    }
    norm.root.mse.test <- sqrt((temp)/((N-1)*var(trainset[,22])))
    cv.results[j,"TR error"] <- norm.root.mse.test
    #cv.results[j,"TR error"] <- sum(error) / dim(error)[1]
    summary(model.linear)
    
    
    # predict VA data
    pred.va <- predict (model.linear, newdata=valset)
    
    N <- dim(valset)[1]
    e <- 0
    temp <- 0
    #error = NULL
    # for(i in 1:N){
    #  e_ass <- abs(pred.va[i] - valset[i,22])
    #  e_per <- e_ass / valset[i,22]
    # error <- rbind(error, e_per)
    #}
    norm.root.mse.test = 0
    for(i in 1:N){
      e = (valset[i,22] - pred.va[i])^2
      temp = e + temp
    }
    
    norm.root.mse.test <- sqrt((temp)/((N-1)*var(valset[,22])))
    
    # cv.results[j,"VA error"] <- sum(error) / dim(error)[1]
    
    cv.results[j,"VA error"] <- norm.root.mse.test
    cv.results[j,"fold"] <- j
          }

 
  ## have a look at the results ...
  cv.results
  
   

      ## What one really uses is the average of the last column
      (VA.error <- mean(cv.results[,"VA error"]))
    
      nome <- paste('LIN_cv_c=',cost,'g=',g,'.txt')
      nome2 <- paste('LIN_VA_error_c=',cost,'g=',g,'.txt')
      write.table(cv.results, file=nome, row.names=F)
      write.table(VA.error, file=nome2, row.names=F)
      
       
     } 
  }
