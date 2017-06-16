library(e1071)
library(TunePareto) # for generateCVRuns()

hitter <- read.csv("/Users/riccardosimionato/Music/iTunes/iTunes Media/Podcasts/Hitter_finalOr.CSV", header=TRUE, sep =";")
hitter2 <- read.csv("/Users/riccardosimionato/Music/iTunes/iTunes Media/Podcasts/Hitter_final2.CSV", header=TRUE, sep =";")

target  <- hitter$Annual_salary
target2  <- hitter2$Annual_salary
#CV part
k <- 10

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
  trainset <- hitter[-va,]
  valset <- hitter[va,]
  valset2 <- valset[,1:2]
  target <- trainset$Annual_salary
  # train on TR data
  model.linear <- svm(trainset[,1:2], target, C=1, kernel="linear", scale = FALSE)
  
  # predict TR data
  pred.va <- predict (model.linear)
  
  tab <- table(hitter[-va,]$Annual_salary, pred.va)
  cv.results[j,"TR error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
  
  # predict VA data
  pred.va <- predict (model.linear, newdata=valset2)
  targetval <- valset$Annual_salary
  tab <- table(targetval, pred.va)
  cv.results[j,"VA error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
  
  cv.results[j,"fold"] <- j
}

## have a look at the results ...
cv.results

## What one really uses is the average of the last column
(VA.error <- mean(cv.results[,"VA error"]))


