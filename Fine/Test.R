library(nnet)
library(randomForest)
library(e1071)

Train <- read.csv("Val.CSV", header=TRUE, sep =",")
Test <- read.csv("Test.CSV", header=TRUE, sep =",")
N <- dim(Test)[1]

#Linear
model.lin <- svm(Train$Annual_salary~., cost=0.1, kernel = "linear", data = Train[,-21])#
pred.lin <- predict (model.lin, newdata=Test)



e <- 0
temp <- 0
norm.root.mse.train = 0
for(i in 1:(N)){
  e = (Test[i,21] - pred.lin[i])^2#
  temp = e + temp
}
norm.root.mse.train <- sqrt((temp)/((N-1)*var(Test$Annual_salary)))
norm.root.mse.train


#Pol
model.pol <- svm(Train$Annual_salary~., cost=0.01, gamma = 0.25, coef0=10, degree=2, kernel = "polynomial", data = Train[,-21])#
pred.pol <- predict (model.pol, newdata=Test)




e <- 0
temp <- 0
norm.root.mse.train = 0
for(i in 1:(N)){
  e = (Test[i,21] - pred.pol[i])^2#
  temp = e + temp
}
norm.root.mse.train <- sqrt((temp)/((N-1)*var(Test$Annual_salary)))
norm.root.mse.train

#Sig
model.sig <- svm(Train$Annual_salary~., cost=0.01, gamma = 1, coef0=1, kernel = "sigmoid", data = Train[,-21])#
pred.sig <- predict (model.sig, newdata=Test)



e <- 0
temp <- 0
norm.root.mse.train = 0
for(i in 1:(N)){
  e = (Test[i,21] - pred.sig[i])^2#
  temp = e + temp
}
norm.root.mse.train <- sqrt((temp)/((N-1)*var(Test$Annual_salary)))
norm.root.mse.train

#Rad
model.rad <- svm(Train$Annual_salary~., cost=0.1, gamma = 0.25, kernel = "radial", data = Train[,-21])#
pred.rad <- predict (model.rad, newdata=Test)



e <- 0
temp <- 0
norm.root.mse.train = 0
for(i in 1:(N)){
  e = (Test[i,21] - pred.rad[i])^2#
  temp = e + temp
}
norm.root.mse.train <- sqrt((temp)/((N-1)*var(Test$Annual_salary)))
norm.root.mse.train

#NN
model.nnet <- nnet(Train$Annual_salary~., data = Train[,-21], size=16, maxit=200, decay=0.01)#
pred.nnet <- predict (model.nnet, newdata=Test)



e <- 0
temp <- 0
norm.root.mse.train = 0
for(i in 1:(N)){
  e = (Test[i,21] - pred.nnet[i])^2#
  temp = e + temp
}
norm.root.mse.train <- sqrt((temp)/((N-1)*var(Test$Annual_salary)))
norm.root.mse.train

#RF

model.rf <- randomForest(Train$Annual_salary~., data = Train[,-21], ntree=200, proximity=TRUE)
pred.rf <- predict (model.rf, newdata=Test)

N <- dim(Test)[1]

e <- 0
temp <- 0
norm.root.mse.train = 0
for(i in 1:(N)){
  e = (Test[i,21] - pred.rf[i])^2#
  temp = e + temp
}
norm.root.mse.train <- sqrt((temp)/((N-1)*var(Test$Annual_salary)))
norm.root.mse.train
