Hitter <- read.csv("data\\hitter_refactored.csv", sep=",", dec =".")

library("e1071")

target <- Hitter[,22]



model_svr <- svm(target~Hitter[,7] + Hitter[, 9] + Hitter[,10] + Hitter[,19], data = Hitter )
prediction <- predict(model_svr, Hitter)

N <- dim(Hitter)[1]
error = NULL
for(i in 1:N){
  e_ass <- abs(prediction[i] - Hitter[i,22])
  e_per <- e_ass / Hitter[i,22]
  error <- rbind(error, e_per)
}

sum(error) / dim(error)[1]
summary(model_svr)
?svm()
