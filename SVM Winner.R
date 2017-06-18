library(e1071)
library(TunePareto) # for generateCVRuns()
hitter <- read.csv("/Users/riccardosimionato/Music/iTunes/iTunes Media/Podcasts/hitter_refactored.CSV", header=TRUE, sep =",")
library(kernlab)


target <- hitter[,22]
#model.pol <- svm(target~hitter[,7] + hitter[, 9] + hitter[,10] + hitter[,19], C=1, gamma = 0.25 , degree=3, epsilon=0.1, coef = 0, kernel = "polynomial", data = hitter)
model.rad <- svm(target~hitter[,7] + hitter[, 9] + hitter[,10] + hitter[,19], C=0.01, gamma = 1 , kernel = "radial", data = hitter)
pred.va <- predict (model.rad, hitter)

#model.pol <- svm(target~hitter[,7] + hitter[, 9] + hitter[,10] + hitter[,19], kernel = "polynomial", data = hitter, cross = 10)

N <- dim(hitter)[1]
error = NULL
for(i in 1:N){
  e_ass <- abs(pred.va[i] - hitter[i,22])
  e_per <- e_ass / hitter[i,22]
  error <- rbind(error, e_per)
}

sum(error) / dim(error)[1]
summary(model.pol)
