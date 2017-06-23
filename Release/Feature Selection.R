#Remove Redundant Features

# load the library
library(mlbench)
library(caret)
# load the data
hitter <- read.csv("hitter_scaled_log_all.CSV", header=TRUE, sep =",")
# calculate correlation matrix
correlationMatrix <- cor(hitter[,1:22])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)


#Feature Selection

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=100)
# run the RFE algorithm
results <- rfe(hitter[,1:22], hitter[,23], sizes=c(1:22), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
