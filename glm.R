Hitter <- read.csv("data\\hitter_refactored2.csv", sep=",", dec=".")

####################
# SPLITTING DATA
####################

N <- dim(Hitter)[1]

validation_data = NULL
training_data = NULL
set.seed(666)
for(i in 1:N){
  p <- runif(1)
  d <- Hitter[i,]
  if(p>2/3){
    validation_data <- rbind(validation_data, d)
  }
  else{
    training_data <- rbind(training_data, d)
  }
}
V <- dim(validation_data)[1]
T <- dim(training_data)[1]

training_target <- training_data[,22]
validation_target <- validation_data[,22]

walks_in_1986 <- training_data[,7]
times_at_bat_car <- training_data[,9]
hits_percentage_car <- training_data[,10]
putouts_in_1986 <- training_data[,19]
model <- lm(training_target ~ #walks_in_1986 + times_at_bat_car + hits_percentage_car
             #+ putouts_in_1986
               training_data[,2] + training_data[,9] + 
               
               #DEFENSIVE STAT
               training_data[,19])
summary(model)
par(mfrow=c(1,1))
plot(model)

S <- V + 1
predictions <- predict(model, newdata=validation_data)
predictions <- predictions[1:V]

training_NRMSE <- sqrt( sum(model$residuals^2) / ((N-1)*var(training_target)) )
validation_NRMSE  <- sqrt( sum((validation_target - predictions)^2) / ((V -1)*var(validation_target)) )



################################
# TRYING RIDGE-REGRESSION
################################
N <- dim(training_data)[1]
N.test <- dim(validation_data)[1]
p <- 1
q <- N-1

coef <- list()
model <- list()
norm.root.mse.train <- NULL
norm.root.mse.test <- NULL

tr <- cbind(training_data[,2] , training_data[,9] , training_data[,19])
tr <- as.matrix(tr)
for (i in p:q)
{
  model[[i]] <- glm(t ~ poly(tr, i, raw=TRUE), data = training_data, family = gaussian)
  
  # store the model coefficients, as well as training and test errors
  
  coef[[i]] <- model[[i]]$coefficients
  norm.root.mse.train[i] <- sqrt(model[[i]]$deviance/N)
  
  predictions <- predict (model[[i]], newdata=validation_data)  
  norm.root.mse.test[i] <- sqrt(sum((validation_target - predictions)^2)/((N.test-1)*var(validation_data)))
}

# we gather everything together

results <- cbind (Degree=p:q, Coefficients=coef, NRMSE.train=norm.root.mse.train, NRMSE.test=norm.root.mse.test)

