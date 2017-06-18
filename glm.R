######################################
# DATA SPLITTING
######################################

Hitter.std <- read.csv("data\\hitter_refactored_scaled.csv", sep=",", dec=".")
N <- dim(Hitter.std)[1]

validation_data = NULL
training_data = NULL

set.seed(666)
for(i in 1:N){
  p <- runif(1)
  d <- Hitter.std[i,]
  if(p>2/3){
    validation_data <- rbind(validation_data, d)
  }
  else{
    training_data <- rbind(training_data, d)
  }
}

training_target <- training_data[,17]
N.train <- dim(training_data)[1]
N.test <- dim(validation_data)[1]
t <- training_data$Salary_1987
t.new <- validation_data$Salary_1987
var.va <- var(t.new)

#######################################
# LINEAR REGRESSION WITH REGULARIZATION
#######################################

model.linreg <- lm(training_target ~ ., training_data[1:16])
summary(model.linreg)


model.linreg.FINAL <- step(model.linreg)
(beta.linreg.FINAL <- coef(model.linreg.FINAL))

se.linreg <- sum((t.new - predict(model.linreg.FINAL, validation_data[,1:16]))^2)
mse.limreg <- se.linreg / N.test

norm.root.mse.linreg <- sqrt(se.linreg / ((N.test-1)*var.va) )
################################
# RIDGE-REGRESSION
################################

library(MASS)

model.ridge <- lm.ridge(training_target ~ ., data=training_data[1:16], lambda = seq(0,40,0.1))

plot(seq(0,40,0.1), model.ridge$GCV, main="GCV of Ridge Regression", type="l", 
     xlab=expression(lambda), ylab="GCV")

# The optimal lambda is given by

(lambda.ridge <- seq(0,40,0.1)[which.min(model.ridge$GCV)])

# We can plot the coefficients and see how they vary as a function of lambda

colors <- rainbow(16)

matplot(seq(0,40,0.1), coef(model.ridge)[,-1], xlim=c(0,40), type="l",xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors, lty=1, lwd=2, main="Ridge coefficients")
abline(v=lambda.ridge, lty=2)
abline(h=0, lty=2)
text(rep(10, 9), coef(model.ridge)[length(seq(0,40,0.1)),-1], colnames(training_data)[-17], pos=4, col=colors)

## So we refit our final ridge regression model using the best lambda

model.ridgereg.FINAL <- lm.ridge(training_target ~ ., data=training_data[1:16], lambda = lambda.ridge)

(beta.ridgereg.FINAL <- coef(model.ridgereg.FINAL))

se.ridgereg <- sum((t.new - beta.ridgereg.FINAL[1] - as.matrix(validation_data[,1:16])%*%beta.ridgereg.FINAL[2:17])^2)
mse.ridgereg <- se.ridgereg / N.test

norm.root.mse.ridgereg <- sqrt(se.ridgereg / ((N.test-1)*var.va) )
########################
# LASSO REGRESSION
########################

library(lars)

model.lasso <- lars(as.matrix(training_data[1:16]), training_target, type="lasso")

lambda.lasso <- c(model.lasso$lambda,0)

beta.lasso <- coef(model.lasso)

colors <- rainbow(16)

# It may help visualization if you plot using the scaled X data

beta.scale <- attr(model.lasso$beta, "scaled:scale")
beta.rescaled <- beta.lasso
for(j in 1:17) beta.rescaled[j,] <- beta.rescaled[j,]*beta.scale

matplot(lambda.lasso, beta.rescaled, xlim=c(16,-2), type="o", pch=20, xlab=expression(lambda), 
        ylab=expression(hat(beta.lasso)), col=colors)
text(rep(-0, 16), beta.rescaled[9,], colnames(training_data), pos=4, col=colors)

## we decide to choose this value
abline(v=lambda.lasso[4], lty=2)
abline(h=0, lty=2)

(beta.lasso <- beta.lasso[4,])

se.lasso <- sum((t.new - predict(model.lasso, as.matrix(validation_data[,1:16]), s=4, type="fit")$fit)^2)
mse.lasso <- se.lasso / N.test
norm.root.mse.lasso <- sqrt(se.lasso / ((N.test-1)*var.va) )
