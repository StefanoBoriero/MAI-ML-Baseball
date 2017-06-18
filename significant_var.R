Hitter.std <- read.csv("data\\hitter_refactored_scaled.csv", header = TRUE, sep=",", dec=".")

model <- lm(Hitter.std$Salary_1987 ~ ., data = Hitter.std[,1:16])
summary(model)
