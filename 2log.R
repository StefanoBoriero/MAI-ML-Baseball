Hitter.std <- read.csv("data//hitter_refactored_scaled.csv", header = TRUE, sep=",")

target <- Hitter.std[,17]
target.log <- log(target)

Hitter.log[,17] <- target.log
colnames(Hitter[,17]) <- c("Log_Salary_1987")

write.csv(Hitter.log, filename="hitter_refactored_scaled_log.csv", row.names = FALSE, header = TRUE)