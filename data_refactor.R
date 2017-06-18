Hitter <- read.csv("data\\hitter-Copia.csv", sep=";", dec=".")

Hitter <- na.omit(Hitter)
par(mfrow=c(1,1))
plot(Hitter[,2:7])

round(cor(Hitter[,2:7]),2)
######################################################
## As we can see, there's a linear dependency between:
## times at bat ~ hits
## runs ~ hits
## home runs ~ runs batted in
## runs batted in ~ times at bat
######################################################


# Trying to replace with percentages
N <- dim(Hitter)[1]

hits_percentage = NULL
run_percentage = NULL
home_run_p = NULL
runs_bat_in_p = NULL
n_walks_P = NULL
for(i in 1:N){
  times_bat <- Hitter[i,2]
  times_hit <- Hitter[i,3]
  home_runs <- Hitter[i,4]
  times_run <- Hitter[i,5]
  runs_bat_in <- Hitter[i,6]
  n_walks <- Hitter[i,7]
  
  hit_p <- times_hit / times_bat
  run_p <- times_run / times_hit
  home_p <- home_runs / times_bat
  r_bat_p <- runs_bat_in / times_bat
  walks_p <- n_walks / times_bat

  
  hits_percentage <- rbind(hits_percentage, hit_p)
  run_percentage <- rbind(run_percentage, run_p)
  home_run_p <- rbind(home_run_p, home_p)
  runs_bat_in_p <- rbind(runs_bat_in_p, r_bat_p)
  n_walks_P <- rbind(n_walks_P, walks_p)
}


Hitter[,3] <- hits_percentage
Hitter[,4] <- home_run_p
Hitter[,5] <- run_percentage
Hitter[,6] <- runs_bat_in_p
Hitter[,7] <- n_walks_P
colnames(Hitter)[c(4,6)] <- c("Home_run_percentage_in_1986", "Runs_batted_in_percentage_in_1986")
colnames(Hitter)[c(3,5,7)] <- c("Hits_percentage_in_1986", "Run_percentage_in_1986"
                                ,"Walks_percentage_in_1986")
colnames(Hitter)

par(mfrow=c(1,1))
plot(Hitter[,2:7])

target <- Hitter[,22]
training_data <- Hitter
model <- lm(target ~ training_data[,2] + training_data[,3] + training_data[,4] + 
              training_data[,5] + training_data[,6] + training_data[,7])
summary(model)

plot(Hitter[,8:14])
(cor(Hitter[,8:14]))
# Let's do the same for carrer data
#########################
# REFACTORING CAREER DATA
#########################

times_at_bat_per_year = NULL
hits_percentage_car = NULL
run_percentage_car = NULL
home_run_p_car = NULL
runs_bat_in_p_car = NULL
walks_p_car = NULL
for(i in 1:N){
  years <- Hitter[i,8]
  times_bat_car <- Hitter[i,9]
  times_hit_car <- Hitter[i,10]
  home_runs_car <- Hitter[i,11]
  times_run_car <- Hitter[i,12]
  runs_bat_in_car <- Hitter[i,13]
  walks_car <- Hitter[i,14]
  
  bat_per_year <- times_bat_car / years
  hit_p <- times_hit_car / times_bat_car
  run_p <- times_run_car / times_hit_car
  home_p <- home_runs_car / runs_bat_in_car
  r_bat_p <- runs_bat_in_car / times_bat_car
  walks_p <- walks_car / times_bat_car
  

  times_at_bat_per_year <- rbind(times_at_bat_per_year, bat_per_year)
  hits_percentage_car <- rbind(hits_percentage_car, hit_p)
  run_percentage_car <- rbind(run_percentage_car, run_p)
  home_run_p_car<- rbind(home_run_p_car, home_p)
  runs_bat_in_p_car <- rbind(runs_bat_in_p_car, r_bat_p)
  walks_p_car <- rbind(walks_p_car, walks_p)
}

Hitter[,9] <- times_at_bat_per_year
Hitter[, 10] <- hits_percentage_car
Hitter[, 11] <- home_run_p_car
Hitter[, 12] <- run_percentage_car
Hitter[, 13] <- runs_bat_in_p_car
Hitter[, 14] <- walks_p_car

colnames(Hitter)[c(9,10,11,12,13,14)] <- c("Times_at_bat_per_year", "Hits_percentage_career", "Home_run_percentage_career",
                                      "Run_percentage_career", "Runs_batted_in_percentage_career"
                                      , "Walks_percentage_career")
plot(Hitter[,9:14])
round(cor(Hitter[,8:14]))

write.csv(Hitter, file = "data\\hitter_refactored.csv", row.names = FALSE)

salary <- Hitter[,22]
Hitter <- Hitter[,-24]
Hitter <- Hitter[,-23]
Hitter <- Hitter[,-(15:18)]
Hitter <- Hitter[,-1]
Hitter.std <- data.frame(cbind(scale(Hitter[,1:16]),Hitter$X1987.annual.salary.on.opening.day.in.thousands.of..dollars))
names(Hitter.std)[17] <- 'Salary_1987'

target <- Hitter.std$Salary_1987
model <- glm(target ~ ., data = Hitter.std[,1:16])
summary(model)
plot(model)
model_reduced <- glm(target ~ 
                      # Years in Major League
                      Hitter.std[,7] +
                      #Times at bat per year
                      Hitter.std[, 8] +
                      # Hits percentage career
                      Hitter.std[,9] +
                      #Run percentage career
                      Hitter.std[,11] + 
                      # Runs batted in percentage career
                      Hitter.std[,12] +
                      # Put outs 1986
                      Hitter.std[,14],
                    
                      data = Hitter.std
                      )
summary(model_reduced)
plot(model_reduced)

N <- dim(Hitter.std)[1]
mean.square.error <- model$deviance/N
mean.square.error.reduced <- model_reduced$deviance/N


(norm.root.mse <- sqrt(model$deviance/((N-1)*var(target))))
(norm.root.mse.reduced <- sqrt(model_reduced$deviance/((N-1)*var(target))))

anova(model, model_reduced)
# Now data looks better
write.csv(Hitter.std, file = "data\\hitter_refactored_scaled.csv", row.names = FALSE)
