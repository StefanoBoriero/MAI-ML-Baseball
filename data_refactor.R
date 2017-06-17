Hitter <- read.csv("data\\hitter-Copia.csv", sep=";", dec=".")

Hitter <- na.omit(Hitter)
par(mfrow=c(1,1))
plot(Hitter[,2:7])
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
for(i in 1:N){
  times_bat <- Hitter[i,2]
  times_hit <- Hitter[i,3]
  home_runs <- Hitter[i,4]
  times_run <- Hitter[i,5]
  runs_bat_in <- Hitter[i,6]
  
  hit_p <- times_hit / times_bat
  run_p <- times_run / times_hit
  home_p <- home_runs / runs_bat_in
  r_bat_p <- runs_bat_in / times_bat

  
  hits_percentage <- rbind(hits_percentage, hit_p)
  run_percentage <- rbind(run_percentage, run_p)
  home_run_p <- rbind(home_run_p, home_p)
  runs_bat_in_p <- rbind(runs_bat_in_p, r_bat_p)
}


Hitter[,3] <- hits_percentage
Hitter[,4] <- home_run_p
Hitter[,5] <- run_percentage
Hitter[,6] <- runs_bat_in_p
colnames(Hitter)[c(4,6)] <- c("Home_run_percentage_in_1986", "Runs_batted_in_percentage_in_1986")
colnames(Hitter)[c(3,5)] <- c("Hits_percentage_in_1986", "Run_percentage_in_1986")
colnames(Hitter)

par(mfrow=c(1,1))
plot(Hitter[,2:7])


model <- lm(target ~ training_data[,2] + training_data[,3] + training_data[,4] + 
              training_data[,5] + training_data[,6] + training_data[,7])
summary(model)

plot(Hitter[,9:14])
# Let's do the same for carrer data
#########################
# REFACTORING CAREER DATA
#########################

hits_percentage_car = NULL
run_percentage_car = NULL
home_run_p_car = NULL
runs_bat_in_p_car = NULL
for(i in 1:N){
  times_bat_car <- Hitter[i,9]
  times_hit_car <- Hitter[i,10]
  home_runs_car <- Hitter[i,11]
  times_run_car <- Hitter[i,12]
  runs_bat_in_car <- Hitter[i,13]
  
  hit_p <- times_hit_car / times_bat_car
  run_p <- times_run_car / times_hit_car
  home_p <- home_runs_car / runs_bat_in_car
  r_bat_p <- runs_bat_in_car / times_bat_car
  

  hits_percentage_car <- rbind(hits_percentage_car, hit_p)
  run_percentage_car <- rbind(run_percentage_car, run_p)
  home_run_p_car<- rbind(home_run_p_car, home_p)
  runs_bat_in_p_car <- rbind(runs_bat_in_p_car, r_bat_p)
}

Hitter[, 10] <- hits_percentage_car
Hitter[, 11] <- home_run_p_car
Hitter[, 12] <- run_percentage_car
Hitter[, 13] <- runs_bat_in_p_car

colnames(Hitter)[c(10,11,12,13)] <- c("Hits_percentage_carrer", "Home_run_percentage_carrer",
                                      "Run_percentage_carrer", "Runs_batted_in_percentage_carrer")
plot(Hitter[,9:14])

# Now data looks better
write.csv(Hitter, file = "data\\hitter_refactored.csv", col.names = colnames(Hitter), row.names = FALSE,
          sep = ";", dec=".")