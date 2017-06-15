###################################
#  PCA dimensionality reduction
###################################

Hitter <- read.csv("data\\hitter.csv", sep=";", quote="\"", dec=".", header=TRUE)
#
N <- dim(Hitter)[1]
D <- dim(Hitter)[2]

Hitter <- na.omit(Hitter)


tot <- rowSums(as.array(Hitter))

for(n in N){
  x <- as.numeric(Hitter_matrix[n, 1:91])
  tot_sum <- x + tot_sum
}
mean_vector <- (1/N) * tot_sum

# Returns a prcomp object, containing:
# - stdve: standard deviation of principal components
#Hitter <- Hitter[,-1]
#Hitter$player.s.team.at.the.beginning.of.1987 <- as.factor(Hitter$player.s.team.at.the.beginning.of.1987)
#pcaHitter <- prcomp(scale(Hitter[,-19]))

#std_dev <- pcaHitter$sdev

#summary(pcaHitter)
#pc <- c(1,2,3,4,5)

#plot(pcaHitter$x[,pc])

#Hitter_r <- Hitter[,1:19]

#head(Hitter_r)
#pcaHitter_reduced <- princomp(scale(Hitter_r[,-19]))
#summary(pcaHitter_reduced)

#loadings(pcaHitter_reduced)

Hitter_original <- read.csv("data\\hitter.csv", header=TRUE, sep=";", dec=".")
Hitter_original <- na.omit(Hitter_original)
y= Hitter_original$X1987.annual.salary.on.opening.day.in.thousands.of..dollars
x=Hitter_original[,-22]
head(x)
x1=as.factor(x[,1])
x2=x[,2]
head(x)
dim(x)
colnames(x)
team=x[,23]
y=as.numeric(y)
homeruns=x[,12]
putouts = x[,19]
hits_car = x[,10]


n_bats_86 = x[,2]
n_hits_86 = x[,3]
n_home_runs_86 = x[,4]
n_runs_86 = x[,5]
n_runs_bat_86 = x[,6]
n_walks_86 = x[,7]
n_years = x[,8]
n_times_bat_car = x[,9]
n_hits_car = x[,10]
n_home_runs_car = x[,11]
n_runs_car = x[,12]
n_runs_bat_car = x[,13]
n_walks_car = x[,14]
player_league_86 = x[,15]
player_div_86 = x[,16]
player_team_86 = as.factor(x[,17])
player_pos_86 = as.factor(x[,18])
n_putouts_86 = x[,19]
n_ass_86 = x[,20]
n_err_86 = x[,21]
player_league_87 = x[,22]
player_team_87 = x[,23]
mod = lm(y ~ n_bats_86 + n_hits_86 + n_home_runs_86 + n_runs_86 + n_runs_bat_86 + n_walks_86 
         + n_years + n_times_bat_car + n_hits_car + n_walks_car + player_league_86 + player_div_86
         +  player_team_86 + player_pos_86 + n_putouts_86 + n_ass_86 + n_err_86 + player_league_87
         + player_team_87)

###################################
# GUARDA QUI QUELLI CON PIù STELLINE SONO PIù SIGNIFICATIVE
###################################
summary(mod)

plot(mod)

help("barplot")
help(hist)
hist(Hitter_original$X1987.annual.salary.on.opening.day.in.thousands.of..dollars ~ Hitter_original$player.s.league.at.the.beginning.of.1987)

barchart(Hitter_original$player.s.league.at.the.beginning.of.1987~Hitter_original$X1987.annual.salary.on.opening.day.in.thousands.of..dollars)


national_salaries <- as.numeric(Hitter_original$X1987.annual.salary.on.opening.day.in.thousands.of..dollars[Hitter_original$player.s.league.at.the.beginning.of.1987 == 'N'])
american_salaries <- as.numeric(Hitter_original$X1987.annual.salary.on.opening.day.in.thousands.of..dollars[Hitter_original$player.s.league.at.the.beginning.of.1987 == 'A'])

sum(national_salaries)
sum(american_salaries)

dim(national_salaries)
length(national_salaries)
length(american_salaries)

avg_national_sal = sum(national_salaries) / length(national_salaries)
avg_american_sal = sum(american_salaries) / length(american_salaries)

barplot(c(avg_american_sal, avg_national_sal))
