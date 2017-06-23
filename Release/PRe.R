#Pitcher <- read.csv("/pitcher.csv", header = TRUE, quote = "\"", sep=";", dec = ".", check.names=TRUE)
Hitter <- read.csv("/Users/riccardosimionato/Music/iTunes/iTunes Media/Podcasts/hitter.csv", header = TRUE, quote = "\"", sep=";", dec = ".", check.names=TRUE)
#Team <- read.csv("/team.csv", header = TRUE, quote = "\"", sep=";", dec = ".", check.names=TRUE)


#Recode
Hitter$Name <- NULL
Hitter[,22] <- NULL
Hitter[,23] <- NULL
#league_at_the_end
oldvalues <-       c("N", "A")
newvalues <-       c(0, 1)

Hitter$league_at_the_end  <- newvalues[ match(Hitter$league_at_the_end, oldvalues) ]

#division_at_the_end
oldvalues <-       c("W", "E")
newvalues <-       c(0, 1)

Hitter$division_at_the_end <- newvalues[ match(Hitter$division_at_the_end , oldvalues) ]

#team_at_the_end
oldvalues <-       c("N.Y.", "Phi.", "St.L.","Mon.", "Chi.", "Pit.", "Hou.", "Cin.", "S.F.", "S.D.", "L.A.", "Atl.", "Bos.", "Det.", "Tor.", "Cle.", "Mil.", "Bal.", "Cal.", "Tex.", "K.C.", "Oak.", "Min.", "Sea.")

newvalues <-       c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)

Hitter$team_at_the_end <- newvalues[ match(Hitter$team_at_the_end , oldvalues) ]

#position(s)
oldvalues <-       c("1B", "2B", "SS", "3B", "RF", "CF", "LF", "C", "DH", "OF", "UT", "OS", "3S", "13", "3O", "O1", "S3", "32", "DO", "OD", "CD", "CS", "23", "1O", "2S")
newvalues <-       c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)

Hitter$position <- newvalues[ match(Hitter$position , oldvalues) ]


#league_at_the_beginning

#oldvalues <-       c("N", "A")
#newvalues <-       c(0, 1)

#Hitter$league_at_the_beginning  <- newvalues[ match(Hitter$league_at_the_beginning , oldvalues) ]

#team_at_the_beginning

oldvalues <-       c("N.Y.", "Phi.", "St.L.","Mon.", "Chi.", "Pit.", "Hou.", "Cin.", "S.F.", "S.D.", "L.A.", "Atl.", "Bos.", "Det.", "Tor.", "Cle.", "Mil.", "Bal.", "Cal.", "Tex.", "K.C.", "Oak.", "Min.", "Sea.")

newvalues <-       c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)

Hitter$team_at_the_beginning <- newvalues[ match(Hitter$team_at_the_beginning , oldvalues) ]

Hitter$team_at_the_beginning <- NULL
# Preprocessing data

Hitter2<- na.omit(Hitter)

#oneHotFeatures = c('team_at_the_end','position','team_at_the_beginning') # Encoding categorical vars with one-hot

#library(ade4)

#for (f in oneHotFeatures){
#  dummy = acm.disjonctif(Hitter2[f])
#  Hitter2[f] = NULL
#  Hitter2 = cbind(Hitter2, dummy)
#}
write.csv(Hitter2, file="hitter666.csv", row.names = FALSE, header = TRUE) 
Hitter <- read.csv("hitter666.csv", header = TRUE, quote = "\"", sep=",", dec = ".", check.names=TRUE)


target <- Hitter$Annual_salary

target.log <- log10(target)

#Hitter$Annual_salary <- 0
Hitter666 <- data.frame(cbind(scale(Hitter[,1:20]),target.log))
names(Hitter666)[21] <- 'Annual_salary'



#colnames(Hitter[,21]) <- c("Log_Salary_1987")
write.csv(Hitter666, file="hitter_scaled_log.csv", row.names = FALSE)


