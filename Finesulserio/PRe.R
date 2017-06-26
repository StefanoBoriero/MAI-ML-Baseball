#Pitcher <- read.csv("/pitcher.csv", header = TRUE, quote = "\"", sep=";", dec = ".", check.names=TRUE)
Hitter <- read.csv("/Users/riccardosimionato/Music/iTunes/iTunes Media/Podcasts/hitter.csv", header = TRUE, quote = "\"", sep=";", dec = ".", check.names=TRUE)
#Team <- read.csv("/team.csv", header = TRUE, quote = "\"", sep=";", dec = ".", check.names=TRUE)


#Recode
Hitter$Name <- NULL

#Hitter[,22] <- NULL
#Hitter[,23] <- NULL

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

oldvalues <-       c("N", "A")
newvalues <-       c(0, 1)

Hitter$league_at_the_beginning  <- newvalues[ match(Hitter$league_at_the_beginning , oldvalues) ]

#team_at_the_beginning

oldvalues <-       c("N.Y.", "Phi.", "St.L.","Mon.", "Chi.", "Pit.", "Hou.", "Cin.", "S.F.", "S.D.", "L.A.", "Atl.", "Bos.", "Det.", "Tor.", "Cle.", "Mil.", "Bal.", "Cal.", "Tex.", "K.C.", "Oak.", "Min.", "Sea.")

newvalues <-       c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)

Hitter$team_at_the_beginning <- newvalues[ match(Hitter$team_at_the_beginning , oldvalues) ]

#Hitter$team_at_the_beginning <- NULL
# Preprocessing data

Hitter<- na.omit(Hitter)

#oneHotFeatures = c('team_at_the_end','position','team_at_the_beginning') # Encoding categorical vars with one-hot

#library(ade4)

#for (f in oneHotFeatures){
#  dummy = acm.disjonctif(Hitter2[f])
#  Hitter2[f] = NULL
#  Hitter2 = cbind(Hitter2, dummy)
#}
#write.csv(Hitter2, file="hitter666.csv", row.names = FALSE, header = TRUE) 
#Hitter <- read.csv("hitter666.csv", header = TRUE, quote = "\"", sep=",", dec = ".", check.names=TRUE)


target <- Hitter$Annual_salary
target <- as.numeric(target)
target.log <- log10(target)

#Hitter666 <- data.frame(cbind(scale(Hitter[,1:20]), target.log))
#Hitter666 <- data.frame(cbind(scale(Hitter$Nputouts), scale(Hitter$Nwalks), scale(Hitter$Nhits), scale(Hitter$Nbat_career), 
 #                             scale(Hitter$Nhits_career), scale(Hitter$Nruns_career), 
  #                            scale(Hitter$Nwalks_career), scale(Hitter$Nrunsbatted), scale(Hitter$Nrunsbatted_career), 
   #                           scale(Hitter$Nyears_in_the_major_leagues), scale(Hitter$Nruns), 
    #                          scale(Hitter$Nbat), scale(Hitter$position), 
     #                         scale(Hitter$Nhomeruns), scale(Hitter$team_at_the_end), 
      #                        scale(Hitter$Nhomeruns_career), scale(Hitter$Nassists),
       #                       scale(Hitter$team_at_the_beginning), scale(Hitter$Nerrors), scale(Hitter$league_at_the_end), 
        #                      target.log))



names(Hitter666)[1] <- 'Nputouts'
names(Hitter666)[2] <- 'Nwalks'
names(Hitter666)[3] <- 'Nhits'
names(Hitter666)[4] <- 'Nbat_career'
names(Hitter666)[5] <- 'Nhits_career'
names(Hitter666)[6] <- 'Nruns_career'
names(Hitter666)[7] <- 'Nwalks_career'
names(Hitter666)[8] <- 'Nrunsbatted'                 
names(Hitter666)[9] <- 'Nrunsbatted_career'        
names(Hitter666)[10] <- 'Nyears_in_the_major_leagues'
names(Hitter666)[11] <- 'Nruns'       
names(Hitter666)[12] <- 'Nbat'                     
names(Hitter666)[13] <- 'position' 
names(Hitter666)[14] <- 'Nhomeruns'
names(Hitter666)[15] <- 'team_at_the_end'         
names(Hitter666)[16] <- 'Nhomeruns_career'
names(Hitter666)[17] <- 'Nassists'
names(Hitter666)[18] <- 'team_at_the_beginning'
names(Hitter666)[19] <- 'Nerrors'
names(Hitter666)[20] <- 'league_at_the_end'
names(Hitter666)[21] <- 'Annual_salary'

#Hitter666 <- data.frame(cbind(scale(Hitter$Nputouts), scale(Hitter$Nbat_career), scale(Hitter$Nwalks), scale(Hitter$Nhits), 
 #                                                          scale(Hitter$Nhits_career), scale(Hitter$Nrunsbatted), 
  #                                                        scale(Hitter$Nruns_career), scale(Hitter$Nrunsbatted_career), scale(Hitter$Nwalks_career), 
   #                                                     scale(Hitter$Nruns), scale(Hitter$Nyears_in_the_major_leagues), 
    #                                                    scale(Hitter$Nhomeruns), scale(Hitter$Nbat), 
     #                                                  scale(Hitter$team_at_the_end), scale(Hitter$position), 
      #                                                scale(Hitter$Nhomeruns_career), scale(Hitter$Nassists),
       #                                              scale(Hitter$team_at_the_beginning), scale(Hitter$league_at_the_end), scale(Hitter$league_at_the_beginning), 
        #                                              scale(Hitter$Nerrors), scale(Hitter$division_at_the_end),
         #                                           target.log))
                              
#names(Hitter666)[1] <- 'Nputouts'
#names(Hitter666)[2] <- 'Nbat_career'
#names(Hitter666)[3] <- 'Nwalks'
#names(Hitter666)[4] <- 'Nhits'
#names(Hitter666)[5] <- 'Nhits_career'
#names(Hitter666)[6] <- 'Nrunsbatted'
#names(Hitter666)[7] <- 'Nruns_career'
#names(Hitter666)[8] <- 'Nrunsbatted_career'                 
#names(Hitter666)[9] <- 'Nwalks_career'        
#names(Hitter666)[10] <- 'Nruns'
#names(Hitter666)[11] <- 'Nyears_in_the_major_leagues'       
#names(Hitter666)[12] <- 'Nhomeruns'                     
#names(Hitter666)[13] <- 'Nbat' 
#names(Hitter666)[14] <- 'team_at_the_end'
#names(Hitter666)[15] <- 'position'         
#names(Hitter666)[16] <- 'Nhomeruns_career'
#names(Hitter666)[17] <- 'Nassists'
#names(Hitter666)[18] <- 'team_at_the_beginning'
#names(Hitter666)[19] <- 'league_at_the_end'
#names(Hitter666)[20] <- 'league_at_the_beginning'
#names(Hitter666)[21] <- 'Nerrors'
#names(Hitter666)[22] <- 'division_at_the_end'
#names(Hitter666)[23] <- 'Annual_salary'

HitterVal <- Hitter666[1:213,]
TestSet <- Hitter666[214:263,]

#colnames(Hitter[,21]) <- c("Log_Salary_1987")
write.csv(HitterVal, file="Val.csv", row.names = FALSE)
write.csv(TestSet, file="Test.csv", row.names = FALSE)

