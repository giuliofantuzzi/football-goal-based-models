#_______________________________________________________________________________         
#        Author:    Giulio Fantuzzi
#       Created:    2023/02/28
# Last modified:    2023/03/08
#         About:    Implementation of Maher's Model for serieA 21-22 matches
#_______________________________________________________________________________         


#-------------------------------------------------------------------------------
# Import serie A 21-22 data (from: https://www.football-data.co.uk/italym.php)
serieA_2122<- read.csv("../data/serieA_21-22.csv")
# Feature selection
serieA_2122<- serieA_2122[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")]
teams_list<- names(table(serieA_2122[,"HomeTeam"]))
#-------------------------------------------------------------------------------
# Prepare data and import log likelihood function
# Create team dummies
source("functions/team_dummies.R")
serieA_2122<- team_dummies(serieA_2122)
source("functions/Maher_loglike.R")
#-------------------------------------------------------------------------------
# Parameters estimation
#Initial guess
parameters_guess= rep(1,40)#both attack and defense parameters (tot= 2n)
#NB: a zeros vector as initial guess would cause errors (in log(lambda) and log(mu))

params <- optim(parameters_guess, Maher_loglike, data=serieA_2122)$par
#Dubbio: forse i parametri devono essere compresi tra 0 e 1???

#Parameters table
alpha<- params[1:20]
beta<- params[21:40]
parameters_table<- data.frame(cbind(teams_list, alpha,beta))

#-------------------------------------------------------------------------------
# FROM BETTER ATTACK TO THE WORST
attack_table= cbind(teams_list,alpha)
attack_table[order(-alpha),]
# FROM BETTER DEFENCE TO THE WORST
defence_table= cbind(teams_list,beta)
defence_table[order(beta),]
#-------------------------------------------------------------------------------
#Comment: basing on serieA 20-21 final ranking, some coefficients appears strange
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Try to use another optimization function: nlminb()
params <- nlminb(start=parameters_guess, objective=Maher_loglike, data=serieA_2122)$par
#Parameters table
alpha<- params[1:20]
beta<- params[21:40]
parameters_table<- data.frame(cbind(teams_list, alpha,beta))
# FROM BETTER ATTACK TO THE WORST
attack_table= cbind(teams_list,alpha)
attack_table[order(-alpha),]
# FROM BETTER DEFENCE TO THE WORST
defence_table= cbind(teams_list,beta)
defence_table[order(beta),]
#-------------------------------------------------------------------------------
#Comment: parameters estimated by nlminb seem much more reasonable yhen using optim!
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# DRAWBACK: This model doesn't consider a "home-effect"
# Should we introduce a "home-effect"? Check the script "home_effect.R"