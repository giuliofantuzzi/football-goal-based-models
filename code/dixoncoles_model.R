#_______________________________________________________________________________         
#        Author:    Giulio Fantuzzi
#       Created:    2023/03/02
# Last modified:    2023/03/02
#         About:    Implementation of Dixon and Coles Model for serieA 21-22 matches
#_______________________________________________________________________________         


#-------------------------------------------------------------------------------
# Import serie A 21-22 data (from: https://www.football-data.co.uk/italym.php)
serieA_2122<- read.csv("../data/serieA_21-22.csv")
# Feature selection
serieA_2122<- serieA_2122[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")]
#-------------------------------------------------------------------------------

# Add teams dummies to dataset
source("functions/team_dummies.R")
serieA_2122<- team_dummies(serieA_2122)

# Import Dixon and Coles tau function
source("functions/DixonColes_tau.R")
# Import Dixon and Coles loglike function
source("functions/DixonColes_loglike.R")
#-------------------------------------------------------------------------------

# Parameters estimation

#Initial guess
parameters_guess= c(rep(1,41),0.5)#(tot= 2n+2) NB: rho non può essere 1 sennò log tau produce NA
#NB: a zeros vector as initial guess would cause errors (in log(lambda) and log(mu))

params <- optim(parameters_guess, DixonColes_loglike, data=serieA_2122)$par 
#NB: qui però non sto usando i vincoli!
#al problema che la media degli alpha deve essere 1 forse posso rimediare facendo una riparametrizzazione
#c'è anche il vincolo su rho!!!

#Parameters table
teams_list<- names(table(serieA_2122[,"HomeTeam"]))
alpha<- params[1:20]
beta<- params[21:40]
gamma< params[41]
rho<- params[42]
parameters_table<- data.frame(cbind(teams_list, alpha,beta))
