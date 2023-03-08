#_______________________________________________________________________________         
#        Author:    Giulio Fantuzzi
#       Created:    2023/03/02
# Last modified:    2023/03/07
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

# List of team names
teams_list<- names(table(serieA_2122[,"HomeTeam"]))
#-------------------------------------------------------------------------------

# Parameters estimation (UNCONSTRAINED OPTIMIZATION)

#Initial guess
parameters_guess= c(rep(1,41),0.5)#(tot= 2n+2)
#NB: rho cannot be 1, bc log tau would produce NA
#NB: a zeros vector as initial guess would cause errors (in log(lambda) and log(mu))
params_unc <- nlminb(parameters_guess, DixonColes_loglike, data=serieA_2122)$par 
#NB: qui non abbiamo usato i vincoli
#usando nlminb anziché optim i parametri ottenuti sembrano MOLTO più credibili

#Parameters table
alpha_unc<- params_unc[1:20]
beta_unc<- params_unc[21:40]
gamma_unc<- params_unc[41]
rho_unc<- params_unc[42]
parameters_table_unc<- data.frame(cbind(teams_list, alpha_unc,beta_unc))
#-------------------------------------------------------------------------------

# Parameters estimation (CONSTRAINED OPTIMIZATION)
# Import costrained verision of Dixon and Coles loglike function
source("functions/DixonColes_costrained_loglike.R")
#equal constraints
DCattackConstr <- function(parameters,data){
    n= (length(parameters)-2)/2
    attack.p <- matrix(parameters[1:n], ncol=1)
    return(sum(attack.p)-0)
}

params_con <- auglag(par=params_unc, fn=DixonColes_costrained_loglike, heq=DCattackConstr,data=serieA_2122)$par
#qui carica per un po e poi si blocca....stima lo stesso dei parametri

#Parameters table
alpha_con<- params_con[1:20]
beta_con<- params_con[21:40]
gamma_con<- params_con[41]
rho_con<- params_con[42]
parameters_table_con<- data.frame(cbind(teams_list, alpha_con,beta_con))

#--------------------------------------------------------------------------------





#Se con la constraint non riusciamo a ottenere qualcosa, riparametrizzo alpha e beta
alpha_unc_ripar<- alpha_unc-mean(alpha_unc)
beta_unc_ripar<-  beta_unc-mean(beta_unc)
#Ovviamente se li ordino ottengo la stessa cosa di prima (qui ho solo fatto una traslazione)
# Però ho effettivamente valori sia >0 che <0
#-------------------------------------------------------------------------

# FROM BETTER ATTACK TO THE WORST
attack_table= cbind(teams_list,alpha_unc_ripar)
attack_table[order(-alpha_unc_ripar),]
# FROM BETTER DEFENCE TO THE WORST
defence_table= cbind(teams_list,beta_unc_ripar)
defence_table[order(beta_unc_ripar),]


