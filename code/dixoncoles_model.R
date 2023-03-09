#_______________________________________________________________________________         
#        Author:    Giulio Fantuzzi
#       Created:    2023/03/02
# Last modified:    2023/03/08
#         About:    Implementation of Dixon and Coles Model for serieA 21-22 matches
#_______________________________________________________________________________         


#-------------------------------------------------------------------------------
# Import serie A 21-22 data (from: https://www.football-data.co.uk/italym.php)
serieA_2122<- read.csv("../data/serieA_21-22.csv")
# Feature selection
serieA_2122<- serieA_2122[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")]
# List of team names
teams_list<- names(table(serieA_2122[,"HomeTeam"]))
#-------------------------------------------------------------------------------

# Add teams dummies to dataset
source("functions/team_dummies.R")
serieA_2122<- team_dummies(serieA_2122)

# Import Dixon and Coles tau function
source("functions/DixonColes_tau.R")
# Import Dixon and Coles loglike function
source("functions/DixonColes_loglike.R")

#-------------------------------------------------------------------------------

# Parameters estimation (UNCONSTRAINED OPTIMIZATION)

#Initial guess
parameters_guess= c(rep(1,41),0.5)#(tot= 2n+2)
#NB: rho cannot be 1, bc log tau would produce NA
#NB: a zeros vector as initial guess would cause errors (in log(lambda) and log(mu))
params_unc <- nlminb(parameters_guess, DixonColes_loglike, data=serieA_2122)$par 
# by using optim(), parameters didn't appear plausible
# by using nlminb(), instead, they seem much more convincing (as in Maher's Model)
#Actually specifying method="BFGS" in optim the result is similar
params_unc_optim <- optim(parameters_guess, DixonColes_loglike, data=serieA_2122, method="L-BFGS-B")$par 
comparison_table<- cbind(params_unc,params_unc_optim)

#Parameters table
alpha_unc<- params_unc[1:20]
beta_unc<- params_unc[21:40]
gamma_unc<- params_unc[41] #>0, it confirms the positive effect of playing Home
rho_unc<- params_unc[42]
parameters_table_unc<- data.frame(cbind(teams_list, alpha_unc,beta_unc))

# Drawback: in the optimization phase we did not consider some parameters costraints
# Dixon & Coles stated the constraints mean(alpha)=0 and mean(beta)=0
# Applying costraints in the log-likelihood causes some warnings...

# Trying with a re-parametrization of the coefficients (ask if it's an acceptable procedure)
alpha_unc_ripar<- alpha_unc-mean(alpha_unc)
beta_unc_ripar<-  beta_unc-mean(beta_unc)
# Parameters order does not change (obvious, we applied just a translation)
# FROM BETTER ATTACK TO THE WORST
attack_table= cbind(teams_list,alpha_unc_ripar)
attack_table[order(-alpha_unc_ripar),]
# FROM BETTER DEFENCE TO THE WORST (Remember that beta is a measure of defence weakness)
defence_table= cbind(teams_list,beta_unc_ripar)
defence_table[order(beta_unc_ripar),]
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# PARTE CRITICA !!!
# Parameters estimation (CONSTRAINED OPTIMIZATION)
# Import costrained verision of Dixon and Coles loglike function
source("functions/DixonColes_costrained_loglike.R")
parameters_guess= params_unc
params_con <- nlminb(start=parameters_guess,
                     objective= DixonColes_costrained_loglike,data=serieA_2122)$par
#qui non carica nemmeno....restituisce gli stessi parametri. MHHHHHH
#errori...si producono degli NA---> ovvio, mu=aj*bi, ma se uno fosse <0 diventa mu negativo e il log non funziona!
#-------------------------------------------------------------------------------
