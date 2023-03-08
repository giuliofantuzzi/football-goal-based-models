#_______________________________________________________________________________         
#        Author:    Giulio Fantuzzi
#       Created:    2023/02/28
# Last modified:    2023/03/02
#         About:    Implementation of Maher's Model for serieA 21-22 matches
#_______________________________________________________________________________         


#-------------------------------------------------------------------------------
# Import serie A 21-22 data (from: https://www.football-data.co.uk/italym.php)
serieA_2122<- read.csv("../data/serieA_21-22.csv")
# Feature selection
serieA_2122<- serieA_2122[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")]
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Import log-likelihood naif function
source("functions/Maher_naif_loglike.R")
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Parameters estimation

#Initial guess
parameters_guess= rep(1,40)#both attack and defense parameters (tot= 2n)
#NB: a zeros vector as initial guess would cause errors (in log(lambda) and log(mu))

params <- optim(parameters_guess, Maher_naif_loglike, data=serieA_2122)$par
#Dubbio: forse i parametri devono essere compresi tra 0 e 1???

#Parameters table
teams_list<- names(table(serieA_2122[,"HomeTeam"]))
alpha<- params[1:20]
beta<- params[21:40]
parameters_table<- data.frame(cbind(teams_list, alpha,beta))

#-------------------------------------------------------------------------------

# loglikelihood NOT NAIF function
# Create team dummies
source("functions/team_dummies.R")
serieA_2122<- team_dummies(serieA_2122)
source("functions/Maher_loglike.R")
params2 <- optim(parameters_guess, Maher_loglike, data=serieA_2122)$par
alpha2<- params2[1:20]
beta2<- params2[21:40]
parameters_table2<- data.frame(cbind(teams_list, alpha2,beta2))


# NON CAPISCO PERCHÈ LE 2 FUNZIONI PORTINO A PARAMETRI DIVERSI!
# INOLTRE ANALIZZANDO I VALORI DEI COEFFICIENTI CI SON COSE STRANE:

#-------------------------------------------------------------------------------
# 1)CON FUNZIONE NAIF:
# Dalla migliore alla peggiore per coefficiente d'attacco
parameters_table$teams[order(parameters_table$alpha,decreasing = TRUE)]
# Dalla migliore alla peggiore per coefficiente difensivo (NB: beta grande= debolezza difensiva, quindi beta piccolo= solidità difensiva)
parameters_table$teams[order(parameters_table$beta,decreasing = F)]

#Commenti: curioso come il Milan (vincitore del campionato) abbia un basso coefficiente di attacco
#          il parametro difensivo però risulta molto buono (la serieA infatti è un campionato abbastanza difensivo)

#NB: se guardo ai goal totali però il milan sarebbe 4 dopo napoli lazio e inter
#    la salernitana sembra avere una buona difesa, ma è stata la squadra che ha subito più goal di tutti
#    ----> o ste contraddizioni dipendono dalla semplicità del modello, o c'è un errore
# 2) CON FUNZIONE MIGLIORE:
# Dalla migliore alla peggiore per coefficiente d'attacco
parameters_table2$teams[order(parameters_table2$alpha2,decreasing = TRUE)]
# Dalla migliore alla peggiore per coefficiente difensivo (NB: beta grande= debolezza difensiva, quindi beta piccolo= solidità difensiva)
parameters_table2$teams[order(parameters_table2$beta2,decreasing = F)]


#LIMITE DEL MODELLO DI MAHER: non considera il fattore campo
# Ma ha senso introdurlo? Analizziamo meglio la questione nello script "home_effect.R"
