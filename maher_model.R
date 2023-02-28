#_______________________________________________________________________________         
#        Author:    Giulio Fantuzzi
#       Created:    2022/08/02
# Last modified:    2023/02/23
#         About:    Implementation of Maher's Model for seriaA 21-22 matches
#_______________________________________________________________________________         


#-------------------------------------------------------------------------------
# Import serie A 21-22 data (from: https://www.football-data.co.uk/italym.php)
serieA_2122<- read.csv("data/serieA_21-22.csv")
# Feature selection
serieA_2122<- serieA_2122[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")]
teams<- names(table(serieA_2122[,"HomeTeam"]))
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# log-likelihood function
loglike_Maher <- function(parameters, data) {
    n= length(parameters)/2
    loglike=0
    for(i in 1:n){
        for(j in 1:n){
            if(j != i){
                x_ij<- data[data$HomeTeam== teams[i] & data$AwayTeam== teams[j] ,"FTHG"]
                y_ij<- data[data$HomeTeam== teams[i] & data$AwayTeam== teams[j] ,"FTAG"]
                lambda<- parameters[i]* parameters[n+j]
                mu<- parameters[j] * parameters[n+i]
                loglike = loglike+ (x_ij* log(lambda)- lambda - log(factorial(x_ij)) + y_ij* log(mu) - mu - log(factorial(y_ij)))
            }
        }
    }
    return(-loglike) #sign - because Max loglike is equal to Min -loglike
}
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Parameters estimation

#Initial guess
parameters_guess= rep(1,40) #both attack and defense parameters (tot= 2n)
#NB: a zeros vector as initial guess would cause errors (in log(lambda) and log(mu))

params <- optim(parameters_guess, loglike_Maher, data=serieA_2122)$par
#Dubbio: forse i parametri devono essere compresi tra 0 e 1???

#Parameters table
alpha<- params[1:20]
beta<- params[21:40]
parameters_table<- data.frame(cbind(teams, alpha,beta))
#-------------------------------------------------------------------------------

#ANALISI SULLE SQUADRE
# Dalla migliore alla peggiore per coefficiente d'attacco
parameters_table$teams[order(parameters_table$alpha,decreasing = TRUE)]

# Dalla migliore alla peggiore per coefficiente difensivo (NB: beta grande= debolezza difensiva, quindi beta piccolo= solidità difensiva)
parameters_table$teams[order(parameters_table$beta,decreasing = F)]


#Commenti: curioso come il Milan (vincitore del campionato) abbia un basso coefficiente di attacco
#          il parametro difensivo però risulta molto buono (la serieA infatti è un campionato abbastanza difensivo)


#NB: se guardo ai goal totali però il milan sarebbe 4 dopo napoli lazio e inter
#    la salernitana sembra avere una buona difesa, ma è stata la squadra che ha subito più goal di tutti
#    ----> o ste contraddizioni dipendono dalla semplicità del modello, o c'è un errore
