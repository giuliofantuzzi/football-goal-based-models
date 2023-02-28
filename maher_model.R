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



#LIMITE DEL MODELLO DI MAHER: non considera il fattore campo
#Analizziamo meglio

# CONFRONTO SUI GOAL FATTI
TeamHomeGoals<- vector(mode="numeric", length=length(teams))
TeamAwayGoals<- vector(mode="numeric", length=length(teams))
# CONFRONTO SUI GOAL SUBITI
TeamHomeGoalsConceeded<- vector(mode="numeric", length=length(teams))
TeamAwayGoalsConceeded<- vector(mode="numeric", length=length(teams))

for (i in 1:length(teams)){
    TeamHomeGoals[i]<- sum(serieA_2122[serieA_2122$HomeTeam== teams[i],"FTHG"])
    TeamAwayGoals[i]<- sum(serieA_2122[serieA_2122$AwayTeam==teams[i],"FTAG"])
    TeamHomeGoalsConceeded[i]<- sum(serieA_2122[serieA_2122$HomeTeam== teams[i],"FTAG"])
    TeamAwayGoalsConceeded[i]<- sum(serieA_2122[serieA_2122$AwayTeam==teams[i],"FTHG"])
}

barplot(TeamHomeGoals, names.arg = teams, las=3, cex.names=0.8, col="dark green", main= "Goal scored")
barplot(TeamAwayGoals, names.arg = teams, las=3, cex.names=0.8, add=T, col="green4")
legend("topleft",c("Home", "Away"), col=c("dark green","green4"),fill=c("dark green","green4"))


#14 squadre su 20 hanno segnato maggiormente in casa

barplot(TeamHomeGoalsConceeded, names.arg = teams, las=3, cex.names=0.8, col="dark red", main= "Goal conceeded")
barplot(TeamAwayGoalsConceeded, names.arg = teams, las=3, cex.names=0.8, add=T, col="indianred1")
legend("topleft",c("Home", "Away"), col=c("dark red","indianred1"),fill=c("dark red","indianred1"))


#14 squadre su 20 hanno concesso meno goal in casa

#-->guardando questa stagione sembra chiaro l'effetto stadio, ed esso ha lo stesso effetto sia su goal fatti che goal subiti