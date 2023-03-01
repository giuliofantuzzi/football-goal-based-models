#_______________________________________________________________________________         
#        Author:    Giulio Fantuzzi
#       Created:    2023/03/01
# Last modified:    2023/03/01
#         About:    Implementation of Maher's log likelihood (naif approach)
#_______________________________________________________________________________         

Maher_naif_loglike<- function(parameters, data) {
    teams_list<- names(table(data[,"HomeTeam"]))
    n= length(parameters)/2
    loglike=0
    for(i in 1:n){
        for(j in 1:n){
            if(j != i){
                x_ij<- data[data$HomeTeam== teams_list[i] & data$AwayTeam== teams_list[j] ,"FTHG"]
                y_ij<- data[data$HomeTeam== teams_list[i] & data$AwayTeam== teams_list[j] ,"FTAG"]
                lambda<- parameters[i]* parameters[n+j]
                mu<- parameters[j] * parameters[n+i]
                loglike = loglike+ (log(dpois(x_ij,lambda)) + log(dpois(y_ij,mu)))
            }
        }
    }
    return(-loglike) #sign - because Max loglike is equal to Min -loglike
}

#NB: this function can only be applied to all-season data
#    Assumption that each team plays against all the other ones
#    This limit shoud be improved by defining dummy variable (see Maher_loglike.R)