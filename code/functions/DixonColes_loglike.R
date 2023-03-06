#_______________________________________________________________________________         
#        Author:    Giulio Fantuzzi
#       Created:    2023/03/02
# Last modified:    2023/03/02
#         About:    Implementation of Dixon and Coles log likelihood.
#                   Appliable to a partial section of the league
#_______________________________________________________________________________         

DixonColes_loglike<- function(parameters,data){
    #parameters of the type [a1,...,an,b1,...,bn,gamma,rho]
    n_teams= (length(parameters)-2)/2
    #------------------------------
    # apply here date filter!!!
    #-----------------------------
    n_rows<- dim(data)[1] 
    loglike<- 0
    for (k in 1:n_rows){
        x<- data[k,"FTHG"]
        y<- data[k,"FTAG"]
        lambda<- parameters[data[k,"HomeDummy"]]* parameters[n_teams+data[k,"AwayDummy"]*parameters[length(parameters)-1]]
        mu<- parameters[data[k,"AwayDummy"]]* parameters[n_teams+data[k,"HomeDummy"]]
        rho<- parameters[length(parameters)]
        loglike = loglike+ (log(DixonColes_tau(x,y,lambda,mu,rho))+log(dpois(x,lambda)) + log(dpois(y,mu)))
    }
    return(-loglike) #sign - because Max loglike is equal to Min -loglike
}