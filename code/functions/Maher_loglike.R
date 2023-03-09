#_______________________________________________________________________________         
#        Author:    Giulio Fantuzzi
#       Created:    2023/03/01
# Last modified:    2023/03/01
#         About:    Implementation of Maher's log likelihood. This function can
#                   be applied to a partial section of the league
#_______________________________________________________________________________         

Maher_loglike<- function(parameters, data) {
    n= length(parameters)/2
    #------------------------------
    # apply here date filter!!!
    #-----------------------------
    alpha= parameters[1:n]
    beta= parameters[(n+1):(2*n)]
    n_rows<- dim(data)[1] 
    loglike<- 0
    for (k in 1:n_rows){
        x<- data[k,"FTHG"]
        y<- data[k,"FTAG"]
        lambda<- alpha[data[k,"HomeDummy"]]* beta[data[k,"AwayDummy"]]
        mu<- alpha[data[k,"AwayDummy"]]* beta[data[k,"HomeDummy"]]
        loglike = loglike+ (log(dpois(x,lambda)) + log(dpois(y,mu)))
    }
    return(-loglike) #sign - because Max loglike is equal to Min -loglike
}