#_______________________________________________________________________________         
#        Author:    Giulio Fantuzzi
#       Created:    2023/03/02
# Last modified:    2023/03/02
#         About:    Implementation of Dixon and Coles log likelihood.
#                   Appliable to a partial section of the league
#                   Costraints for team's coefficients a1,...an, b1,...,bn
#                   mean(a) = mean(b) = 0
#_______________________________________________________________________________         

DixonColes_costrained_loglike<- function(parameters,data){
    #parameters of the type [a1,...,an,b1,...,bn,gamma,rho]
    n= (length(parameters)-2)/2
    alpha= parameters[1:n]
    beta= parameters[(n+1):(2*n)]
    gamma=parameters[2*n+1]
    rho= parameters[2*n+2]
    n_rows<- dim(data)[1]
    loglike=0
    #-----------------------------
    if(!all(is.nan(alpha)) && mean(alpha)==0){ 
        for (k in 1:n_rows){
            x<- data[k,"FTHG"]
            y<- data[k,"FTAG"]
            lambda<-alpha[data[k,"HomeDummy"]]* beta[data[k,"AwayDummy"]]*gamma
            mu<- alpha[data[k,"AwayDummy"]]* beta[data[k,"HomeDummy"]]
            loglike = loglike+log( DixonColes_tau(x,y,lambda,mu,rho))-lambda-mu+x*log(lambda)+y*log(mu)
        }
        return(-loglike)
    }
    else{
        return(-Inf)
    }
}

