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
    n<- (length(parameters)-2)/2
    #------------------------------
    # apply here date filter!!!
    #-----------------------------
    alpha= parameters[1:n]
    beta= parameters[(n+1):(2*n)]
    gamma=parameters[2*n+1]
    rho= parameters[2*n+2]
    #-----------------------------
    #Apply costraints (CAAPIRE PERCHE NON FUNZIONA!!!)
    #1) media coefficienti = 0 (ANCHE DANDOLO DICE MEGLIO MEDIA 0 RISPETTO A 1)
    #alpha[n]= - sum(alpha[1:n-1])
    #COSI NON RUNNA NEMMENO
    #2) media coefficienti = 1 (come dicono in dixon coles)
    #alpha[n]= n- sum(alpha[1:n-1])
    #beta[n]= n- sum(beta[1:n-1])
    #COSI RUNNA MA POI SI BLOCCA
    #3) media coeff =0 alternativo
    #alpha= alpha- mean(alpha)
    #beta= beta- mean(beta)
    #--->poi dipende tutto dall'initial guess...ma se metto ora il vincolo poi dpois facilmente da errore!!!
    #-----------------------------
    n_rows<- dim(data)[1] 
    loglike<- 0
    for (k in 1:n_rows){
        x<- data[k,"FTHG"]
        y<- data[k,"FTAG"]
        lambda<-alpha[data[k,"HomeDummy"]]* beta[data[k,"AwayDummy"]]*gamma
        mu<- alpha[data[k,"AwayDummy"]]* beta[data[k,"HomeDummy"]]
        
        loglike = loglike+log( DixonColes_tau(x,y,lambda,mu,rho))-lambda-mu+x*log(lambda)+y*log(mu)
        }
    return(-loglike) #sign - because Max loglike is equal to Min -loglike
}