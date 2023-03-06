#_______________________________________________________________________________         
#        Author:    Giulio Fantuzzi
#       Created:    2023/03/02
# Last modified:    2023/03/02
#         About:    Implementation of Dixon and Coles Tau funcions.
#                   Used to correct the correlation of "few-goals" matches.
#_______________________________________________________________________________         

DixonColes_tau<- function(x,y,lambda, mu, rho){
    if (x==0 & y==0){return(1- lambda*mu*rho)}
    else if (x==0 & y==1){return(1+lambda*rho)}
    else if (x==1 & y==0){return(1+mu*rho)}
    else if (x==1 & y==1){return(1-rho)}
    else{return(1)}
}

