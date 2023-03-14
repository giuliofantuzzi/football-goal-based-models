#_______________________________________________________________________________         
#        Author:    Giulio Fantuzzi
#       Created:    2023/03/09
# Last modified:    2023/03/10
#         About:    Analysis by using "FootBayes" package
#_______________________________________________________________________________         


#-------------------------------------------------------------------------------
# Import libraries
library(footBayes)
library(engsoccerdata)
library(tidyverse)

#-------------------------------------------------------------------------------
# Import 2020/2021 data
serieA_2122<- read.csv("../data/serieA_21-22.csv")
# Feature selection
serieA_2122<- serieA_2122[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG")]
# Change dataframe format to apply footBayes package
serieA_2122<- data.frame(cbind(rep(2010,380),serieA_2122))
colnames(serieA_2122)= c("Season", "home", "visitor", "hgoal", "vgoal")
#-------------------------------------------------------------------------------
fit_mle<- mle_foot(data = serieA_2122,
                   model="biv_pois",
                   interval = "Wald")

#Plot teams coefficients
foot_abilities(object = fit_mle, data = serieA_2122,type = c("attack"))
foot_abilities(object = fit_mle, data = serieA_2122,type = c("defense"))
#attack parameters (strenght in attack)
sort(fit_mle$att[,2], decreasing = T)
#defense parameters (team weakness in defense)
sort(fit_mle$def[,2], decreasing = F)
#-------------------------------------------------------------------------------
