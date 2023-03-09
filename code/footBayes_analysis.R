#_______________________________________________________________________________         
#        Author:    Giulio Fantuzzi
#       Created:    2023/03/09
# Last modified:    2023/03/09
#         About:    Analysis by using "FootBayes" package
#_______________________________________________________________________________         


#-------------------------------------------------------------------------------
# Import libraries
library(footBayes)
library(engsoccerdata)
library(tidyverse)

# Import serieA 2021/2022 data
data(italy)
italy <- as.data.frame(italy)
italy_2010 <- subset(italy[, c(2,3,4,6,7)],
                     Season =="2010")
#-------------------------------------------------------------------------------

# Fit model by maximum likelihood estimation
fit1_mle <- mle_foot(data = italy_2010,
                     model="biv_pois",
                     interval = "Wald") # mle biv poisson

#Plot team's coefficients
foot_abilities(object = fit1_mle, data = italy_2010, cex.var = 1)

# Dynamic approach-->coefficients change over time
fit1_stan <- stan_foot(data = italy_2010,
                       model="biv_pois",
                       dynamic_type ="weekly",
                       iter=40)

foot_abilities(fit1_stan, italy_2010)
