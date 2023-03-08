#_______________________________________________________________________________         
#        Author:    Giulio Fantuzzi
#       Created:    2023/03/07
# Last modified:    2023/03/07
#         About:    Analysis of home effect in football matches
#_______________________________________________________________________________         

# Import and select data
serieA_2122<- read.csv("../data/serieA_21-22.csv")
serieA_2122<- serieA_2122[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")]
teams_list<- names(table(serieA_2122[,"HomeTeam"]))

# Define variables for goals scored/conceeded Home/Away
TeamHomeGoalsScored<- vector(mode="numeric", length=length(teams_list))
TeamAwayGoalsScored<- vector(mode="numeric", length=length(teams_list))
TeamHomeGoalsConceeded<- vector(mode="numeric", length=length(teams_list))
TeamAwayGoalsConceeded<- vector(mode="numeric", length=length(teams_list))

for (i in 1:length(teams_list)){
    TeamHomeGoalsScored[i]<- sum(serieA_2122[serieA_2122$HomeTeam== teams_list[i],"FTHG"])
    TeamAwayGoalsScored[i]<- sum(serieA_2122[serieA_2122$AwayTeam==teams_list[i],"FTAG"])
    TeamHomeGoalsConceeded[i]<- sum(serieA_2122[serieA_2122$HomeTeam== teams_list[i],"FTAG"])
    TeamAwayGoalsConceeded[i]<- sum(serieA_2122[serieA_2122$AwayTeam==teams_list[i],"FTHG"])

}

# Create a dataframe
goalscored_df <- data.frame(Stadium=rep(c("Home", "Away"),each=20),
                     Teams=rep(teams_list,2),
                     Goals=c(TeamHomeGoalsScored,TeamAwayGoalsScored)
)

goalconceeded_df <- data.frame(Stadium=rep(c("Home", "Away"),each=20),
                               Teams=rep(teams_list,2),
                               Goals=c(TeamHomeGoalsConceeded,TeamAwayGoalsConceeded)
)

#-------------------------------------------------------------------------------
# Plots
library(ggplot2)
# 1) Goal scored Home vs Away
ggplot(goalscored_df, aes(x=Teams, y=Goals, fill=Stadium)) + 
    geom_bar(stat="identity", width=0.6, position=position_dodge(),colour="black")+
    scale_fill_manual(values = c("#145A32", "#52BE80"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,face="bold"))+
    ggtitle("GOAL SCORED SERIE A 21-22") +
    theme(plot.title = element_text(hjust = 0.5,face="bold"))
# 14 teams scored more Home; 4 scored more Away; 1 scored equally

# 2) Goal conceeded Home vs Away
ggplot(goalconceeded_df, aes(x=Teams, y=Goals, fill=Stadium)) + 
    geom_bar(stat="identity", width=0.6, position=position_dodge(),colour="black")+
    scale_fill_manual( values=c("#641E16", "#EC7063"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,face="bold"))+
    ggtitle("GOAL CONCEEDED SERIE A 21-22") +
    theme(plot.title = element_text(hjust = 0.5,face="bold"))
# 11 teams conceeded more Away, 6 conceeded more Home; 1 conceeded equally

#---> There's an evident home effect, and it has a bigger impact on the goal scored