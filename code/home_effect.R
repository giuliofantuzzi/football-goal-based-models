#_______________________________________________________________________________         
#        Author:    Giulio Fantuzzi
#       Created:    2023/03/07
# Last modified:    2023/03/08
#         About:    Analysis of home effect in football matches
#_______________________________________________________________________________         

#-------------------------------------------------------------------------------
# Import and select data
serieA_2122<- read.csv("../data/serieA_21-22.csv")
serieA_2122<- serieA_2122[,c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")]
teams_list<- names(table(serieA_2122[,"HomeTeam"]))
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
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
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Create dataframes
goalscored_df <- data.frame(Stadium=rep(c("Home", "Away"),each=20),
                     Teams=rep(teams_list,2),
                     Goals=c(TeamHomeGoalsScored,TeamAwayGoalsScored)
)

goalconceeded_df <- data.frame(Stadium=rep(c("Home", "Away"),each=20),
                               Teams=rep(teams_list,2),
                               Goals=c(TeamHomeGoalsConceeded,TeamAwayGoalsConceeded)
)
#-------------------------------------------------------------------------------

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
#-------------------------------------------------------------------------------
#---> There's an evident home effect, and it has a bigger impact on the goal scored
#-------------------------------------------------------------------------------

#Another way to show this is plot goal difference Home - Away

#-------------------------------------------------------------------------------
# Dataframes
goal_scored_difference<- TeamHomeGoalsScored- TeamAwayGoalsScored
# Creazione di un dataframe di esempio
goal_scored_difference_df <- data.frame(
    teams_list,
    goal_scored_difference
)
goal_conceeded_difference<- TeamHomeGoalsConceeded- TeamAwayGoalsConceeded
# Creazione di un dataframe di esempio
goal_conceeded_difference_df <- data.frame(
    teams_list,
    goal_conceeded_difference
)
# Creazione della variabile per il colore
goal_scored_difference_df$colore <- ifelse(goal_scored_difference_df$goal_scored_difference >= 0, 
                                           "#52BE80", "#EC7063")
goal_conceeded_difference_df$colore <- ifelse(goal_conceeded_difference_df$goal_conceeded_difference >= 0, 
                                              "#EC7063", "#52BE80")
#-------------------------------------------------------------------------------
# Plots
# Goal scored difference Home vs Away
ggplot(goal_scored_difference_df, aes(x = teams_list, y = goal_scored_difference, fill = colore)) +
    geom_bar(stat = "identity", width=0.6, position=position_dodge(),colour="black") +
    scale_fill_identity() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,face="bold"))+
    labs(title = "Goal scored difference Home vs Away",
         x = "", 
         y = "Goal scored difference")+
    theme(plot.title = element_text(hjust = 0.5,face="bold"))

# Goal conceeded difference Home vs Away
ggplot(goal_conceeded_difference_df, aes(x = teams_list, y = goal_conceeded_difference, fill = colore)) +
    geom_bar(stat = "identity", width=0.6, position=position_dodge(),colour="black") +
    scale_fill_identity() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,face="bold"))+
    labs(title = "Goal conceeded difference Home vs Away",
         x = "", 
         y = "Goal conceeded difference")+
    theme(plot.title = element_text(hjust = 0.5,face="bold"))
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Analisi dei risultati utili in casa vs fuori casa
serieA_2122$useful<-  (serieA_2122$FTR=="H") | (serieA_2122$FTR=="D")
useful_results_home<- vector(mode="numeric", length=length(teams_list))
useful_results_away<- vector(mode="numeric", length=length(teams_list))
for (i in 1:length(teams_list)){
    useful_results_home[i]<- sum(serieA_2122[serieA_2122$HomeTeam== teams_list[i],"useful"])
    useful_results_away[i]<- sum(!serieA_2122[serieA_2122$AwayTeam== teams_list[i],"useful"])
}

useful_results_df <- data.frame(Stadium=rep(c("Home", "Away"),each=20),
                            Teams=rep(teams_list,2),
                            Result=c(useful_results_home,useful_results_away)
)

ggplot(useful_results_df, aes(x=Teams, y=Result, fill=Stadium)) + 
    geom_bar(stat="identity", width=0.6, position=position_dodge(),colour="black")+
    scale_fill_manual(values = c("#EC7063", "#52BE80"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,face="bold"))+
    ggtitle("USEFUL RESULTS SERIE A 21-22") +
    theme(plot.title = element_text(hjust = 0.5,face="bold"))

#Oppure con la differenza

useful_result_difference<- useful_results_home- useful_results_away
# Creazione di un dataframe di esempio
useful_result_difference_df <- data.frame(
    teams_list,
    useful_result_difference
)

useful_result_difference_df$Better<- ifelse(useful_result_difference_df$useful_result_difference > 0, 
                                           "Home", "Away")

#Useful result difference Home vs Away
ggplot(useful_result_difference_df, aes(x = teams_list, y = useful_result_difference, fill = Better)) +
    geom_bar(stat = "identity", width=0.6, position=position_dodge(),colour="black") +
    scale_fill_manual(values = c("#EC7063", "#52BE80"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,face="bold"))+
    labs(title = "Useful result difference Home vs Away",
         x = "", 
         y = "Useful result difference")+
    theme(plot.title = element_text(hjust = 0.5,face="bold"))
