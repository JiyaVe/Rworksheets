#Q2
#Cricketer Info

cricket <- read.csv("battingbowling.csv")

cricket

player <- subset(cricket, Bowling <= 40 & Batting >= 25)

player

team1 <- subset(player, Team == "India")
team2 <- subset(player, Team == "Australia")
team3 <- subset(player, Team == "England")
team4 <- subset(player, Team == "New Zealand")
team5 <- subset(player, Team == "Pakistan")
team6 <- subset(player, Team == "South Africa")

for (i in c(team1,team2,team3,team4,team5,team6))
{
  if( dim(i + 1)[1] > dim(i)[1] ) 
    max <- i
}