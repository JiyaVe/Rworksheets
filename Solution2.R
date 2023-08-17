#Q2
#Cricketer Info

cricket <- read.csv("battingbowling.csv")

cricket

player <- subset(cricket, Bowling <= 40 & Batting >= 25)


n1 <- subset(player,Team == "India") 
n2 <- subset(player,Team == "Australia") 
n3 <- subset(player,Team == "England") 
n4 <- subset(player,Team == "New Zealand") 
n5 <- subset(player,Team == "Pakistan")
n6 <- subset(player,Team == "South Africa") 
  
n <- max(dim(n1)[1],dim(n2)[1],dim(n3)[1],dim(n4)[1],dim(n5)[1],dim(n6)[1])
m <- min(dim(n1)[1],dim(n2)[1],dim(n3)[1],dim(n4)[1],dim(n5)[1],dim(n6)[1])


if (dim(n1)[1] == n) {print(n1$Team[1])}
if (dim(n2)[1] == n) {print(n2$Team[1])}
if (dim(n3)[1] == n) {print(n3$Team[1])}
if (dim(n4)[1] == n) {print(n4$Team[1])}
if (dim(n5)[1] == n) {print(n5$Team[1])}
if (dim(n6)[1] == n) {print(n6$Team[1])}

if (dim(n1)[1] == m) {print(n1$Team[1])}
if (dim(n2)[1] == m) {print(n2$Team[1])}
if (dim(n3)[1] == m) {print(n3$Team[1])}
if (dim(n4)[1] == m) {print(n4$Team[1])}
if (dim(n5)[1] == m) {print(n5$Team[1])}
if (dim(n6)[1] == m) {print(n6$Team[1])}


cricket <- read.csv("battingbowling.csv")


## Problem 2(a)
allround_index <- (cricket$Batting > 25 & cricket$Bowling < 40)
allround <- cricket[allround_index, ]
allround

tab <- table(allround$Team)
which.min(tab)
which.max(tab)
