#Q1
#dplyr

library(dplyr)
library(tidyverse)
library(rvest)

html <- read_html("https://www.icc-cricket.com/rankings/womens/player-rankings/odi/batting")
table <- html %>% html_table()
table <- html_table(html)

class(table)
length(table)


batting <- table[[1]]
class(batting)

batting$Pos <- 1:100

batting

#Q6
#(a)

india <- batting %>% filter(Team == "IND")
india %>% summarise(n=n())#n()counting function and n in the name we gave to the column

#(b)

by_country <- batting %>% group_by(Team)
by_country %>% summarise(n_p = n())

#(c)
Rank <- by_country %>% summarise(avg_rank = mean(Pos),avg_rating = mean(Rating))

#(d)
arrange(Rank,avg_rank,avg_rating)
Rank %>% arrange(avg_rank,avg_rating)

#Q2
#Done

#Q3
#filter rows
starwars %>% filter(skin_color == "light", eye_color == "brown") 

#arrange rows
starwars %>% arrange(height,mass)# we used mass as tiebreaker

#to arrange in descending order
starwars %>% arrange(desc(height),mass)

#Choose rows using their position with slice()
starwars %>% slice(5:10) #5 and 10 included

slice_head(starwars,n=3)# select 3 from the starting
slice_tail(starwars,n=3)#select 3 from the ending



