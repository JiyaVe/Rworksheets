############################
#ASSIGNMENTS
#######1

my.birthday <- function(n)
{
  days <- sample(1:365, n, replace = TRUE)
  
  if(max(table(days)) >= 2)
  {
    return(1)
  } else{
    return(0)
  }
}

matches= numeric(length = 1e3)
for( i in 1:1e3)
{
  matches[i] = birthday(n=50)
}
my.ans = mean(matches)







###########2





# Function for finding proportion of greys
my.prop.grey <- function(img)
{
  col.mat <- as.array(img[, ,1, ])
  dims <- dim(col.mat)
  grey <- matrix(0, nrow = dims[1], ncol = dims[2])
  
  # Have accepted both rounding strategies
  col.mat.round <- round(col.mat, 1)
  # col.mat.round <- round(col.mat)
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      foo <- col.mat.round[i, j, ]
      grey[i,j] <- (foo[1] == foo[2]) & (foo[1] == foo[3])
    }
  }
  
  return(mean(grey))
}







##################3


library(rvest)
library(tidyverse)

html <- read_html("https://bwf.tournamentsoftware.com/ranking/category.aspx?id=36352&category=473&C473FOC=&p=1&ps=100")

foo <- html %>% html_table()
foo <- foo[[1]]
foo <- foo[-101, -c(2,3,4,6)]
foo$Rank <- as.numeric(foo$Rank)

by_conf <- foo %>% group_by(Confederation)
MY.confed_summary <- by_conf %>% 
  summarize(Avg_rank = mean(Rank), number = n()) %>%
  arrange(Avg_rank)