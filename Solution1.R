#Q1
#Count Number of Students

seat <- read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")

seat

head(seat)

head(seat,3)

tail(seat,2)

summary(seat)
#summary of dataset

str(seat)
# structure of dataset
# $ represent column

n <- dim(seat)[1]
ms <- 0
bs <- 0

for(i in 1:n)
{
  if (seat$Roll.No[i] > 1000000 )
  {
    ms <- ms + 1
  }
  else
  {
    bs <- bs + 1
  }
}

bs # nmuber of bs students
ms # number of ms students

#Without using loop ( by truth values)

sum(seat$Roll.No > 1000000)
sum(seat$Roll.No < 1000000)
