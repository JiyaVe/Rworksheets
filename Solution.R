#Worksheet5
 library(profvis)
 
   attempts <- function(age)
   {
     count <- 0
     remain <- age # age no. of candles remain in the beginning
     while(remain > 0)
     {
       count <- count + 1
       # randomly choose any number between 1 and remain
       blow_out <- sample(1:remain, size = 1)
       remain <- remain - blow_out
     }
     return(count)
   }
   att_vec <- numeric(length = 1e3)
   for(i in 1:1e3)
   {
     att_vec[i] <- attempts(25)
   }
 mean(att_vec)
 att_vec2 <- replicate(1e3, attempts(25))
mean(att_vec2) 

library(rbenchmark)

#Q2

benchmark({
  att_vec <- numeric(length = 1e3)
  for(i in 1:1e3)
  {
    att_vec[i] <- attempts(25)
  }},
  replicate(1e3, attempts(25)), replications = 100)

#Q3

benchmark({
  att_vec <- numeric(length = 1e4)
  for(i in 1:1e4)
  {
    att_vec[i] <- attempts(25)
  }},
  replicate(1e4, attempts(25)), replications = 20)

 
# 1 is faster because of memory allocation
#for loop allocates the memory systematically

#Q4

benchmark({
  att_vec <- numeric(length = 1e4)
  for(i in 1:1e4)
  {
    att_vec[i] <- attempts(25)
  }},
  replicate(1e4, attempts(25)),{att_vec <- NULL
  for(i in 1:1e4)
  {
    att_vec <- c(att_vec, attempts(25))
  }}, replications = 25) 
#it does not vary linearly with the number of replicate times 

#Q5

m <- 1000
n <- 1200
mat <- matrix(data = 0, nrow = m, ncol = n)
for (i in 1:m)
{
  for (j in 1:n)
  {
    mat[i,j] <- runif(n = 1, min = 0,max = 1)
  }
}

?apply

