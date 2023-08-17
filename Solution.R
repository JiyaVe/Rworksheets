#Q1

#Random Experiment rbinom()
# n = number of coin tosses
# size = 1 (tells R we are tossing a coin)
# prob = probability of success

rbinom(n = 10, size = 1, prob = 0.5)

#(a)

toss <- rbinom(n = 1000, size = 1, prob = 0.5) 
heads <- sum(toss)
h <- heads/1000
h # if taking Heads as SUCCESS
1-h # if taking Heads as FAILURE

#(b)

toss2 <- rbinom(n = 1000,size = 1, prob = 0.3)
heads2 <- sum(toss2)
heads2/1000

#Q2

#Rolling a die
s <- sample(x = 1:6, size=1)

#rolling a unfair die
sample(x = 1:6,size = 1,prob = c(.1,.2,.1,.1,.3,.2))

# drawing a random number between [a,b]
# n = number of random numbers
# min = a
# max = b

runif(n = 1, min = 0, max = 1)

#(a)

ball <- function()
{
  balls <- c("red","red","red","green","green","blue","blue")
  n <- sample(x = 1:7, size =1,prob = c(3/7,3/7,3/7,2/7,2/7,2/7,2/7))
  balls[n]
}
ball()

#suppose 1 = red, 2 = green, 3 = blue
sample(1:3, size = 1, prob = c(3,2,2)/7)

#(b)

A <- matrix(c(3, 1, -2, 4, 5, 3, -1, 2, -2), nrow = 3, ncol = 3)

A1 <- A[, 1]
A2 <- A[, 2]
A3 <- A[, 3]

A1 <- as.matrix(A1)
A2 <- as.matrix(A2)
A3 <- as.matrix(A3)

n1 <- norm(A1)
n2 <- norm(A2)
n3 <- norm(A3)

p1 <- n1/(n1+n2+n3)
p2 <- n2/(n1+n2+n3)
p3 <- n3/(n1+n2+n3)

p <- function()
{ n <- sample(x = 1:3, size = 1, prob = c(p1,p2,p3))

A[,n]
}
p()

#(C)

dart <- function() { round(runif(n = 1, min = 0, max = 5), digits = 2) }
dart()

#Q3
#(a)

exceed <- function()
{
  count <- 0
  sum <- 0
  while(sum <= 1)
  {
    sum = sum + runif(n = 1, min = 0, max = 1)
    count = count + 1
  }
  return(count)
}

exceed()

#(b)

store <- numeric(length = 1000)
for (r in 1:1000)
{
  store[r] <- exceed()
}
store

#(c)

sum1<- 0
for(r in 1:1000)
{
  sum1 = sum1 + store[r]
}
avg <- sum1/1000
avg

#Q4
#(a)

candles <- function(age)
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
candles(25)
#(b)

v <- numeric (length = 1000)

for( r in 1:1000 )
{
  v[r] <- candles(25)
}
v
#(c)
mean(v)
sum2 <- 0
for(r in 1:1000)
{
  sum2 = sum2 + v[r]
}
avg1 <- sum2/1000
avg1

#(d)

d <- numeric (length = 1000)

for( r in 1:1000 )
{
  d[r] <- candles(30)
}


sum3 <- 0
for(r in 1:1000)
{
  sum3 = sum3 + d[r]
}
avg2 <- sum3/1000
avg2


