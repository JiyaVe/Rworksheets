#1

v <- c(length = 1000)
n <- 1

for (i in 1:1000)
{
  v[i] <- n
  n <- n + 2
}
v

#2
#Fibonacci Series : sequence of numbers (also called Fibonacci numbers), 
#where every number is the sum of the preceding two numbers

v2 <- c(length = 50)
v2[1] <- 1
v2[2] <- 2

for(i in 2:49)
{
  v2[i + 1] <- v2[i] + v2[i-1]
  
}
v2

#3

roll <- function()
{ n <- sample(x = 1:6,size = 1)
  if (n%%2 == 0) return(1)
  else return(0)
}

roll()

#4

coin <- function()
{r <- rbinom( n = 15,size = 1,prob = 0.5 )
 if ( sum(r) >= 8 ) {print('Win!')}
 else {print("Lose!")}
}

coin()

#5

mat <- matrix(data = 1,nrow = 5,ncol = 5)
mat

#6

mat1 <- matrix(data = 0,nrow = 5,ncol = 5)
n <- 1


for(i in 1:dim(mat1)[1])
{
  for(j in 1:dim(mat1)[2] )
  {
    if(i == j) {mat1[i,j] = n}
  }
 n <- n + 1
}
mat1

#7


mat2 <- matrix(data = 0, nrow = 10, ncol = 10)

for(i in 1:dim(mat2)[1])
{
  for(j in 1:dim(mat2)[2] )
  {
    mat2[i,j] <- sample(x = 1:6,size = 1)
  }
  
}
mat2

#8

defm <- function()
{
  n <- as.numeric(readline("n : "))
  rho <- as.numeric(readline("rho : "))
  m <- matrix(data = rho,nrow = n,ncol = n)
  for(i in 1:dim(m)[1])
  {
    for(j in 1:dim(m)[2] )
    {
      if(i == j) {m[i,j] = 1}
    }
  }
  print(m)
}
defm()

defm <- function(n,rho)
{
  m <- matrix(data = rho,nrow = n,ncol = n)
  for(i in 1:dim(m)[1])
  {
    for(j in 1:dim(m)[2] )
    {
      if(i == j) {m[i,j] = 1}
    }
  }
  print(m)
} 

defm(4,5)


#9

defm <- function(n,rho)
{
  m <- matrix(data = rho,nrow = n,ncol = n)
  for(i in 1:dim(m)[1])
  {
    for(j in 1:dim(m)[2] )
    {
      m[i,j] <- rho^(abs(i-j))
     }
  }
  print(m)
} 

defm(5,2)

#10#imp

matf <- function(mat)
{
  n <- ncol(mat)
  mat1 <- mat[,c(1, n, by = 2)]
  return(mat1)
}
mat <- matrix(data = sample(x=1:6,size = 1),nrow = 3,ncol= 5)
matf(mat)
mat

#11

a <- array(data = 1,dim = c(10,4,6,5))
a


###########################################
###########################################

#1

area <- function(r)
{
  a = pi*r*r
  return(a)
}

area(5)

#2

greater <- function(x,y)
{
  if (x>y){
    print(x)
  }
  else
  {
    print(y)
  }
}

greater(6,8)

#3

count <- 0
rolls <- function()
{
  die <- numeric(1000)
  for(j in 1:1000)
  {
    die[j] <- sample(x = 1:6,size = 1)
    if(die[j]%%2 == 0){count <- count + 1}
  }
  
  return(count)
}

rolls()


#4

r <- numeric(1000)
count <- 0
for (i in 1:1000)
{
  r[i] <- runif(n=1,min = 0,max = 1)
  if(r[i] >=0.1 && r[i] <= 0.2)
  {
    count <- count + 1
  }
}
count

#5#imp


simulate_collect_toys <- function() {
  toys <- c("Harry", "Dumbledore", "Hermione", "Ron", "Neville", "Mcgonagall", "Dobby")
  collected_toys <- character(0)
  packets_bought <- 0
  
  while (length(collected_toys) < length(toys)) {
    packet <- sample(toys, 1, prob = c(0.25, 0.20, 0.20, 0.15, 0.10, 0.05, 0.05))
    if (!(packet %in% collected_toys)) {
      collected_toys <- c(collected_toys, packet)
    }
    packets_bought <- packets_bought + 1
  }
  
  return(packets_bought)
}

d <- c(1000)
for (r in 1:1000)
{
  d[r] <- simulate_collect_toys()
}

d

mean(d)

#16

library(rvest)
library(tidyverse)
library(dplyr)

html <- read_html("https://stats.stackexchange.com/questions?tab=Votes")

question <- html_elements(html,".s-post-summary--content-title a")
question <- html_text(question)
question

views <- html %>% html_elements(".s-post-summary--stats-item.is-supernova") %>% html_text()

views <- views %>% substring(15,18)
views

korm <- views %>% substring(4,4)

n_view <- as.numeric(substring(views,1,3))

view <- (n_view*1000)*(korm == "k") + (n_view*1000000)*(korm == "m")

ans <- html %>% html_elements(".s-post-summary--stats-item.has-answers.has-accepted-answer") %>% html_text()
ans <- substring(ans,3,4)
ans[4] <- substring(ans[4],1,1)
ans[8] <- substring(ans[8],1,1)
ans[9] <- substring(ans[9],1,1)
ans[12] <- substring(ans[12],1,1)
ans[13] <- "NA"
ans[14] <- "NA"
ans[15] <- "NA"

votes <- html %>% html_elements(".s-post-summary--stats-item.s-post-summary--stats-item__emphasized") %>% html_text()
votes <- substring(votes,15,18)
votes[2:length(votes)] <- votes[2:length(votes)] %>% substring(1,3)
votes <- as.numeric(votes)

data <- data.frame(question,view,ans,votes)
data


#15

html <- read_html("https://en.wikipedia.org/wiki/United_States_at_the_Olympics")
tab <- html_table(html)
summer_games <- tab[[5]]
summer_games

#14

html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")

name <- html_elements(html,".article_movie_title a") %>% html_text()

rank <- 1:100

score <- html_elements(html,".tMeterScore") %>% html_text()

year <- html_elements(html,".subtle.start-year") %>% html_text()

tomato <- data.frame(rank,name,score,year)
tomato

#6

#Not really correct
bottle <- numeric(100)
bottle[1:100] <- 2

eat <- function(bottle)
{ 
  i <- sample(1:100,size = 1)
  bottle[i] <- bottle[i] - 1
  return(bottle)
}

n <- 50
while(n>0)
{
  bottle <- eat(bottle)
  n <- n - 1
}
bottle

half <- 0
m <- 0
for( m in 1:100)
{
  if(bottle[m] == 1)
  {
    half <- half + 1
  }
}
half
avg <- half/50
avg

# Function to simulate pulling a tablet from the bottle
pull_tablet <- function() {
  return(sample(c(0.5, 1), 1))  # 0.5 represents a half-tablet, 1 represents a whole tablet
}

# Function to run the simulation
simulate_days_to_half_tablet <- function() {
  days = 0
  while (TRUE) {
    pulled_tablet = pull_tablet()
    days = days + 1
    if (pulled_tablet == 0.5) {
      break
    }
  }
  return(days)
}

# Number of simulations
num_simulations <- 10000
results <- numeric(num_simulations)

# Run the simulations
for (i in 1:num_simulations) {
  results[i] <- simulate_days_to_half_tablet()
}

# Calculate the average number of days
average_days <- mean(results)

cat("Average number of days to pull a half-tablet:", average_days, "\n")


#7

MontyHall <- function() {
  # Step 1: Randomly place the car behind one of the three doors
  car_location <- sample(1:3, size = 1)
  
  # Step 2: Contestant chooses one of the doors
  contestant_choice <- sample(1:3, size = 1)
  
  # Determine the door that Monty will open (it must have a goat and not be the contestant's choice)
  possible_open_doors <- c(1, 2, 3)[-c(contestant_choice, car_location)]
  monty_opens <- sample(possible_open_doors, size = 1)
  
  # Determine the remaining door for the contestant to switch to
  remaining_doors <- c(1, 2, 3)[-c(contestant_choice, monty_opens)]
  switched_choice <- remaining_doors
  
  # Step 3: Check if the contestant's switched choice has the car
  win <- switched_choice == car_location
  
  return(as.numeric(win))
}

sti <- numeric(1000)
for ( i in 1:1000)
{
  sti[i] <- MontyHall()
}
sti
mean(sti)

#8

library(imager)

prop.color <- function(img,col)
{
  im_mat <- as.array(img[,,1,])
  
}

img <- load.image("land1.jpeg")
im_mat <- as.array(img[,,1,])
im_mat[1,1,1:3]
col <- c(0,0,0)
count <- 0

for(i in 1:dim(im_mat)[1])
{
  for(j in 1:dim(im_mat)[2])
  {
    if(norm(im_mat[i,j,1:3] - col,"2")<=0.5)
    {
      count <- count + 1
    }
    
  }
}
count
