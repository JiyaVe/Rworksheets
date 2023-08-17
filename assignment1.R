### Paste all your code here and save 
#Q1

birthday <- function(n)
{
  oneorzero <- 0
  
  p <- c(length = n)
  for(j in p)
  {
    p[j] <- 1/365
  }
  i <- sample(x = 1:365, size = n, prob = p[1:n])
  for (k in i)
  {
    for (l in i)
    {
      if(i[k]== i[l])
      {
        oneorzero <- 1
      }
    }
  }
  return(oneorzero)
}


#Q2

matches <- c(length = 1000)
for(i in 1:1000)
{
  matches[i] <- birthday(50)
}
ans <- mean(matches)