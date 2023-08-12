#Questions

#1
#Logic: Run loop throughout the pixels and calculate the euclidian distance of the pixels. 
#The point with the smallest distance will be closest to the point (0,1,0)purest green.

#To find purest Green
dist <- matrix(0, nrow = dim(dog)[1], ncol = dim(dog)[2])

for (i in 1:dim(dog)[1])
{
  for (j in 1:dim(dog)[2])
  {
  dist[i,j] <- norm(dog[i,j,1,] - c(0,1,0), "2")
    
  }
}

index <- which(dist == min(dist), arr.ind = TRUE)
plot(dog)
points(index , col = "Red" ,pch = 16)

#2
#To find purest red and blue change the vector to (1,0,0) and (0,0,1)
