#Questions

#1
#Logic: Run loop throughout the pixels and calculate the euclidian distance of the pixels. 
#The point with the smallest distance will be closest to the point (0,1,0)purest green.

#To find purest Green
library(imager)

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

#3

im1 <- load.image("col1.png")
im2 <- load.image("col2.png")
im3 <- load.image("col3.png")


for (i in c(im2,im3))
{
 if (norm(i[5,5,1,] - c(1,0,0), "2") <0.6 & (norm(i[5,5,1,] - c(1,0,0)>=0)))
 {
   print(i," is Red.")
 }
  else if (norm(i[5,5,1,] - c(0,1,0), "2") <0.6 & (norm(i[5,5,1,] - c(0,1,0)>=0)))
    {
      print(i," is Green.")
    }
  else
  {
    print(i, "is Blue.")
  }
  
}