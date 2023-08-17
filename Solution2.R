#Questions

#1
#Logic: Run loop throughout the pixels and calculate the euclidian distance of the pixels. 
#The point with the smallest distance will be closest to the point (0,1,0)purest green.

#To find purest Green
library(imager)
dog <- load.image("dog.jpeg")

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
col1 <- load.image("col1.png")
dim(col1)

col2 <- load.image("col2.png")
dim(col2)

col3 <- load.image("col3.png")
dim(col3)

colour<-c("red","green","blue")

index1 <- which(col1[1,1,1,1:3]>0.5)
colour[index1]

index2 <- which(col2[1,1,1,1:3]>0.5)
colour[index2]

index3 <- which(col3[1,1,1,1:3]>0.5)
colour[index3]

#3
col1 <- load.image("col1.png")
dim(col1)
col2 <- load.image("col2.png")
dim(col2)
col3 <- load.image("col3.png")
dim(col3)

colour<-c("red","green","blue")
index1 <- which.max(col1[1,1,1,1:3])
index1
colour[index1]

index2 <- which.max(col2[1,1,1,1:3])
colour[index2]


index3 <- which.max(col3[1,1,1,1:3])
index3
colour[index3]

#3 (More Accurate)
col1 <- load.image("col1.png")
col1 <- as.cimg(col1[ , , , 1:3])
col2 <- load.image("col2.png")
col3 <- load.image("col3.png")

diff.col <- function(img, col)
{
  col.mat <- as.array(img[, , 1, ])
  dims <- dim(col.mat)
  
  dist <- matrix(0, nrow = dims[1], ncol = dims[2])
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      dist[i,j] <- norm(col.mat[i,j, ] - col, "2")
    }
  }
  return(dist)
}
which.color <- function(img)
{
  dist.cols <- numeric(length = 3)
  colors.matrix <- diag(3)  
  for(k in 1:3)
  {
    dist.cols[k] <- mean(diff.col(img, col = colors.matrix[ ,k]))
  }
  ind <- which.min(dist.cols)
  guess <- c("red", "green", "blue")[ind]
  
  return(guess)
}

which.color(col1)  
which.color(col2)  
which.color(col3)