##### Worksheet 4 solutions #####

library(imager)

##############################
# Problem 1
dog <- load.image("dog.jpeg")
col.mat<-as.array(dog[, ,1, ])

dims <- dim(col.mat)

# Calculate distance to purest color
# for each pixel
dist <- matrix(0, nrow = dims[1], ncol = dims[2])
for(i in 1:dims[1])
{
  for(j in 1:dims[2])
  {
    # replace c(0,1,0) with c(1,0,0) or c(0,0,1) for other problems
    dist[i,j] <- norm(col.mat[i,j, ] - c(0,1,0), "2")
  }
}

# find pixels with smallest distance
ind1 <- which(dist == min(dist), arr.ind = TRUE)

# another way
# read ?apply help page to understand the command below
dist <- apply(col.mat, c(1,2), function(s) norm(s - c(0,1,0), "2"))
ind2 <- which(dist == min(dist), arr.ind = TRUE)

# checking to see that ind1 = ind2. Hence both methods are same
ind1 == ind2

plot(dog)
points(ind1, col = "red")




###################################
# Problem 3

col1 <- load.image("col1.png")
# col1 has 4 color channels for some reason. 
# Removing the last one.
col1 <- as.cimg(col1[ , , , 1:3])

col2 <- load.image("col2.png")
col3 <- load.image("col3.png")


# I will make a function that calculates the
# difference matrix from a color for each image

diff.col <- function(img, col)
{
  col.mat <- as.array(img[, , 1, ])
  dims <- dim(col.mat)
  
  # Calculate distance to given color
  dist <- matrix(0, nrow = dims[1], ncol = dims[2])
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      # distance from the col give by user
      dist[i,j] <- norm(col.mat[i,j, ] - col, "2")
    }
  }
  # return the distance matrix of each pixel
  return(dist)
}

## Making another function for each color 
## for a given image. Then for whichever primary
## color the mean of the distance matrix is smallest
## That will be the guess. 
which.color <- function(img)
{
  dist.cols <- numeric(length = 3)
  
  # make a matrix of ones.
  # this is a shortcut to define all three
  # primary colors in one go.
  colors.matrix <- diag(3)  
  for(k in 1:3)
  {
    # picking kth column of colors.matrix as we are picking
    # the kth primary color to compare to
    dist.cols[k] <- mean(diff.col(img, col = colors.matrix[ ,k]))
  }
  
  # which color has the smallest distance
  ind <- which.min(dist.cols)
  guess <- c("red", "green", "blue")[ind]
  
  return(guess)
}

which.color(col1)  # guessing for col1
which.color(col2)  # guessing for col2
which.color(col3)  # guessing for col3


###################################
# Problem 4

pic1 <- load.image("land1.jpeg")
pic2 <- load.image("land2.jpeg")

# We can reuse function diff.col!
# measuring average distance to c(1,1,1) for both pics
distance.to.white1 <- mean(diff.col(pic1, c(1,1,1)))
distance.to.white2 <- mean(diff.col(pic2, c(1,1,1)))

ifelse(distance.to.white1 < distance.to.white2, "Land1", "Land2")




##############################
# Problem 5
# 180 deg means that n x m remains n x m
# and directions are flipped
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])

dims <- dim(col.mat)
rot <- array(0, dim = dims)

for(i in 1:dims[1])
{
  for(j in 1:dims[2])
  {
    rot[i, j, ] <- col.mat[dims[1] - i + 1, dims[2] - j + 1, ]
  }
}

# Let's plot size by side
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(rot))



##############################
# Problem 6
# 90 deg means that n x m becomes m x n
# and then appropriate rotation
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])

dims <- dim(col.mat)
rot <- array(0, dim = c(dims[2], dims[1], dims[3]))

for(i in 1:dims[1])
{
  for(j in 1:dims[2])
  {
    # i component becomes j
    rot[j, i, ] <- col.mat[i, dims[2] - j + 1, ]
  }
}

# Let's plot size by side
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(rot))




##############################
# Problem 7
# 90 deg means that n x m becomes m x n
# and then appropriate rotation
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])

dims <- dim(col.mat)
rot <- array(0, dim = c(dims[2], dims[1], dims[3]))

for(i in 1:dims[1])
{
  for(j in 1:dims[2])
  {
    # i component becomes j
    rot[j, i, ] <- col.mat[dims[1] - i + 1, j, ]
  }
}

# Let's plot size by side
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(rot))




##############################
# Problem 8

# cropping image first
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])
col.mat <- col.mat[1:600, 1:600, ]

reduce <- array(0, dim = c(300, 300, 3))
red.dims <- dim(reduce)

# I will take an average of the 2 by 2 pixels
# and then assign the average value to the reduce
averaging <- rep(0,3)
for(i in 1:red.dims[1])
{
  for(j in 1:red.dims[2])
  {
    ind1 <- (2*(i-1) + 1): (2*i)
    ind2 <- (2*(j-1) + 1): (2*j)
    
    # taking the average rbg in the 2 by 2 area
    averaging[1] <- mean(col.mat[ind1, ind2, 1])
    averaging[2] <- mean(col.mat[ind1, ind2, 2])
    averaging[3] <- mean(col.mat[ind1, ind2, 3])
    
    reduce[i, j, ] <- averaging
  }
}
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(reduce))
save.image(as.cimg(reduce), file = "dog_300.jpeg")


##############################
# Problem 9

# cropping image first
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])
col.mat <- col.mat[1:600, 1:600, ]

reduce <- array(0, dim = c(60, 60, 3))
red.dims <- dim(reduce)

# I will take an average of the 10 by 10 pixels
# and then assign the average value to the reduce
averaging <- rep(0,3)
for(i in 1:red.dims[1])
{
  for(j in 1:red.dims[2])
  {
    ind1 <- (10*(i-1) + 1): (10*i)
    ind2 <- (10*(j-1) + 1): (10*j)
    
    # taking the average rbg in the 10 by 10 area
    averaging[1] <- mean(col.mat[ind1, ind2, 1])
    averaging[2] <- mean(col.mat[ind1, ind2, 2])
    averaging[3] <- mean(col.mat[ind1, ind2, 3])
    
    reduce[i, j, ] <- averaging
  }
}
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(reduce))
save.image(as.cimg(reduce), file = "dog_60.jpeg")

library(help = "imager")

#############shortcuts

#4

library(imager)
name1 <- readline("Enter image file name1: ")
img1 <- load.image(name1)

dist <- matrix(0,nrow = dim(img1)[1], ncol = dim(img1)[2])

for (i in 1:dim(img1)[1])
{
  for (j in 1:dim(img1)[2])
  {
    dist[i,j] <- norm(img1[i,j,1,] - c(1,1,1), "2")
  }
}

index1 <- which(dist == min(dist), arr.ind = TRUE)



name2 <- readline("Enter image file name2: ")
img2 <- load.image(name2)

dist2 <- matrix(0,nrow = dim(img2)[1], ncol = dim(img2)[2])

for (i in 1:dim(img2)[1])
{
  for (j in 1:dim(img2)[2])
  {
    dist2[i,j] <- norm(img2[i,j,1,] - c(1,1,1), "2")
  }
}

index2 <- which(dist2 == min(dist2), arr.ind = TRUE)

if ( dim(index1)[1]> dim(index2)[1]) { print("Image 1 has a lot of snow.")}
if( dim(index2)[1]> dim(index1)[1]) { print("Image 2 has a lot of snow.")}

#5 #6 #7
n <- readline("Enter the image file name: ")
i <- load.image("dog.jpeg")
new180 <- imrotate(i,180)
plot(new180)

new90 <- imrotate(i,90)#cloackwise
plot(new90)

new1 <- imrotate(i,270)#anticlockwise
plot(new1)  


#8 #9

dog <- load.image("dog.jpeg")
col_mat <- as.array(dog[,,1,])
dim(col_mat)

cropped <- col_mat[1:600,1:600,]
crop_dog <- as.cimg(cropped)
plot(crop_dog)

resized_img <- resize(crop_dog,  size_x = 300L, size_y = 300L)
plot(resized_img)
save.image(resized_img,"resized_img.jpeg")

resized_img2 <- resize(crop_dog,  size_x = 60L, size_y = 60L)
plot(resized_img2)

library(help="imager" )
imrotate()

# Sample image matrix (replace this with your actual image matrix)
image_matrix <- as.matrix(dog[,,1,1])

# Function to perform a horizontal flip
horizontal_flip <- function(image_matrix) {
  num_rows <- nrow(image_matrix)
  num_cols <- ncol(image_matrix)
  
  flipped_image <- matrix(0, nrow = num_rows, ncol = num_cols)
  
  for (row in 1:num_rows) {
    flipped_image[row, ] <- rev(image_matrix[row, ])
  }
  
  return(flipped_image)
}

flipped_horizontal <- horizontal_flip(image_matrix)
print(flipped_horizontal)

plot(as.cimg(flipped_horizontal))

vertical_flip <- function(image_matrix)
{num_rows <- nrow(image_matrix)
num_cols <- ncol(image_matrix)
flipped_image <- matrix(0, nrow = num_rows, ncol = num_cols)

for (col in 1:num_cols ){
  flipped_image[,col] <- rev(image_matrix[,col])
}
return(flipped_image)
}
v <- vertical_flip(image_matrix)
plot(as.cimg(v))


# Sample image matrices (replace these with your actual image matrices)


# Function to blend two images
image_blend <- function(image1, image2, alpha) {
  # Ensure both images have the same dimensions
  #if (dim(image1) != dim(image2)) {
  # stop("Images must have the same dimensions.")
  ## }
  
  # Calculate the blended image
  blended_image <- (alpha * image1) + ((1 - alpha) * image2)
  
  # Clip pixel values to the valid range (0 to 255)
  blended_image <- pmax(0, pmin(255, blended_image))
  
  return(blended_image)
}

# Blend the images with alpha = 0.5 (equal weight)
image1 <- as.matrix(load.image("col1.png")[,,1,1])
image2 <- as.matrix(load.image("col2.png")[,,1,1])

alpha_value <- 0.5
blended_result <- image_blend(image1, image2, alpha_value)


?diag
?rep
?par
