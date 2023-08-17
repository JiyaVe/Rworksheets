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
i <- load.image(n)
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


