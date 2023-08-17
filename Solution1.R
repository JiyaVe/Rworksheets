#Images
#The InTro

library(imager)

#loading and plotting the image
dog <- load.image("dog.jpeg")
dim(dog)
plot(dog)

#plotting the image in grayscale
graydog <- grayscale(dog)
dim(graydog)
plot(graydog)

#To extract the raw matrix or array of the image
gray.mat <- as.matrix(graydog[,,1,1])#Of grayscale
dim(gray.mat)

col.mat <- as.array(dog[,,1,])#of the original image
dim(col.mat)

#crop image
cropped <- col.mat[1:300,,]#vertical cropping
crop.dog <- as.cimg(cropped)
plot(crop.dog)

dog[1,1,1,]



