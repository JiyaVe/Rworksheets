#Worksheet 12

data <- read.csv("movie_unweighted.csv")
data

?hist

#2(a)
hist(data$ratings,main = "Histogram of Ratings",xlab = "Ratings")

#b
hist(data$ratings,main = "Histogram of Ratings",xlab = "Ratings",col = "white")

#c
mean1 <- mean(data$ratings)
mean2 <- mean(data$unweighted)


median1 <- median(data$ratings)
median2 <- median(data$unweighted)

par(mfrow = c(1,2)) #it breaks the window in 1 row 2 columns

hist(data$ratings,main = "Histogram of Ratings",xlim = c(7.5,10),xlab = "Ratings")
abline(v = c(mean1,median1),col = c("tomato","green"),lty = c(2,3),lwd = 2)
legend("topright",legend = c("Sample Mean","Sample Median"),col = c("tomato","green"),lty = c(2,3),lwd = 2)

hist(data$unweighted,main = "Histogram of Unweighted",xlim = c(7.5,10),xlab = "Unweighted Mean",col = "white")
abline(v = c(mean2,median2),col = c("purple","green"),lwd = 2)
legend("topright",legend = c("Sample Mean","Sample Median"),fill = c("tomato","green"))#fill means box fill

#Both of the data points are positively skewed but first one is more positively skewed

?abline



