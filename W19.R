#worksheet19
#1

data <- read.csv("auto-mpg.csv")
head(data)

?cor

cor(data$acceleration,data$mpg)
#Both are roughly positively correlated

#2

plot(data$acceleration,data$mpg,xlab = "Acceleration",ylab = "MPG",pch = 16, col = data$cylinders)
abline(lm(data$mpg ~ data$acceleration),col = "green")

#3



#5
?iris
head(iris)

plot(iris)
plot(iris,col = iris$Species)

cor(iris$Sepal.Length,iris$Sepal.Width)
cor(iris$Sepal.Width,iris$Petal.Width)

#6

dat <- read.csv("fire-dat.csv")

head(dat)
cor(dat$firefight,dat$injured)
plot(dat$firefight,dat$injured)
#intensity of the fire missing

#7

dat <- read.csv("fire-intense.csv")
plot(dat)
