#Q3

plot(c(1:10),c(1:10),type="l")

x <- 0:10

plot(y <- x,type = "l")

#Q4

n <- 1:1000
f <- (n+(1/n))^n
plot(f,type="l")
abline(v=exp(1),col="red")
