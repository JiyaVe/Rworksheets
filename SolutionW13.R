library(ggplot2)
load("IMDB_movies.Rdata")

base <- ggplot(dat, aes(x = rating))

base + geom_histogram()

base + geom_bar()

base + geom_boxplot()

base2 <- ggplot(dat,aes(y = rating,x = year))
base2

base2 + geom_point() + labs(title = "Movie rating",x = "Year",y = "Rating")

base2 +geom_point() + coord_cartesian(xlim = c(1997,2003))

Year <- dat$year < 2000
Year <- as.factor(Year)
Year
