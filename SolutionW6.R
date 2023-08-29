

library(tidyverse)
library(rvest)

html <- read_html("http://iitk.ac.in/math/faculty")

#html_elements() to extract all instances of a particular kind of tag
#Method1
name <- html_elements(html,".head3") #use . when there is class
name <- html_elements(name,"a") #No . for tags
name <- html_text(name)
name

#Method2
name <- html_elements(html,".head3 a")
name <- html_text(name)
html
class(html)

#Method3 Using Pipes
name <- html %>% html_elements(".head3 a") %>% html_text() # %>% means "and then do" | takes the input as the first argument

#Q1

h <- read_html("http://iitk.ac.in/math/visitors-post-doctoral-fellow")
name <- html_elements(h,".head2")
name
name <- html_text(name)
name
?substring

name <- substring(name, 5)
name 

#Q2

html <- read_html("https://www.imdb.com/chart/top/")

name <- html_elements(html,".ipc-title__text")
name <- html_text(name)
name <- name[-c(1,2)]
name <- name[1:250]

name <- sapply(1:250 ,function(k) strsplit(name[k],as.character(k))[[1]][2])#we can use for loop also
name <- substring(name, 3)
name

#Q3

name <- html %>% html_elements(".sc-b85248f1-6.bnDqKN.cli-title-metadata-item") %>% html_text()
name
