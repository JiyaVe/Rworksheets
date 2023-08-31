#WORKSHEET 7

library(tidyverse)
library(rvest)

html <- read_html("https://www.imdb.com/chart/top/")

url <- html_elements(html,".ipc-lockup-overlay.ipc-focusable") %>% html_attr("href")

titles <- substring(url,8,16)

titles[36] <- substring(url[36],8,17)

pages <- paste0("https://www.imdb.com/title/",titles,"/ratings/")
unweight_rate <- numeric(length = 250)

for(i in 1: length(pages))
{
  print(i)
  rating <- read_html(pages[i])
  
  unweight <- rating %>% html_elements(".sc-32706d9c-1.dRWuAv") %>% html_text()
  unweight_rate[i] <- as.numeric(substring(unweight,1,3))
  
  
}



