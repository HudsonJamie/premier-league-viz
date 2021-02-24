library(tidyverse)

data.url <- "https://www.football-data.co.uk/mmz4281/2021/E0.csv"
data.file <- "data/matchData.csv"
data.date <- Sys.Date()
download.file(data.url, destfile = data.file, quiet = TRUE)

data.matches <- read.csv(data.file, header = TRUE)
names(data.matches)

data.edit <- data.matches %>% select(HomeTeam, AwayTeam, FTHG, FTAG, Date)
head(data.edit)
data.edit$Date <- as.Date(data.edit$Date, "%d/%m/%Y")

calculate.win <- function(ourScore, theirScore){
  return(ourScore > theirScore)
}

calculate.lose <- function(ourScore, theirScore){
  return(ourScore < theirScore)
}

calculate.draw <- function(ourScore, theirScore){
  return(ourScore == theirScore)
}

calculate.points <- function(ourScore, theirScore){
  return(ifelse(ourScore < theirScore, 0, ifelse(ourScore == theirScore, 1, 3)))
}

