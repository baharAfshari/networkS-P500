library(igraph)
library(ggnetwork)
library(tidyr)
library(dplyr)
library(tibble)
library(gplots)

load("data/crypto/daily_cosinus_2021_2.rda")

test <- vector()
n_coin <- vector()
date <- vector()
for (i in 1:length(daily_res)) {
  test[i] <- mean(daily_res[[i]]$cos_vecs, na.rm = T)
  b <- daily_res[[i]]$Date
  date[i] <- as.character(b[1])
  check <- daily_res[[i]][!is.na(daily_res[[i]]$cos_vecs),]
  n_coin <- length(unique(daily_res[[i]]$Ticker))
}

tets<- data.frame(as.Date(date), test, n_coin)
tets <- tets[order(tets$as.Date.date.),]
tets_max <- tets[order(-tets$test),]
plot(tets, type = "l")

b <- daily_res[[i]]$Date
b[1]
