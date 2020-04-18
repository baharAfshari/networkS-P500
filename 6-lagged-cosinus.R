library(tidyr)
library(dplyr)
library(ggplot2)
library(lsa)
library(purrr)
library(magrittr)
library(tibble)


load("data/daily_cosinus_2018.rda")
load("data/daily_cosinus_lag1_2018.rda")


for (day in 1:length(daily_res_lag1)) {
  lagday_network_sample <- daily_res_lag1[[day]]
  lag_day_network_sample <- spread(lagday_network_sample[,-1], Ticker2, cos_vecs)
  lag_day_network_sample <- as.matrix(column_to_rownames(lag_day_network_sample, var = "Ticker"))
  sym_lag_1day <- pmax(lag_day_network_sample, t(lag_day_network_sample))
  mat <- as.matrix(lag_day_network_sample)
  for (i in 1:length(mat)) {
    for (j in 1:i+1) {
      if(mat[i,j]>mat[j,i]){
        mat[i,j]
      } else{
        mat[j,i]
      }
    }
  }
  
  
}