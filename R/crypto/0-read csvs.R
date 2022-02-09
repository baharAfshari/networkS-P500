# load libraries ----------------------------------------------------------

library(xts)
library(TTR)
library(tidyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(raster)
library(REdaS)


# load normal_list from clean_data scripts --------------------------------

path <-  "data/Data"
list_of_files <- list.files(path)
names_of_cryptos <- substr(list_of_files, 1, nchar(list_of_files)-9)
list_of_files <- paste(path, list_of_files, sep = "/")
total_tidy_list <- list()

for (i in 1:length(list_of_files)) {
  temp_tbl <- read.csv(list_of_files[i])[,-1]
  if(nrow(temp_tbl) > 200) {
    Ticker <- rep(names_of_cryptos[i], nrow(temp_tbl))
    temp_tbl <- cbind(temp_tbl, Ticker)
    temp_tbl <- temp_tbl[, c(1, 8, 5, 6)]
    temp_tbl[, 1] <- as.Date(temp_tbl[, 1])
    temp_tbl <- temp_tbl[order(temp_tbl$Date),]
    # xts_tbl <- xts(temp_tbl[, -1], temp_tbl[, 1])
    
    n = 20
    diff_price <- diff(temp_tbl$Close)
    diff_volume <- diff(temp_tbl$Volume)
    sma_diff_price <- SMA(abs(diff_price), n)[-c(1:n)]
    sma_diff_volume <- SMA(abs(diff_volume), n)[-c(1:n)]
    
    vel_x <- diff_price[-c(1:n+1)] / sma_diff_price
    vel_y <- diff_volume[-c(1:n+1)] / sma_diff_volume
    
    temp_tbl <- cbind(temp_tbl[-c(1:(n+1)),], vel_x, vel_y)
    temp_tbl<- temp_tbl %>% mutate(vel_size = sqrt(vel_x^2 + vel_y^2)) %>% 
      mutate(vel_x_normal = vel_x/vel_size, vel_y_normal = vel_y/vel_size, atan = atan(vel_y/vel_x), 
             degree = rad2deg(atan))
    
    total_tidy_list[[names_of_cryptos[i]]] <- temp_tbl
  }
}

total_tidy <- bind_rows(total_tidy_list)
save(total_tidy, file = "data/crypto/total_tidy.rda")
