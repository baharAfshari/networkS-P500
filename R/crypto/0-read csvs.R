# load libraries ----------------------------------------------------------

library(xts)
library(TTR)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(raster)
library(lsa)


# load normal_list from clean_data scripts --------------------------------

path <-  "data/Data"
list_of_files <- list.files(path)
names_of_cryptos <- substr(list_of_files, 1, nchar(list_of_files)-9)
list_of_files <- paste(path, list_of_files, sep = "/")

for (i in 1:length(list_of_files)) {
  temp_tbl <- read.csv(list_of_files[i])[,-1]
  Ticker <- rep(names_of_cryptos[i], nrow(temp_tbl))
  temp_tbl <- cbind(temp_tbl, Ticker)
  temp_tbl <- temp_tbl[, c(1, 8, 5, 6)]
  temp_tbl[, 1] <- as.Date(temp_tbl[, 1])
  temp_tbl <- temp_tbl[order(temp_tbl$Date),]
  
}
