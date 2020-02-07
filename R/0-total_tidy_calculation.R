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

load("data/normal_list.rda")



# convert to tidy format --------------------------------------------------

n <- length(normal_list)
normal_tidy <- data.frame(index(normal_list[[1]]),rep(names(normal_list)[1], nrow(normal_list[[1]])), normal_list[[1]])
colnames(normal_tidy)[1:2] <- c("Date", "Ticker")
rownames(normal_tidy) <- NULL
for(i in 2:n) {
  u <- data.frame(index(normal_list[[i]]),rep(names(normal_list)[i], nrow(normal_list[[i]])), normal_list[[i]])
  colnames(u)[1:2] <- c("Date", "Ticker")
  rownames(u) <- NULL
  normal_tidy <- rbind(normal_tidy, u)
}

normal_tidy <- normal_tidy %>%
  mutate(Date = as.Date(Date))

save(normal_tidy, file = "data/normal_tidy.rda")
load("data/normal_tidy.rda")

# total_tidy --------------------------------------------------------------
total_tidy <- normal_tidy %>%
  group_by(Ticker) %>%
  mutate(vol_x = c(diff(Close),NA), vol_y = c(diff(Volume), NA), 
         vol_size = sqrt(vol_y^2 + vol_x^2), vol_x_normal = vol_x / vol_size,
         vol_y_normal = vol_y/vol_size) %>% 
  mutate(atan= atan2(vol_y ,vol_x), degree = atan*180/pi) %>% 
  ungroup() 

save(total_tidy, file="data/total_tidy.rda")
