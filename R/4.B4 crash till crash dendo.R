
# library -----------------------------------------------------------------
library(dendextend)
library(tidyverse)
library(RColorBrewer)

# loading data ------------------------------------------------------------
load("data/daily_cosinus_2018.rda")

# normal day --------------------------------------------------------------
day_network_sample <- daily_res[["2018-06-06"]]
s_day_network_sample <- spread(day_network_sample[,-1], Ticker2, cos_vecs)
s_day_network_sample <- column_to_rownames(s_day_network_sample, var = "Ticker")
s_day_network_sample[is.na(s_day_network_sample)] <- 0
s_day_network_sample <- s_day_network_sample+ t(s_day_network_sample)
s_day_network_sample <- as.matrix(s_day_network_sample)


# crash day ---------------------------------------------------------------
crash_day <- daily_res[["2018-02-05"]]
crash_day_sample <- spread(crash_day[,-1], Ticker2, cos_vecs)
crash_day_sample <- column_to_rownames(crash_day_sample, var = "Ticker")
crash_day_sample[is.na(crash_day_sample)] <- 0
crash_day_sample <- crash_day_sample + t(crash_day_sample)
crash_day_sample <- as.matrix(crash_day_sample)


# 10 day B4 crash day -----------------------------------------------------

B4crash <- daily_res[["2018-02-05"]]
B4crash_sample <- spread(B4crash[,-1], Ticker2, cos_vecs)
B4crash_sample <- column_to_rownames(B4crash_sample, var = "Ticker")
B4crash_sample[is.na(B4crash_sample)] <- 0
B4crash_sample <- B4crash_sample + t(B4crash_sample)
B4crash_sample <- as.matrix(B4crash_sample)


# plot dendo --------------------------------------------------------------

normal.dist <- as.dist(B4crash_sample)
normal.tree <- hclust(normal.dist, method="complete")
normal.dend <- as.dendrogram(normal.tree) # create dendrogram object
plot(color_branches(normal.dend, h=1),leaflab="none")

