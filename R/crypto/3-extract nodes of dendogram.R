library(dendextend)
library(tidyverse)
library(RColorBrewer)

# load('data/crypto/day_network_sample.rda')
load("data/crypto/daily_cosinus_2018.rda")

# s_day_network_sample <- spread(day_network_sample[,-1], Ticker2, cos_vecs)
# s_day_network_sample <- column_to_rownames(s_day_network_sample, var = "Ticker")
# s_day_network_sample[is.na(s_day_network_sample)] <- 0
# 
# normal.dist <- as.dist(s_day_network_sample)
# normal.tree <- hclust(normal.dist, method="complete")
# normal.dend <- as.dendrogram(normal.tree) # create dendrogram object
# plot(normal.dend, leaflab = "none")
# 
# nleaves(normal.dend)
# nnodes(normal.dend)
# 
# clusters <- cutree(normal.dend, h=1.5)
# table(clusters)
# 
# clusters[1:6]
# plot(color_branches(normal.dend, h=1),leaflab="none")
# 
# clusters.df <- data.frame(Ticker = names(clusters), cluster = clusters)
# cluster1.n <- filter(clusters.df, cluster == 1)$Ticker
# cluster2.n <- filter(clusters.df, cluster == 2)$Ticker
# cluster3.n <- filter(clusters.df, cluster == 3)$Ticker
# cluster4.n <- filter(clusters.df, cluster == 4)$Ticker
# cluster5.n <- filter(clusters.df, cluster == 5)$Ticker
# cluster6.n <- filter(clusters.df, cluster == 6)$Ticker


# make a function for all days --------------------------------------------
cluster_extraction <- function(x){
  
  s_day_network_sample <- spread(x[,-1], Ticker2, cos_vecs)
  s_day_network_sample <- column_to_rownames(s_day_network_sample, var = "Ticker")
  s_day_network_sample[is.na(s_day_network_sample)] <- 0
  s_day_network_sample <- s_day_network_sample + t(s_day_network_sample)
  
  normal.dist <- as.dist(s_day_network_sample)
  normal.tree <- hclust(normal.dist, method="complete")
  normal.dend <- as.dendrogram(normal.tree) # create dendrogram object
  clusters <- cutree(normal.dend, h=1.99999)
  
  clusters.df <- data.frame(Ticker = names(clusters), cluster = clusters)
  rownames(clusters.df) <- NULL
  return(clusters.df)
}

clusters_daily_list <- lapply(daily_res, cluster_extraction)
clusters_daily_tbl <- bind_rows(clusters_daily_list, .id="Date")

counting_clusters <- clusters_daily_tbl %>% 
  group_by(Date) %>% 
  summarise(number_clusters= max(cluster))

ggplot(counting_clusters, aes(Date,number_clusters, group = 1))+
  geom_line()+ stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess"
  )

counting_clusters$Date <- as.POSIXct(counting_clusters$Date,format("%Y-%m-%d"))


library(ggpmisc)
ggplot(counting_clusters,aes(Date,number_clusters, group = 1)) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", 
             vjust = -0.5, x.label.fmt = "%m-%d", ignore_threshold= 0.95, position = "identity" ) +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue",
               vjust = 1, hjust = 1,  x.label.fmt = "%m-%d", ignore_threshold= 0.80)
