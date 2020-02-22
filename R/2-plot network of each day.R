library(igraph)
library(ggnetwork)
library(tidyr)
library(tibble)
library(gplots)


# heatmap + dendogram for a normal day ------------------------------------
load('data/day_network_sample.rda')
write.csv(day_network_sample, file = "data/normal_day.csv", quote = F, row.names = F)

s_day_network_sample <- spread(day_network_sample[,-1], Ticker2, cos_vecs)
s_day_network_sample <- column_to_rownames(s_day_network_sample, var = "Ticker")
s_day_network_sample[is.na(s_day_network_sample)] <- 0

# custom palette
my_palette <- colorRampPalette(c("red", "purple", "blue"))(n = 299)
# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(-1,-0.3,length=100),  # for red
               seq(-0.3, 0.3,length=100),              # for yellow
               seq(0.3, 1,length=100))              # for green

dist_s_sample <- dist(s_day_network_sample, method="euclidean")
dist_s_sample<- as.matrix(dist_s_sample)
a <- heatmap.2(dist_s_sample,
          density.info="none",  
          trace="none",         
          margins =c(12,9),    
          col=my_palette,
          dendrogram='both',     
          Rowv=T,
          Colv="Rowv",
          symm= T, revC = identical("Colv", "Rowv")) 

# heatmap + dendogram for a crash day -------------------------------------

load("data/daily_cosinus_2018.rda")
crash_day <- daily_res[["2018-12-24"]]

write.csv(crash_day, file = "data/crash_day.csv", row.names = F, quote = F)

crash_day_sample <- spread(crash_day[,-1], Ticker2, cos_vecs)
crash_day_sample <- column_to_rownames(crash_day_sample, var = "Ticker")
crash_day_sample[is.na(crash_day_sample)] <- 0

# custom palette
my_palette <- colorRampPalette(c("red", "purple", "blue"))(n = 299)
# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(-1,-0.3,length=100),  # for red
               seq(-0.3, 0.3,length=100),              # for yellow
               seq(0.3, 1,length=100))              # for green

dist_crash_day <- dist(crash_day_sample, method="euclidean")
dist_crash_day<- as.matrix(dist_crash_day)
heatmap.2(dist_crash_day,
          density.info="none",  
          trace="none",         
          margins =c(12,9),    
          col=my_palette,
          dendrogram='both',     
          Rowv=T,
          Colv="Rowv",
          symm= T, revC = identical("Colv", "Rowv")) 
